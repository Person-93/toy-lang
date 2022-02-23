use super::{
  super::{is_rust_keyword, Delimiter, Specs},
  collapsed::{Ast, Choice, ChoiceKind, Group, GroupKind, Node, NodeKind},
  Ident, Modifier,
};
use crate::collections::NamedItem;
use heck::ToPascalCase;
use proc_macro2::TokenStream;
use quote::{format_ident, quote, ToTokens};

impl<'a> Ast<'a> {
  pub fn print(&self, specs: &Specs<'_>) -> TokenStream {
    let nodes = self.iter().filter_map(|node| match &node.kind {
      NodeKind::Node(_)
      | NodeKind::StaticToken(_)
      | NodeKind::DynamicToken(_)
      | NodeKind::Choice(Choice {
        kind: ChoiceKind::Option { .. },
        inline: _,
      }) => None,
      NodeKind::Group(Group {
        members,
        kind,
        inline: _,
      }) => match kind {
        GroupKind::Zero | GroupKind::One(_) => None,
        GroupKind::Many(indices) => {
          let members = indices
            .iter()
            .copied()
            .map(|idx| &members[idx])
            .filter_map(|node| {
              self.print_as_type(&node.kind, Some(node.ident)).map(|ty| {
                let ident = node.tag.unwrap_or(node.ident);
                quote! { pub #ident: #ty }
              })
            });

          let ident = node.ident.as_type();
          Some(quote! { pub struct #ident { #(#members),* } })
        }
      },
      NodeKind::Choice(Choice {
        kind: ChoiceKind::Regular(choices),
        inline: _,
      }) => {
        let variants = choices.iter().map(|choice| {
          let ty = choice.ident.as_type();
          let body = match &choice.kind {
            NodeKind::Node(child) => {
              let child = self.get(child.0).unwrap();
              match self.print_as_type(&child.kind, Some(child.ident)) {
                Some(ty) => quote! { (#ty) },
                None => quote! {},
              }
            }
            NodeKind::StaticToken(_) => quote! {},
            NodeKind::DynamicToken(ident) => {
              let ty = ident.as_type();
              quote! { (tokens::#ty) }
            }
            NodeKind::Group(Group {
              members,
              kind,
              inline: _,
            }) => match kind {
              GroupKind::Zero => quote! {},
              GroupKind::One(idx) => {
                let node = &members[*idx];
                self.print_as_type(&node.kind, Some(node.ident)).unwrap()
              }
              GroupKind::Many(_) => quote! { (#ty) },
            },
            NodeKind::Choice(Choice {
              kind: ChoiceKind::Regular(_),
              inline: _,
            }) => quote! { (#ty) },
            NodeKind::Choice(Choice {
              kind:
                ChoiceKind::Option {
                  primary,
                  secondary: _,
                },
              inline: _,
            }) => {
              let ty = self.print_as_type(&primary.kind, Some(primary.ident));
              quote! { (Option<#ty>) }
            }
            NodeKind::Delimited(inner, _) => self.print_as_type(inner, None).unwrap_or_default(),
            NodeKind::Modified(inner, modifier) => {
              let ty = self.print_modified_type(&**inner, None, *modifier);
              quote! { (#ty) }
            }
            NodeKind::Todo => quote! { () },
          };
          quote! { #ty #body }
        });

        let ident = format_ident!("{}", node.ident.to_pascal_case());
        Some(quote! { pub enum #ident { #(#variants),* } })
      }
      NodeKind::Delimited(inner, _delimiter) => match &**inner {
        NodeKind::Node(_) | NodeKind::StaticToken(_) | NodeKind::DynamicToken(_) => None,
        NodeKind::Group(Group {
          members,
          kind,
          inline: _,
        }) => match kind {
          GroupKind::Zero | GroupKind::One(_) => None,
          GroupKind::Many(indices) => {
            let members = indices
              .iter()
              .copied()
              .map(|idx| &members[idx])
              .filter_map(|node| {
                self.print_as_type(&node.kind, Some(node.ident)).map(|ty| {
                  let ident = node.tag.unwrap_or(node.ident);
                  quote! { #ident: #ty }
                })
              });

            let ident = node.ident.as_type();
            Some(quote! { pub struct #ident { #(#members),* } })
          }
        },
        NodeKind::Choice(_)
        | NodeKind::Delimited(_, _)
        | NodeKind::Modified(_, _)
        | NodeKind::Todo => None,
      },
      NodeKind::Modified(..) => None,
      NodeKind::Todo => None,
    });

    let parsers = self.nodes.iter().map(|node| self.print_parser(node, specs));

    let recursive_parsers = self.print_recursive_parsers(specs);

    quote! {
      #![allow(dead_code)]

      use super::super::tokens;
      #(#nodes)*

      pub mod parse {
        use super::{*, super::Error};
        use chumsky::prelude::*;
        use tokens::{parse::*, Token};

        #(#parsers)*
        #recursive_parsers
      }
    }
  }

  fn print_as_type(&self, kind: &NodeKind<'_>, hint: Option<Ident<'_>>) -> Option<TokenStream> {
    match kind {
      NodeKind::Node(child) => match self.get(child.0) {
        Some(child) => self.print_as_type(&child.kind, Some(child.ident)),
        None => {
          let message = format!("missing node `{child}`");
          Some(quote! { compile_error!(#message) })
        }
      },
      NodeKind::StaticToken(_) => None,
      NodeKind::DynamicToken(ty) => {
        let ty = ty.as_type();
        Some(quote! { tokens::#ty })
      }
      NodeKind::Group(Group {
        members,
        kind,
        inline: _,
      }) => match kind {
        GroupKind::Zero => None,
        GroupKind::One(idx) => {
          let node = &members[*idx];
          self.print_as_type(&node.kind, Some(node.ident))
        }
        GroupKind::Many(_) => hint.map(|ident| ident.as_type().to_token_stream()),
      },
      NodeKind::Choice(Choice {
        kind: ChoiceKind::Regular(_),
        inline: _,
      }) => hint.map(|ident| ident.as_type().to_token_stream()),
      NodeKind::Choice(Choice {
        kind: ChoiceKind::Option {
          primary,
          secondary: _,
        },
        inline: _,
      }) => {
        let ty = self.print_as_type(&primary.kind, Some(primary.ident));
        Some(quote! { Option<#ty> })
      }
      NodeKind::Delimited(inner, _) => self.print_as_type(inner, hint),
      NodeKind::Modified(inner, modifier) => {
        Some(self.print_modified_type(&**inner, None, *modifier))
      }
      NodeKind::Todo => Some(quote! { () }),
    }
  }

  fn print_modified_type(
    &self,
    kind: &NodeKind<'_>,
    hint: Option<Ident<'_>>,
    modifier: Modifier,
  ) -> TokenStream {
    match modifier {
      Modifier::Repeat | Modifier::Csv | Modifier::OnePlus | Modifier::CsvOnePlus => {
        let ty = match kind {
          NodeKind::Node(child) => {
            let child = self.get(child.0).unwrap();
            self
              .print_as_type(&child.kind, hint)
              .unwrap_or_else(|| child.ident.as_type().to_token_stream())
          }
          NodeKind::StaticToken(_) => quote! { usize },
          NodeKind::DynamicToken(ident) => ident.as_type().to_token_stream(),
          NodeKind::Group(Group {
            members,
            kind,
            inline: _,
          }) => match kind {
            GroupKind::Zero | GroupKind::Many(_) => unreachable!(),
            GroupKind::One(idx) => {
              let node = &members[*idx];
              self
                .print_as_type(&node.kind, Some(node.ident))
                .unwrap_or_else(|| todo!())
            }
          },
          NodeKind::Choice(Choice {
            kind: ChoiceKind::Regular(_),
            inline: _,
          }) => match hint {
            Some(ident) => ident.as_type().to_token_stream(),
            None => todo!(),
          },
          NodeKind::Choice(Choice {
            kind:
              ChoiceKind::Option {
                primary,
                secondary: _,
              },
            inline: _,
          }) => match self.print_as_type(&primary.kind, Some(primary.ident)) {
            Some(ty) => quote! { Option<#ty> },
            None => todo!(),
          },
          NodeKind::Delimited(..) => match hint {
            Some(ident) => ident.as_type().to_token_stream(),
            None => todo!(),
          },
          NodeKind::Modified(_, _) => unreachable!(),
          NodeKind::Todo => quote! { () },
        };
        quote! { Vec<#ty> }
      }
      Modifier::Optional => match self.print_as_type(kind, None) {
        Some(ty) => quote! { Option<#ty> },
        None => quote! { bool },
      },
      Modifier::Boxed => match self.print_as_type(kind, hint) {
        Some(ty) => quote! { Box<#ty> },
        None => quote! { compile_error!("empty box") },
      },
    }
  }

  fn print_parser(&self, node: &Node<'a>, specs: &Specs<'a>) -> TokenStream {
    let ident = node.ident;
    let ty: TokenStream =
      match &node.kind {
        NodeKind::Node(_) => todo!(),
        NodeKind::StaticToken(_) => quote! { () },
        NodeKind::DynamicToken(ident) => ident.as_type().to_token_stream(),
        NodeKind::Group(Group {
          members: _,
          kind,
          inline: _,
        }) => match kind {
          GroupKind::Zero => quote! { () },
          GroupKind::One(_) | GroupKind::Many(_) => {
            self.print_as_type(&node.kind, Some(ident)).unwrap()
          }
        },
        NodeKind::Choice(Choice {
          kind: ChoiceKind::Regular(_),
          inline: _,
        }) => ident.as_type().to_token_stream(),
        NodeKind::Choice(Choice {
          kind: ChoiceKind::Option {
            primary,
            secondary: _,
          },
          inline: _,
        }) => {
          let primary = primary.ident.as_type();
          quote! { Option<#primary> }
        }
        NodeKind::Delimited(inner, _) => self
          .print_as_type(inner, Some(node.ident))
          .unwrap_or_else(|| {
            let message = format!("failed to get type for {ident}");
            quote! { compile_error!(#message) }
          }),
        NodeKind::Modified(inner, modifier) => self.print_modified_type(inner, None, *modifier),
        NodeKind::Todo => quote! { () },
      };
    let body = if self.is_node_cyclic(node) {
      let idx = self.index_of(node.ident.0).unwrap();
      let idx = self.cyclic[0..=idx].iter().filter(|b| **b).count() - 1;
      let idx = proc_macro2::Literal::usize_unsuffixed(idx);
      quote! { RECURSIVE.with(|parsers| parsers.borrow().#idx.clone()) }
    } else {
      self.print_parser_body(&node.kind, Some(node.ident), specs)
    };

    quote! {
      pub fn #ident() -> impl Parser<Token, #ty, Error = Error> { #body }
    }
  }

  fn print_parser_body(
    &self,
    node_kind: &NodeKind<'_>,
    hint: Option<Ident<'_>>,
    specs: &Specs<'_>,
  ) -> TokenStream {
    match node_kind {
      NodeKind::Node(child) => quote! { #child() },
      NodeKind::StaticToken(ident) => quote! { #ident() },
      NodeKind::DynamicToken(ident) => {
        let ty = ident.as_type();
        quote! { select! { Token::#ty(item) => item } }
      }
      NodeKind::Group(Group {
        members,
        kind,
        inline,
      }) => match kind {
        GroupKind::Zero => {
          let mut members = members.iter();
          let first = self.print_sub_parser(members.next().unwrap(), specs, *inline);
          let members: Vec<_> = members
            .map(|node| {
              let parser = self.print_sub_parser(node, specs, *inline);
              quote! { .then(#parser) }
            })
            .collect();

          if members.is_empty() {
            first
          } else {
            quote! { #first #(#members)*.ignored() }
          }
        }
        GroupKind::One(idx) => {
          let binding =
            (1..members.len()).fold(ident_from_idx(0).to_token_stream(), |accum, idx| {
              let ident = ident_from_idx(idx);
              quote! { (#accum, #ident) }
            });

          let members = members.iter().enumerate().map(|(current_idx, node)| {
            let parser = self.print_sub_parser(node, specs, *inline);
            if current_idx == 0 {
              parser
            } else {
              quote! { .then(#parser) }
            }
          });

          let idx = ident_from_idx(*idx);

          quote! { #(#members)*.map(|#binding| #idx ) }
        }
        GroupKind::Many(_) => {
          let ty = match self.print_as_type(node_kind, hint) {
            Some(ty) => ty,
            None => {
              let message = format!("unable to compute type for group: {node_kind}");
              return quote! { compile_error!(#message) };
            }
          };

          let member_init = members
            .iter()
            .map(|node| {
              self
                .print_as_type(&node.kind, Some(node.ident))
                .map(|_| node.tag.unwrap_or(node.ident))
            })
            .enumerate()
            .filter_map(|(idx, ident)| ident.map(|ident| (idx, ident)))
            .map(|(idx, ident)| {
              let idx = ident_from_idx(idx);
              quote! { #ident: #idx }
            });

          let binding =
            (1..members.len()).fold(ident_from_idx(0).to_token_stream(), |accum, idx| {
              let ident = ident_from_idx(idx);
              quote! { (#accum, #ident) }
            });

          let members = members.iter().enumerate().map(|(current_idx, node)| {
            let parser = self.print_sub_parser(node, specs, *inline);
            if current_idx == 0 {
              parser
            } else {
              quote! { .then(#parser) }
            }
          });

          quote! {
            #(#members)*.map(|#binding| #ty { #(#member_init),* })
          }
        }
      },
      NodeKind::Choice(Choice {
        kind: ChoiceKind::Regular(choices),
        inline: _,
      }) => {
        let ty = self.print_as_type(node_kind, hint).unwrap();
        let choices = choices.iter().enumerate().map(|(idx, choice)| {
          let ident = choice.ident;
          let variant = ident.as_type();

          let parser = self.print_sub_parser(choice, specs, false);

          let func = make_func(self, &choice.kind, ty.clone(), variant.to_token_stream());

          let choice = quote! { #parser.map(#func) };
          return if idx == 0 {
            choice
          } else {
            quote! { .or(#choice) }
          };

          fn make_func(
            ast: &Ast<'_>,
            kind: &NodeKind<'_>,
            ty: TokenStream,
            variant: TokenStream,
          ) -> TokenStream {
            match kind {
              NodeKind::Node(child) => make_func(ast, &ast.get(child.0).unwrap().kind, ty, variant),
              NodeKind::StaticToken(_) => quote! { |_| #ty::#variant },
              NodeKind::DynamicToken(_) => quote! { #ty::#variant },
              NodeKind::Group(Group {
                members: _,
                kind,
                inline: _,
              }) => match kind {
                GroupKind::Zero => quote! { |_| #ty::#variant },
                GroupKind::One(_) => quote! { #ty::#variant },
                GroupKind::Many(_) => quote! { #ty::#variant },
              },
              NodeKind::Choice(_) => quote! { #ty::#variant },
              NodeKind::Delimited(_, _) => quote! { #ty::#variant },
              NodeKind::Modified(_, _) => quote! { #ty::#variant },
              NodeKind::Todo => quote! { #ty::#variant },
            }
          }
        });
        quote! { #(#choices)* }
      }
      NodeKind::Choice(Choice {
        kind: ChoiceKind::Option { primary, secondary },
        inline: _,
      }) => {
        let primary = match &primary.kind {
          NodeKind::Group(Group { inline, .. }) => self.print_sub_parser(primary, specs, *inline),
          _ => {
            let ident = primary.ident;
            quote! { #ident() }
          }
        };
        quote! { #primary.map(Some).or(#secondary().map(|_| None)) }
      }
      NodeKind::Delimited(inner, delimiter) => self.delimit_parser(
        self.print_parser_body(inner, hint, specs),
        *delimiter,
        specs,
      ),
      NodeKind::Modified(inner, modifier) => {
        modify_parser(inner, self.print_parser_body(inner, None, specs), *modifier)
      }
      NodeKind::Todo => print_todo(),
    }
  }

  fn print_sub_parser(&self, node: &Node<'_>, specs: &Specs<'_>, inline: bool) -> TokenStream {
    if inline {
      self.print_parser_body(&node.kind, None, specs)
    } else {
      let ident = node.ident;
      let parser = quote! { #ident() };
      match &node.kind {
        NodeKind::Node(child) => quote! { #child() },
        NodeKind::StaticToken(_) | NodeKind::DynamicToken(_) => parser,
        NodeKind::Group(Group { inline: true, .. }) => {
          self.print_parser_body(&node.kind, None, specs)
        }
        NodeKind::Group(Group { inline: false, .. }) => parser,
        NodeKind::Choice(Choice { inline: true, .. }) => {
          self.print_parser_body(&node.kind, None, specs)
        }
        NodeKind::Choice(Choice { inline: false, .. }) => parser,
        NodeKind::Delimited(inner, delimiter) => {
          let parser = match &**inner {
            NodeKind::Node(child) => quote! { #child() },
            NodeKind::Modified(inner, modifier) => modify_parser(inner, parser, *modifier),
            _ => parser,
          };
          self.delimit_parser(parser, *delimiter, specs)
        }
        NodeKind::Modified(inner, modifier) => modify_parser(inner, parser, *modifier),
        NodeKind::Todo => print_todo(),
      }
    }
  }

  fn is_node_cyclic(&self, node: &Node<'_>) -> bool {
    match node.kind {
      NodeKind::Node(_) | NodeKind::Group(_) | NodeKind::Choice(_) => {
        let idx = self.nodes.index_of(node.name()).unwrap();
        self.cyclic[idx]
      }
      NodeKind::StaticToken(_)
      | NodeKind::DynamicToken(_)
      | NodeKind::Delimited(_, _)
      | NodeKind::Modified(_, _)
      | NodeKind::Todo => false,
    }
  }

  fn delimit_parser(
    &self,
    parser: TokenStream,
    delimiter: Ident<'_>,
    specs: &Specs<'_>,
  ) -> TokenStream {
    match specs.delimiters.get(delimiter.0) {
      Some(Delimiter {
        name: _,
        open,
        close,
      }) => {
        let open = format_ident!("{}", open);
        let close = format_ident!("{}", close);
        quote! { #parser.delimited_by(#open(), #close()) }
      }
      None => {
        let message = format!("missing delimiter: {delimiter}");
        quote! { compile_error!(#message) }
      }
    }
  }

  fn print_recursive_parsers(&self, specs: &Specs<'_>) -> TokenStream {
    let nodes: Vec<_> = self
      .nodes
      .iter()
      .zip(self.cyclic.iter())
      .filter_map(|(node, recursive)| recursive.then(|| node))
      .collect();

    let idents = nodes.iter().map(|node| node.ident.to_token_stream());
    let idents = quote! { (#(#idents),*) };

    let parser_types = nodes.iter().map(|node| {
      let ty = self.print_as_type(&node.kind, Some(node.ident)).unwrap();
      quote! { Recursive<'static, Token, #ty, Error> }
    });

    let decls = nodes.iter().map(|node| {
      let ident = node.ident;
      quote! {
        #[allow(unused_assignments)]
        let mut #ident = recursive(|_| todo());
      }
    });

    let body = nodes.iter().fold(TokenStream::new(), |accum, node| {
      let ident = node.ident;
      let parser = self.print_recursive_sub_parser(&node.kind, Some(node.ident), specs);
      quote! {
        #ident = recursive(|#[allow(unused_variables)] #ident| {
          #accum
          #parser
        });
      }
    });

    quote! {
      thread_local! {
        static RECURSIVE: std::cell::RefCell<(#(#parser_types),*)> = std::cell::RefCell::new({
          #(#decls)*
          #body
          #idents
        });
      }
    }
  }

  fn print_recursive_sub_parser(
    &self,
    node_kind: &NodeKind<'_>,
    hint: Option<Ident<'_>>,
    specs: &Specs<'_>,
  ) -> TokenStream {
    match node_kind {
      NodeKind::Node(child) => {
        let child = self.get(child.0).unwrap();
        if self.is_node_cyclic(child) {
          let ident = child.ident;
          quote! { #ident.clone() }
        } else {
          self.print_recursive_sub_parser(&child.kind, hint, specs)
        }
      }
      NodeKind::StaticToken(ident) | NodeKind::DynamicToken(ident) => quote! { #ident() },
      NodeKind::Group(Group {
        members,
        kind,
        inline: _,
      }) => match kind {
        GroupKind::Zero => self.print_parser_body(node_kind, None, specs),
        GroupKind::One(idx) => {
          let binding =
            (1..members.len()).fold(ident_from_idx(0).to_token_stream(), |accum, idx| {
              let ident = ident_from_idx(idx);
              quote! { (#accum, #ident) }
            });

          let members = members.iter().enumerate().map(|(current_idx, node)| {
            if self.is_node_cyclic(node) {
              let ident = node.ident;
              quote! { #ident.clone() }
            } else {
              let parser = self.print_recursive_sub_parser(&node.kind, hint, specs);
              if current_idx == 0 {
                parser
              } else {
                quote! { .then(#parser) }
              }
            }
          });

          let idx = ident_from_idx(*idx);

          quote! { #(#members)*.map(|#binding| #idx ) }
        }
        GroupKind::Many(_) => {
          let ty = match self.print_as_type(node_kind, hint) {
            Some(ty) => ty,
            None => {
              let message = format!("unable to compute type for group: {node_kind}");
              return quote! { compile_error!(#message) };
            }
          };

          let member_init = members
            .iter()
            .map(|node| {
              self
                .print_as_type(&node.kind, Some(node.ident))
                .map(|_| node.ident)
            })
            .enumerate()
            .filter_map(|(idx, ident)| ident.map(|ident| (idx, ident)))
            .map(|(idx, ident)| {
              let idx = ident_from_idx(idx);
              quote! { #ident: #idx }
            });

          let binding =
            (1..members.len()).fold(ident_from_idx(0).to_token_stream(), |accum, idx| {
              let ident = ident_from_idx(idx);
              quote! { (#accum, #ident) }
            });

          let members = members.iter().enumerate().map(|(current_idx, node)| {
            if self.is_node_cyclic(node) {
              node.ident.to_token_stream()
            } else {
              let parser = self.print_recursive_sub_parser(&node.kind, hint, specs);
              if current_idx == 0 {
                parser
              } else {
                quote! { .then(#parser) }
              }
            }
          });

          quote! {
            #(#members)*.map(|#binding| #ty { #(#member_init),* })
          }
        }
      },
      NodeKind::Choice(Choice {
        kind: ChoiceKind::Regular(choices),
        inline: _,
      }) => {
        let ty = self.print_as_type(node_kind, hint).unwrap();
        let choices = choices.iter().enumerate().map(|(idx, node)| {
          let ident = node.ident;
          let variant = ident.as_type();

          let parser = if self.is_node_cyclic(node) {
            quote! { #ident.clone() }
          } else {
            self.print_recursive_sub_parser(&node.kind, hint, specs)
          };

          let func = make_func(self, &node.kind, ty.clone(), variant.to_token_stream());

          let choice = quote! { #parser.map(#func) };
          return if idx == 0 {
            choice
          } else {
            quote! { .or(#choice) }
          };

          fn make_func(
            ast: &Ast<'_>,
            kind: &NodeKind<'_>,
            ty: TokenStream,
            variant: TokenStream,
          ) -> TokenStream {
            match kind {
              NodeKind::Node(child) => make_func(ast, &ast.get(child.0).unwrap().kind, ty, variant),
              NodeKind::StaticToken(_) => quote! { |_| #ty::#variant },
              NodeKind::DynamicToken(_) => quote! { #ty::#variant },
              NodeKind::Group(Group {
                members: _,
                kind,
                inline: _,
              }) => match kind {
                GroupKind::Zero => quote! { |_| #ty::#variant },
                GroupKind::One(_) => quote! { #ty::#variant },
                GroupKind::Many(_) => quote! { #ty::#variant },
              },
              NodeKind::Choice(_) => quote! { #ty::#variant },
              NodeKind::Delimited(_, _) => quote! { #ty::#variant },
              NodeKind::Modified(_, _) => quote! { #ty::#variant },
              NodeKind::Todo => quote! { #ty::#variant },
            }
          }
        });
        quote! { #(#choices)* }
      }
      NodeKind::Choice(Choice {
        kind: ChoiceKind::Option { primary, secondary },
        inline: _,
      }) => {
        let primary = if self.is_node_cyclic(primary) {
          let ident = primary.ident;
          quote! { #ident.clone() }
        } else {
          match &primary.kind {
            NodeKind::Group(_) => self.print_recursive_sub_parser(&primary.kind, hint, specs),
            _ => {
              let ident = primary.ident;
              quote! { #ident() }
            }
          }
        };
        quote! { #primary.map(Some).or(#secondary().map(|_| None)) }
      }
      NodeKind::Delimited(inner, delimiter) => self.delimit_parser(
        self.print_recursive_sub_parser(inner, hint, specs),
        *delimiter,
        specs,
      ),
      NodeKind::Modified(inner, modifier) => modify_parser(
        &NodeKind::Todo, // dummy arg to prevent it from recursing
        self.print_recursive_sub_parser(inner, hint, specs),
        *modifier,
      ),
      NodeKind::Todo => print_todo(),
    }
  }
}

impl ToTokens for Ident<'_> {
  fn to_tokens(&self, tokens: &mut TokenStream) {
    let ident = if is_rust_keyword(self.0) {
      format_ident!("{}_", self.0)
    } else {
      format_ident!("{}", self.0)
    };
    tokens.extend(quote! { #ident })
  }
}

impl Ident<'_> {
  fn as_type(&self) -> proc_macro2::Ident {
    format_ident!("{}", self.0.to_pascal_case())
  }
}

fn modify_parser(inner: &NodeKind<'_>, parser: TokenStream, modifier: Modifier) -> TokenStream {
  let parser = if let NodeKind::Modified(inner, modifier) = inner {
    modify_parser(inner, parser, *modifier)
  } else {
    parser
  };
  match modifier {
    Modifier::Repeat => quote! { #parser.repeated() },
    Modifier::Csv => quote! { #parser.separated_by(comma()) },
    Modifier::OnePlus => quote! { #parser.repeated().at_least(1) },
    Modifier::CsvOnePlus => {
      quote! { #parser.separated_by(comma()).at_least(1) }
    }
    Modifier::Optional => quote! { #parser.or_not() },
    Modifier::Boxed => quote! { #parser.map(Box::new) },
  }
}

fn print_todo() -> TokenStream {
  quote! { todo::<Token, (), Error>() }
}

fn ident_from_idx(idx: usize) -> proc_macro2::Ident {
  format_ident!("_{idx}")
}
