use super::{
  super::{is_rust_keyword, Specs},
  collapsed::{Ast, Choice, Group, GroupKind, Node, NodeKind},
  Ident, Modifier,
};
use heck::ToPascalCase;
use proc_macro2::TokenStream;
use quote::{format_ident, quote, ToTokens};

impl Ast<'_> {
  pub fn print(&self, _specs: &Specs<'_>) -> TokenStream {
    let nodes = self.iter().filter_map(|Node { ident, kind }| match kind {
      NodeKind::Node(_)
      | NodeKind::StaticToken(_)
      | NodeKind::DynamicToken(_)
      | NodeKind::Choice(Choice::Option { .. }) => None,
      NodeKind::Group(Group { members, kind }) => match kind {
        GroupKind::Zero | GroupKind::One(_) => None,
        GroupKind::Many(indices) => {
          let members = indices
            .iter()
            .copied()
            .map(|idx| &members[idx])
            .filter_map(|node| {
              self.print_as_type(&node.kind, Some(node.ident)).map(|ty| {
                let ident = node.ident;
                quote! { pub #ident: #ty }
              })
            });

          let ident = ident.as_type();
          Some(quote! { pub struct #ident { #(#members),* } })
        }
      },
      NodeKind::Choice(Choice::Regular(choices)) => {
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
            NodeKind::Group(Group { members, kind }) => match kind {
              GroupKind::Zero => quote! {},
              GroupKind::One(idx) => {
                let node = &members[*idx];
                self.print_as_type(&node.kind, Some(node.ident)).unwrap()
              }
              GroupKind::Many(_) => quote! { (#ty) },
            },
            NodeKind::Choice(Choice::Regular(_)) => quote! { (#ty) },
            NodeKind::Choice(Choice::Option {
              primary,
              secondary: _,
            }) => {
              let ty = self.print_as_type(&primary.kind, Some(primary.ident));
              quote! { (Option<#ty>) }
            }
            NodeKind::Delimited(inner, _) => self.print_as_type(inner, None).unwrap_or_default(),
            NodeKind::Modified(inner, modifier) => {
              let ty = self.print_modified_type(&**inner, None, *modifier);
              quote! { (#ty) }
            }
          };
          quote! { #ty #body }
        });

        let ident = format_ident!("{}", ident.to_pascal_case());
        Some(quote! { pub enum #ident { #(#variants),* } })
      }
      NodeKind::Delimited(inner, _delimiter) => match &**inner {
        NodeKind::Node(_) | NodeKind::StaticToken(_) | NodeKind::DynamicToken(_) => None,
        NodeKind::Group(Group { members, kind }) => match kind {
          GroupKind::Zero | GroupKind::One(_) => None,
          GroupKind::Many(indices) => {
            let members = indices
              .iter()
              .copied()
              .map(|idx| &members[idx])
              .filter_map(|node| {
                self.print_as_type(&node.kind, Some(node.ident)).map(|ty| {
                  let ident = node.ident;
                  quote! { #ident: #ty }
                })
              });

            let ident = ident.as_type();
            Some(quote! { pub struct #ident { #(#members),* } })
          }
        },
        NodeKind::Choice(_) | NodeKind::Delimited(_, _) | NodeKind::Modified(_, _) => None,
      },
      NodeKind::Modified(..) => None,
    });

    quote! {
      use super::super::tokens;
      #(#nodes)*
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
      NodeKind::Group(Group { members, kind }) => match kind {
        GroupKind::Zero => None,
        GroupKind::One(idx) => {
          let node = &members[*idx];
          self.print_as_type(&node.kind, Some(node.ident))
        }
        GroupKind::Many(_) => hint.map(|ident| ident.as_type().to_token_stream()),
      },
      NodeKind::Choice(Choice::Regular(_)) => hint.map(|ident| ident.as_type().to_token_stream()),
      NodeKind::Choice(Choice::Option {
        primary,
        secondary: _,
      }) => {
        let ty = self.print_as_type(&primary.kind, Some(primary.ident));
        Some(quote! { Option<#ty> })
      }
      NodeKind::Delimited(inner, _) => self.print_as_type(inner, hint),
      NodeKind::Modified(inner, modifier) => {
        Some(self.print_modified_type(&**inner, None, *modifier))
      }
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
          NodeKind::StaticToken(_) => {
            let ident = proc_macro2::Ident::new("usize", proc_macro2::Span::call_site());
            quote! { #ident }
          }
          NodeKind::DynamicToken(ident) => ident.as_type().to_token_stream(),
          NodeKind::Group(Group { members, kind }) => match kind {
            GroupKind::Zero | GroupKind::Many(_) => unreachable!(),
            GroupKind::One(idx) => {
              let node = &members[*idx];
              self
                .print_as_type(&node.kind, Some(node.ident))
                .unwrap_or_else(|| todo!())
            }
          },
          NodeKind::Choice(Choice::Regular(_)) => match hint {
            Some(ident) => ident.as_type().to_token_stream(),
            None => todo!(),
          },
          NodeKind::Choice(Choice::Option {
            primary,
            secondary: _,
          }) => match self.print_as_type(&primary.kind, Some(primary.ident)) {
            Some(ty) => quote! { Option<#ty> },
            None => todo!(),
          },
          NodeKind::Delimited(..) => match hint {
            Some(ident) => ident.as_type().to_token_stream(),
            None => todo!(),
          },
          NodeKind::Modified(_, _) => unreachable!(),
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
