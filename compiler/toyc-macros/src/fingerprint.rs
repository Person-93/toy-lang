use proc_macro2::TokenStream;
use quote::{format_ident, quote};
use syn::{
  parse::Result, spanned::Spanned, Data, DataEnum, DataStruct, DataUnion,
  DeriveInput, Error, Fields, Index, Variant,
};

pub(crate) fn fingerprint(input: DeriveInput) -> Result<TokenStream> {
  let DeriveInput {
    attrs: _,
    vis: _,
    ident,
    generics,
    data,
  } = input;

  let body = match data {
    Data::Struct(DataStruct { fields, .. }) => match fields {
      Fields::Named(fields) => {
        let fields = fields
          .named
          .into_iter()
          .map(|field| field.ident.unwrap())
          .map(|field| {
            quote! { self.#field.fingerprint(fingerprinter); }
          });
        quote! { #(#fields)* }
      }
      Fields::Unnamed(fields) => {
        let fields = fields
          .unnamed
          .into_iter()
          .enumerate()
          .map(|(idx, _)| Index::from(idx))
          .map(|idx| quote! { self.#idx.fingerprint(fingerprinter); });
        quote! { #(#fields)* }
      }
      Fields::Unit => {
        return Err(Error::new(
          fields.span(),
          "cannot fingerprint a unit struct",
        ))
      }
    },
    Data::Enum(DataEnum { variants, .. }) => {
      let match_arms = variants.into_iter().map(
        |Variant { ident, fields, .. }| match fields {
          Fields::Named(fields) => {
            let (lhs, rhs) = fields
              .named
              .into_iter()
              .map(|field| field.ident.unwrap())
              .map(|field| {
                (
                  quote! { #field },
                  quote! { #field.fingerprint(fingerprinter); },
                )
              })
              .unzip::<_, _, Vec<_>, Vec<_>>();
            quote! { Self::#ident{ #(#lhs),* } => { #(#rhs)* } }
          }
          Fields::Unnamed(fields) => {
            let (lhs, rhs) = fields
              .unnamed
              .into_iter()
              .enumerate()
              .map(|(idx, _)| format_ident!("_{idx}"))
              .map(|ident| {
                (
                  quote! { #ident },
                  quote! { #ident.fingerprint(fingerprinter); },
                )
              })
              .unzip::<_, _, Vec<_>, Vec<_>>();
            quote! { Self::#ident(#(#lhs),*) => { #(#rhs)* } }
          }
          Fields::Unit => quote! { Self::#ident => () },
        },
      );
      quote! {
        ::core::mem::discriminant(self).fingerprint(fingerprinter);
        match self {
          #(#match_arms),*
        }
      }
    }
    Data::Union(DataUnion { union_token, .. }) => {
      return Err(Error::new(
        union_token.span,
        "cannot fingerprint a union type",
      ))
    }
  };

  let (impl_generics, type_generics, where_clause) = generics.split_for_impl();

  Ok(quote! {
    const _: () = {
      extern crate toyc_data_structures;
      use ::toyc_data_structures::fingerprint::{CanBeFingerprinted, Fingerprinter};

      impl #impl_generics CanBeFingerprinted for #ident #type_generics #where_clause {
        fn fingerprint(&self, fingerprinter: &mut Fingerprinter) {
          #body
        }
      }
    };
  })
}
