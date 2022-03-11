mod fingerprint;

use proc_macro::TokenStream;
use syn::{parse_macro_input, DeriveInput};

#[proc_macro_derive(CanBeFingerprinted)]
pub fn derive_fingerprint(input: TokenStream) -> TokenStream {
  let input = parse_macro_input!(input as DeriveInput);
  fingerprint::fingerprint(input)
    .unwrap_or_else(|err| err.into_compile_error())
    .into()
}
