use proc_macro::TokenStream;
use quote::quote;
use syn;

#[proc_macro_derive(ToRange)]
pub fn to_range_derive(input: TokenStream) -> TokenStream {
    let ast = syn::parse(input).expect("Cannot parse struct");
    impl_to_range_derive(&ast)
}

fn impl_to_range_derive(ast: &syn::DeriveInput) -> TokenStream {
    let name = &ast.ident;
    let gen = match &ast.data {
        syn::Data::Struct(_) => {
            quote! {
                    impl ToRange for #name {
                        fn to_range(&self) -> std::ops::Range<usize> {
                            self.info.tokens.to_range()
                        }
                    }

            }
        }
        syn::Data::Enum(e) => {
            let arm: Vec<_> = e
                .variants
                .iter()
                .map(|variant| {
                    let var_name = &variant.ident;
                    use syn::Fields::*;
                    let field = match &variant.fields {
                        Unnamed(fields) => {
                            if fields.unnamed.len() > 1 {
                                panic!("#[derive(ToRange)] is not defined for tuple enums with more than one field")
                            }
                            quote! {
                                (info)
                            }
                        }
                        Named(_) => {
                            quote! {
                                {
                                    info,
                                    ..
                                }
                            }
                        }
                        _ => panic!("#[derive(ToRange) is not defined for this enum type]"),
                    };
                    quote! {
                        Self::#var_name #field => info.to_range()
                    }
                })
                .collect();
            quote! {
                impl ToRange for #name {
                    fn to_range(&self) -> std::ops::Range<usize> {
                        match self {
                            #(#arm),*
                        }
                    }
                }
            }
        }
        _ => panic!("#[derive(ToRange) is not defined]"),
    };
    gen.into()
}
