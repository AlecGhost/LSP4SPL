use proc_macro2::TokenStream;
use quote::quote;
use syn::DataEnum;

#[proc_macro_derive(ToRange)]
pub fn to_range_derive(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let ast = syn::parse(input).expect("Cannot parse struct");
    let output = impl_to_range_derive(&ast);
    proc_macro::TokenStream::from(output)
}

fn impl_to_range_derive(ast: &syn::DeriveInput) -> TokenStream {
    let name = &ast.ident;
    let implementation = match &ast.data {
        syn::Data::Struct(_) => {
            quote! {
                self.info.to_range()
            }
        }
        syn::Data::Enum(e) => {
            let variants: Vec<_> = access_info_in_enum_variants(e, "ToRange");
            quote! {
                match self {
                    #(#variants info.to_range()),*
                }
            }
        }
        _ => panic!("#[derive(ToRange) is not defined]"),
    };
    let gen = quote! {
        impl ToRange for #name {
            fn to_range(&self) -> std::ops::Range<usize> {
                #implementation
            }
        }
    };
    gen
}

#[proc_macro_derive(ToTextRange)]
pub fn to_text_range_derive(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let ast = syn::parse(input).expect("Cannot parse struct");
    let output = impl_to_text_range_derive(&ast);
    proc_macro::TokenStream::from(output)
}

fn impl_to_text_range_derive(ast: &syn::DeriveInput) -> TokenStream {
    let name = &ast.ident;
    let implementation = match &ast.data {
        syn::Data::Struct(_) => {
            quote! {
                self.info.to_text_range(tokens)
            }
        }
        syn::Data::Enum(e) => {
            let variants: Vec<_> = access_info_in_enum_variants(e, "ToTextRange");
            quote! {
                match self {
                    #(#variants info.to_text_range(tokens)),*
                }
            }
        }
        _ => panic!("#[derive(ToTextRange) is not defined]"),
    };
    let gen = quote! {
        impl ToTextRange for #name {
            fn to_text_range(&self, tokens: &[Token]) -> std::ops::Range<usize> {
                #implementation
            }
        }
    };
    gen
}

fn access_info_in_enum_variants(data_enum: &DataEnum, derivation: &str) -> Vec<TokenStream> {
    data_enum
        .variants
        .iter()
        .map(|variant| {
            let var_name = &variant.ident;
            use syn::Fields::*;
            let field = match &variant.fields {
                Unnamed(fields) => {
                    if fields.unnamed.len() > 1 {
                        panic!(
                            "#[derive({})] is not defined for tuple enums with more than one field",
                            derivation
                        )
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
                _ => panic!(
                    "#[derive({}) is not defined for this enum type]",
                    derivation
                ),
            };
            quote! {
                Self::#var_name #field =>
            }
        })
        .collect()
}
