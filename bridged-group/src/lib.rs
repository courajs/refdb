#![allow(unused_mut, dead_code, unused_variables, unused_imports)]

extern crate proc_macro;

use proc_macro::TokenStream;
use quote::*;
use syn::*;
use syn::visit_mut::VisitMut;
use syn::visit::*;

#[proc_macro]
pub fn bridged_group(ts: TokenStream) -> TokenStream {
    let t: File = parse(ts).expect("b");
    bridged_group_impl(t).into_token_stream().into()
}

fn bridged_group_impl(mut t: File) -> impl ToTokens {
    let uniq = pop_uniqueness(&mut t.attrs).expect("should provide uniqueness");

    let mut v = InGroupCollector(Vec::new());
    v.visit_file(&t);

    let kinds = v.0;
    let names: Vec<String> = kinds.iter().map(|i|i.to_string()).collect();

    let impls = kinds.iter().map(|i| {
        let s = i.to_string();
        quote! {
            impl rf0::bridge::Bridged for #i {
                fn radt() -> (rf0::types::RADT, rf0::types::TypeRef) {
                    todo!()
                }
                fn to_value(&self) -> (rf0::types::TypedValue, Vec<rf0::storage::Item>) {
                    todo!()
                }
                fn from_value(v: &rf0::types::TypedValue, deps: &std::collections::HashMap<rf0::core::Hash, rf0::storage::Item>) -> Result<Self, rf0::error::MonsterError> {
                    todo!()
                }
            }
        }
    });

    quote! {
        #t
        #(#impls)*
    }
}

fn pop_uniqueness(attrs: &mut Vec<Attribute>) -> Option<Expr> {
    for i in 0..attrs.len() {
        if attrs[i].path.is_ident("uniq") {
            let a = attrs.remove(i);
            return Some(a.parse_args::<Expr>().unwrap());
        }
    }
    None
}

struct InGroupCollector<'a>(Vec<&'a Ident>);
impl<'ast> Visit<'ast> for InGroupCollector<'ast> {
    fn visit_item_struct(&mut self, f: &'ast ItemStruct) {
        self.0.push(&f.ident)
    }
    fn visit_item_enum(&mut self, e: &'ast ItemEnum) {
        self.0.push(&e.ident)
    }
}

enum Kind {
    // Box(Box<Kind>),
    Vector(Box<Kind>),
    // Tuple(Vec<Kind>),
    Ingroup(Ident),
    Other(Type),
}

struct FieldCollector(Vec<Ident>);
impl FieldCollector {
    fn new() -> FieldCollector {
        FieldCollector(Vec::new())
    }
}
impl<'ast> Visit<'ast> for FieldCollector {
    fn visit_field(&mut self, f: &'ast Field) {
        self.0.push(f.ident.clone().expect("a"));
        visit_field(self, f);
    }
}

/*
impl VisitMut for Visitor {
    fn visit_field_mut(&mut self, i: &mut Field) {
        i.attrs.retain(|attr| {
            println!("{:?}", attr);
            !attr.path.is_ident("thing")
        });
    }
}
*/


#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn uniqueness_from_attribute() {
        // https://docs.rs/syn/1.0.16/syn/struct.Attribute.html#parsing-from-tokens-to-attribute
        let input = quote! { #![uniq(*b"1234")] };
        let attr = parse2::<File>(input).unwrap().attrs[0].clone();

        dbg!(attr.parse_args::<Expr>());

        // panic!();
    }


}
