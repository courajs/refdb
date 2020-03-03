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

    let mut name_finder = ItemNamesCollector::new();
    name_finder.visit_file(&t);
    let names = name_finder.names;

    let mut field_finder = AllFieldsCollector::new(names.clone());
    field_finder.visit_file(&t);

    let items: Vec<_> = field_finder.structs.iter().map(|(kind, fields)| {
        let f = fields.into_iter().map(|kind| {
            match kind {
                Kind::Ingroup(ident) => {
                    let p = names.iter().position(|n| n == ident).unwrap();
                    quote! { rf0::types::RADTItem::CycleRef(#p) }
                },
                Kind::Other(ty) => {
                    quote! {
                        {
                            let (_,typeref) = <#ty as rf0::bridge::Bridged>::radt();
                            rf0::types::RADTItem::ExternalType(typeref)
                        }
                    }
                }
            }
        });
        quote! {
            rf0::types::RADTItem::Product(vec![
                #(
                    #f
                ),*
            ])
        }
    }).collect();

    let impls = names.into_iter().enumerate().map(|(index, ident)| {
        let uniq = uniq.clone();
        let items = items.clone();
        quote! {
            impl rf0::bridge::Bridged for #ident {
                fn radt() -> (rf0::types::RADT, rf0::types::TypeRef) {
                    let r = rf0::types::RADT {
                        uniqueness: #uniq,
                        items: vec![
                            #(#items),*
                        ]
                    };
                    let t = r.item_ref(#index);
                    (r, t)
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

    let t = quote! {
        #t
        #(#impls)*
    };

    if false {
        println!("");
        println!("");
        println!("");
        dbg!(t.to_token_stream().to_string());
    }

    t
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

struct ItemNamesCollector{
    pub names: Vec<Ident>,
}
impl ItemNamesCollector {
    fn new() -> Self {
        Self { names: Vec::new() }
    }
}
impl<'ast> Visit<'ast> for ItemNamesCollector {
    fn visit_item_struct(&mut self, f: &'ast ItemStruct) {
        self.names.push(f.ident.clone())
    }
    fn visit_item_enum(&mut self, e: &'ast ItemEnum) {
        self.names.push(e.ident.clone())
    }
}

enum Kind {
    // Box(Box<Kind>),
    // Vector(Box<Kind>),
    // Tuple(Vec<Kind>),
    Ingroup(Ident),
    Other(Type),
}

struct AllFieldsCollector {
    in_names: Vec<Ident>,
    structs: Vec<(Ident, Vec<Kind>)>,
}
impl AllFieldsCollector {
    fn new(names: Vec<Ident>) -> Self {
        Self { in_names: names, structs: Vec::new() }
    }
}
impl<'ast> Visit<'ast> for AllFieldsCollector {
    fn visit_item_struct(&mut self, s: &'ast ItemStruct) {
        let mut v = FieldCollector::new();
        v.visit_item_struct(s);
        let kinds = v.types.into_iter().map(|ty| {
            if let Type::Path(TypePath { path, ..}) = &ty {
                if let Some(n) = self.in_names.iter().find(|n| path.is_ident(*n)) {
                    return Kind::Ingroup(n.clone());
                }
            }
            return Kind::Other(ty);
        }).collect();
        self.structs.push((s.ident.clone(), kinds));
    }
}

struct FieldCollector {
    types: Vec<Type>,
}
impl FieldCollector {
    fn new() -> FieldCollector {
        FieldCollector{ types: Vec::new()}
    }
}
impl<'ast> Visit<'ast> for FieldCollector {
    fn visit_field(&mut self, f: &'ast Field) {
        self.types.push(f.ty.clone());
        // visit_field(self, f);
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
