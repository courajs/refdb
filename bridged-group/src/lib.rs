#![allow(unused_mut, dead_code, unused_variables, unused_imports)]

extern crate proc_macro;

use std::collections::HashMap;

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

fn ty_to_tokens(ty: &Ty, locals: &HashMap<&Ident, usize>) -> impl ToTokens {
    match ty {
        Ty::Ingroup(ident) => {
            let p = locals.get(ident).unwrap();
            quote! { rf0::types::RADTItem::CycleRef(#p) }
        },
        Ty::Other(ast_ty) => {
            quote! {
                {
                    let (_,typeref) = <#ast_ty as rf0::bridge::Bridged>::radt();
                    rf0::types::RADTItem::ExternalType(typeref)
                }
            }
        },
    }
}

fn bridged_group_impl(mut file: File) -> impl ToTokens {
    let uniq = pop_uniqueness(&mut file.attrs).expect("should provide uniqueness");

    let mut name_finder = ItemNamesCollector::new();
    name_finder.visit_file(&file);
    let names = name_finder.names;

    let mut name_to_cycle_refs = HashMap::new();
    for (i, name) in names.iter().enumerate() {
        name_to_cycle_refs.insert(name, i);
    }

    let mut defs_finder = DefinitionsCollector::new(names.clone());
    defs_finder.visit_file(&file);
    // dbg!(defs_finder.defs);

    let radt_items: Vec<_> = defs_finder.defs.iter().map(|def| {
        match def {
            Def::Struct(StructDef{fields, ..}) => {
                match fields {
                    ItemFields::Unit => quote! { rf0::types::RADTItem::Product(Vec::new()) },
                    ItemFields::TupleLike(types) => {
                        let fields = types.iter().map(|ty| ty_to_tokens(ty, &name_to_cycle_refs));
                        quote! {
                            rf0::types::RADTItem::Product(vec![
                                #(
                                    #fields),*
                            ])
                        }
                    },
                    ItemFields::StructLike(named_fields) => {
                        let fields = named_fields.iter().map(|(_,ty)| ty_to_tokens(ty, &name_to_cycle_refs));
                        quote! {
                            rf0::types::RADTItem::Product(vec![
                                #(
                                    #fields
                                ),*
                            ])
                        }
                    },
                }
            },
            Def::Enum(EnumDef{variants,..}) => {
                let variants = variants.iter().map(|(_,fields)| {
                    match fields {
                        ItemFields::Unit => quote! { rf0::types::RADTItem::Sum(Vec::new()) },
                        ItemFields::TupleLike(types) => {
                            let fields = types.iter().map(|ty| ty_to_tokens(ty, &name_to_cycle_refs));
                            quote! {
                                rf0::types::RADTItem::Product(vec![
                                    #(
                                        #fields
                                    ),*
                                ])
                            }
                        },
                        ItemFields::StructLike(named_fields) => {
                            let fields = named_fields.iter().map(|(_,ty)| ty_to_tokens(ty, &name_to_cycle_refs));
                            quote! {
                                rf0::types::RADTItem::Product(vec![
                                    #(
                                        #fields
                                    ),*
                                ])
                            }
                        },
                    }
                });
                quote! {
                    rf0::types::RADTItem::Sum(vec![
                        #(
                            #variants
                        ),*
                    ])
                }
            },
        }
    }).collect();

    /*
    let items: Vec<_> = field_finder.structs.iter().map(|(ty, fields)| {
        let f = fields.into_iter().map(|ty| {
            match ty {
                Ty::Ingroup(ident) => {
                    let p = names.iter().position(|n| n == ident).unwrap();
                    quote! { rf0::types::RADTItem::CycleRef(#p) }
                },
                Ty::Other(ty) => {
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
    */

    let impls = names.into_iter().enumerate().map(|(index, ident)| {
        let uniq = uniq.clone();
        let items = radt_items.clone();
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
        #file
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

#[derive(Debug, Clone, PartialEq)]
enum Def {
    Struct(StructDef),
    Enum(EnumDef),
}
#[derive(Debug, Clone, PartialEq)]
struct StructDef {
    name: Ident,
    fields: ItemFields,
}
#[derive(Debug, Clone, PartialEq)]
struct EnumDef {
    name: Ident,
    variants: Vec<(Ident, ItemFields)>,
}
#[derive(Debug, Clone, PartialEq)]
enum ItemFields {
    Unit,
    TupleLike(Vec<Ty>),
    StructLike(Vec<(Ident, Ty)>)
}
#[derive(Debug, Clone, PartialEq)]
enum Ty {
    // Box(Box<Ty>),
    // Vector(Box<Ty>),
    // Tuple(Vec<Ty>),
    Ingroup(Ident),
    Other(Type),
}



struct DefinitionsCollector {
    in_names: Vec<Ident>,
    defs: Vec<Def>,
}
impl DefinitionsCollector {
    fn new(names: Vec<Ident>) -> Self {
        Self { in_names: names, defs: Vec::new() }
    }
}
impl<'ast> Visit<'ast> for DefinitionsCollector {
    fn visit_item_struct(&mut self, s: &'ast ItemStruct) {
        self.defs.push(Def::Struct(StructDef {
            name: s.ident.clone(),
            fields: gather_fields(&s.fields, &self.in_names),
        }));
    }

    fn visit_item_enum(&mut self, e: &'ast ItemEnum) {
        self.defs.push(Def::Enum(EnumDef {
            name: e.ident.clone(),
            variants: e.variants.iter().map(|var| {
                (var.ident.clone(), gather_fields(&var.fields, &self.in_names))
            }).collect(),
        }));
    }
}

fn gather_fields(f: &Fields, in_group: &[Ident]) -> ItemFields {
    match f {
        Fields::Unit => ItemFields::Unit,
        Fields::Unnamed(ufs) => {
            ItemFields::TupleLike(ufs.unnamed.iter().map(|f| interpret_type(&f.ty, in_group)).collect())
        },
        Fields::Named(nfs) => {
            ItemFields::StructLike(nfs.named.iter().map(|f| (f.ident.clone().unwrap(), interpret_type(&f.ty, in_group))).collect())
        },
    }
}
fn interpret_type(ty: &Type, in_group: &[Ident]) -> Ty {
    if let Type::Path(TypePath {path, ..}) = ty {
        if let Some(n) = in_group.iter().find(|n| path.is_ident(*n)) {
            return Ty::Ingroup(n.clone());
        }
    }
    return Ty::Other(ty.clone());
}


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
