#![allow(unused_mut, dead_code, unused_variables, unused_imports)]

extern crate proc_macro;

use std::collections::HashMap;
use std::ops::Deref;

use proc_macro::TokenStream;
use quote::*;
use syn::*;
use syn::visit_mut::VisitMut;
use syn::visit::*;
use proc_macro2::TokenStream as TwokenStream;

use pretty_sure::sure;
use indexmap::IndexSet;


#[proc_macro]
pub fn bridged_group(ts: TokenStream) -> TokenStream {
    let t: File = parse(ts).expect("b");
    bridged_group_impl(t).into_token_stream().into()
}

fn ty_to_radt_tokens(ty: &Ty, locals: &HashMap<Ty, usize>) -> TwokenStream {
    match ty {
        Ty::Box(inner) => ty_to_radt_tokens(inner, locals),
        Ty::Vec(_) => {
            let p = locals.get(ty).unwrap();
            quote! { rf0::types::RADTItem::CycleRef(#p) }
        },
        Ty::Map(_) => {
            let p = locals.get(ty).unwrap();
            quote! { rf0::types::RADTItem::CycleRef(#p) }
        }
        Ty::Hash(e) => {
            quote! { rf0::types::RADTItem::ExternalType(#e) }
        }
        Ty::Ingroup(ident) => {
            let p = locals.get(ty).unwrap();
            quote! { rf0::types::RADTItem::CycleRef(#p) }
        },
        Ty::Other(ast_ty) => {
            quote! { rf0::types::RADTItem::ExternalType(<#ast_ty as rf0::bridge::TypeRefed>::type_ref()) }
        },
    }
}

fn find_names(f: &File) -> Vec<Ident> {
    let mut finder = ItemNamesCollector::new();
    finder.visit_file(f);
    finder.names
}
fn find_defs(f: &File, names: &[Ident]) -> Vec<Def> {
    let mut finder = DefinitionsCollector::new(names.to_vec());
    finder.visit_file(f);
    finder.defs
}
fn strip_hash_macros(f: &mut File) {
    StripHashMacros.visit_file_mut(f);
}

fn bridged_group_impl(mut file: File) -> impl ToTokens {
    let uniq = pop_uniqueness(&mut file.attrs).expect("should provide uniqueness");
    let names = find_names(&file);
    let defs = find_defs(&file, &names);
    strip_hash_macros(&mut file);

    let num_main_types = defs.len();

    // types there are a Vec of, which we serialize as a cons list
    let mut vectorized_types = IndexSet::new();
    // types there are a map of, which we serialize as a cons list of key/value pairs
    let mut mapped_types = IndexSet::new();
    for def in defs.iter() {
        for ty in def.types() {
            match ty {
                Ty::Vec(inner) => {
                    vectorized_types.insert(*inner);
                },
                Ty::Map(inner) => {
                    mapped_types.insert((inner.0, inner.1));
                },
                _ => ()
            }
        }
    }

    // mapping from Ty to index for types represented directly as a radt item - vecs, maps, and
    // each item in the bridged group
    let mut locals = HashMap::new();
    locals.extend(defs.iter().enumerate().map(|(i, def)| {
        (Ty::Ingroup(def.name()), i)
    }));
    locals.extend(vectorized_types.iter().enumerate().map(|(i,ty)| {
        // 2i because there's a cons and a list entry for each type, plus one for the shared nil
        // And, they're inserted after all the base defined types
        (Ty::Vec(Box::new(ty.clone())), num_main_types + 2*i + 1)
    }));
    locals.extend(mapped_types.iter().enumerate().map(|(i, (from,to))| {
        let base = num_main_types + 1 + 2*vectorized_types.len();
        (Ty::Map(Box::new((from.clone(), to.clone()))), base + 3*i) 
    }));
    
    let ref_for_ty = |ty: &Ty| -> TwokenStream {
        ty_to_radt_tokens(ty, &locals)
    };


    // core radt items for each item in the group
    let mut radt_items = Vec::new();
    radt_items.extend(defs.iter().map(|def| {
        match def {
            Def::Struct(StructDef{fields, ..}) => {
                let fs = fields.types().into_iter().map(|ty|ref_for_ty(&ty));
                quote! {
                    rf0::types::RADTItem::Product(vec![
                        #(#fs),*
                    ])
                }
            },
            Def::Enum(EnumDef{variants,..}) => {
                let variants = variants.iter().map(|(_,fields)| {
                    let fs = fields.types().into_iter().map(|ty|ref_for_ty(&ty));
                    quote! {
                        rf0::types::RADTItem::Product(vec![
                            #(#fs),*
                        ])
                    }
                });
                quote! {
                    rf0::types::RADTItem::Sum(vec![
                        #(#variants),*
                    ])
                }
            },
        }
    }));

    // insert nil
    if vectorized_types.len() > 0 || mapped_types.len() > 0 {
        radt_items.push(quote!{rf0::types::RADTItem::Product(Vec::new())});
    }

    // radt items for each vec type
    let nil_index = num_main_types;
    if vectorized_types.len() > 0 {
        radt_items.extend(vectorized_types.iter().enumerate().map(|(i, ty)| {
            let list_index = num_main_types + 2*i + 1;
            let cons_index = list_index + 1;
            let val = ref_for_ty(ty);
            quote! {
                rf0::types::RADTItem::Sum(vec![
                    rf0::types::RADTItem::CycleRef(#nil_index),
                    rf0::types::RADTItem::CycleRef(#cons_index),
                ]),
                rf0::types::RADTItem::Product(vec![
                    #val,
                    rf0::types::RADTItem::CycleRef(#list_index),
                ])
            }
        }));
    }

    // radt items for each map type
    if mapped_types.len() > 0 {
        radt_items.extend(mapped_types.iter().enumerate().map(|(i, (from, to))| {
            // each main item, shared nil, cons+list for each vec
            let base = num_main_types + 1 + 2*vectorized_types.len();
            let map_index = base + 3*i;
            let cons_index = map_index + 1;
            let entry_index = map_index + 2;
            let key = ref_for_ty(from);
            let val = ref_for_ty(to);
            quote! {
                rf0::types::RADTItem::Sum(vec![
                    rf0::types::RADTItem::CycleRef(#nil_index),
                    rf0::types::RADTItem::CycleRef(#cons_index),
                ]),
                rf0::types::RADTItem::Product(vec![
                    rf0::types::RADTItem::CycleRef(#entry_index),
                    rf0::types::RADTItem::CycleRef(#map_index),
                ]),
                rf0::types::RADTItem::Product(vec![#key, #val])
            }
        }));
    }

    // body for group_ids method. Passed down the call chain during serialization so
    // types know whether to serialize inline or as a Hash ref to a dependency
    let type_ids = {
        // let group_items = names.iter().map(|name| quote!{std::any::TypeId::of::<#name>()});
        quote! {
            let mut h = std::collections::HashSet::new();
            #(h.insert(std::any::TypeId::of::<#names>());)*
            h
        }
    };

    // actual implementations of Bridged, Serialize, etc for each group item
    let impls = defs.into_iter().enumerate().map(|(index, def)| {
        let name = def.name();
        let uniq = uniq.clone();
        let items = radt_items.clone();
        let from_value = match &def {
            Def::Struct(StructDef{name,fields}) => {
                let fields_len = fields.len();
                let fields_def = match fields {
                    ItemFields::Unit => quote!{},
                    ItemFields::TupleLike(fs) => {
                        let things = fs.iter().enumerate().map(|(i,ty)| {
                            let type_stream = ty.type_token_stream();
                            quote!{ <#type_stream as DeserializeFromRADTValue>::deserialize(&fields[#i], deps)? }
                        });
                        quote!{ (#(#things),*) }
                    },
                    ItemFields::StructLike(fs) => {
                        let things = fs.iter().enumerate().map(|(i, (field_name, ty))| {
                            let type_stream = ty.type_token_stream();
                            quote!{ #field_name: <#type_stream as DeserializeFromRADTValue>::deserialize(&fields[#i], deps)? }
                        });
                        quote!{ {#(#things),*}}
                    },
                };
                quote! {
                    impl rf0::bridge::DeserializeFromRADTValue for #name {
                        fn deserialize(val: &rf0::types::RADTValue, deps: & impl rf0::bridge::DependencySource) -> Result<Self, rf0::error::MonsterError> {
                            use rf0::error::MonsterError;
                            use rf0::bridge::Bridged;
                            use rf0::bridge::DeserializeFromRADTValue;
                            use rf0::storage::Item;
                            use rf0::types::RADTValue;
                            use rf0::types::TypedValue;
                            match val {
                                RADTValue::Hash(h) => {
                                    let (_,tr) = <Self as Bridged>::radt();
                                    match &deps.get(h) {
                                        Some(rf0::storage::Item::Value(TypedValue{kind:tr, value})) => {
                                            <Self as DeserializeFromRADTValue>::deserialize(value, deps)
                                        },
                                        Some(_) => Err(MonsterError::Todo("mismatched types in specific deser")),
                                        None => Err(MonsterError::Todo("missing dep in deser"))
                                    }
                                },
                                RADTValue::Product(fields) if fields.len() == #fields_len => {
                                    Ok(#name #fields_def)
                                },
                                _ => Err(MonsterError::Todo("bad value for deser"))
                            }
                        }
                    }
                }
            },
            Def::Enum(EnumDef{name,variants}) => {
                let variant_branches = variants.iter().enumerate().map(|(i,(var_name,fields))|{
                    let i = i as u8;
                    let fields_len = fields.len();
                    let fields_def = match fields {
                        ItemFields::Unit => quote!{},
                        ItemFields::TupleLike(fs) => {
                            let things = fs.iter().enumerate().map(|(i,ty)| {
                                let type_stream = ty.type_token_stream();
                                quote!{ <#type_stream as DeserializeFromRADTValue>::deserialize(&fields[#i], deps)? }
                            });
                            quote!{ (#(#things),*) }
                        },
                        ItemFields::StructLike(fs) => {
                            let things = fs.iter().enumerate().map(|(i, (field_name, ty))| {
                                let type_stream = ty.type_token_stream();
                                quote!{ #field_name: <#type_stream as DeserializeFromRADTValue>::deserialize(&fields[#i], deps)? }
                            });
                            quote!{ {#(#things),*}}
                        },
                    };
                    quote!{
                        RADTValue::Sum {kind: #i, value} => {
                            match value.deref() {
                                RADTValue::Product(fields) if fields.len() == #fields_len => {
                                    Ok(#name::#var_name #fields_def)
                                },
                                _ => Err(MonsterError::Todo("deser enum num fields mismatch"))
                            }
                        }
                    }
                });
                quote! {
                    impl rf0::bridge::DeserializeFromRADTValue for #name {
                        fn deserialize(val: &rf0::types::RADTValue, deps: & impl rf0::bridge::DependencySource) -> Result<Self, rf0::error::MonsterError> {
                            use std::ops::Deref;
                            use rf0::error::MonsterError;
                            use rf0::bridge::Bridged;
                            use rf0::bridge::DeserializeFromRADTValue;
                            use rf0::storage::Item;
                            use rf0::types::RADTValue;
                            use rf0::types::TypedValue;
                            match val {
                                RADTValue::Hash(h) => {
                                    let (_,tr) = <Self as Bridged>::radt();
                                    match &deps.get(h) {
                                        Some(rf0::storage::Item::Value(TypedValue{kind:tr, value})) => {
                                            <Self as DeserializeFromRADTValue>::deserialize(value, deps)
                                        },
                                        Some(_) => Err(MonsterError::Todo("mismatched types in specific deser")),
                                        None => Err(MonsterError::Todo("missing dep in deser"))
                                    }
                                },
                                #(#variant_branches),*
                                _ => Err(MonsterError::Todo("bad value for deser"))
                            }
                        }
                    }
                }
            }
        };
        let to_value = match def {
            Def::Struct(StructDef{fields,..}) => {
                let field_values = match fields {
                    ItemFields::Unit => Vec::new(),
                    ItemFields::TupleLike(fields) => {
                        (0..fields.len()).map(|i| {
                            let index = syn::Index::from(i);
                            quote! { self.#index.serialize(&mut deps, &group) }
                        }).collect()
                    },
                    ItemFields::StructLike(fields) => {
                        fields.iter().map(|(name,_ty)| {
                            quote! { self.#name.serialize(&mut deps, &group) }
                        }).collect()
                    },
                };
                quote! {
                    let mut deps = Vec::new();
                    let group = Self::group_ids();
                    let val = rf0::types::RADTValue::Product(vec![
                        #(#field_values),*
                    ]);
                    let (_,typeref) = Self::radt();
                    (TypedValue { kind: typeref, value: val }, deps)
                }
            },
            Def::Enum(EnumDef{name, variants}) => {
                let variants = variants.iter().enumerate().map(|(i, (var_name, fields))| {
                    let i = i as u8;
                    match fields {
                        ItemFields::Unit => {
                            quote!{
                                #name::#var_name => rf0::types::RADTValue::Sum {
                                    kind: #i,
                                    value: Box::new(rf0::types::RADTValue::Product(Vec::new())),
                                }
                            }
                        },
                        ItemFields::TupleLike(fs) => {
                            let idents = (0..fs.len()).map(|i|format_ident!("f{}", i));
                            let serialized_fields = (0..fs.len()).map(|i| {
                                let ident = format_ident!("f{}", i);
                                quote!{ #ident.serialize(&mut deps, &group) }
                            });
                            quote!{
                                #name::#var_name(#(#idents),*) => rf0::types::RADTValue::Sum {
                                    kind: #i,
                                    value: Box::new(rf0::types::RADTValue::Product(vec![
                                        #(#serialized_fields),*
                                    ])),
                                }
                            }
                        },
                        ItemFields::StructLike(fs) => {
                            assert!(!fs.iter().any(|(id,_)|id == "deps" || id == "group"), "can't serialize a 'deps' or 'group' field");
                            let bindings = fs.iter().map(|(id,_)|id);
                            let fields = fs.iter().map(|(id,_)| quote!{ #id.serialize(&mut deps, &group) });
                            quote! {
                                #name::#var_name{#(#bindings),*} => rf0::types::RADTValue::Sum {
                                    kind: #i,
                                    value: Box::new(rf0::types::RADTValue::Product(vec![
                                        #(#fields),*
                                    ])),
                                }
                            }
                        },
                    }
                });
                quote!{
                    let mut deps = Vec::new();
                    let group = Self::group_ids();
                    let val = match self {
                        #(#variants),*
                    };
                    let (_,typeref) = Self::radt();
                    (TypedValue { kind: typeref, value: val}, deps)
                }
            },
        };
        quote! {
            #from_value
            impl rf0::bridge::Bridged for #name {
                fn group_ids() -> std::collections::HashSet<std::any::TypeId> {
                    #type_ids
                }
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
                    use rf0::bridge::SerializeToRADTValue;
                    #to_value
                }
                fn from_value(v: &rf0::types::TypedValue, deps: & impl rf0::bridge::DependencySource) -> Result<Self, rf0::error::MonsterError> {
                    use rf0::types::TypedValue;
                    use rf0::bridge::Bridged;
                    use rf0::bridge::DeserializeFromRADTValue;
                    use rf0::error::MonsterError;
                    let (_,tr) = <Self as Bridged>::radt();
                    match v {
                        TypedValue {kind:tr, value} => {
                            <Self as DeserializeFromRADTValue>::deserialize(value, deps)
                        },
                        _ => Err(MonsterError::Todo("ahh wrong type to from_value"))
                    }
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

#[derive(Hash, Eq, Debug, Clone, PartialEq)]
enum Def {
    Struct(StructDef),
    Enum(EnumDef),
}
impl Def {
    fn name(&self) -> Ident {
        match self {
            Def::Struct(StructDef{name,..}) => name.clone(),
            Def::Enum(EnumDef{name,..}) => name.clone(),
        }
    }

    fn types(&self) -> Vec<Ty> {
        match self {
            Def::Struct(StructDef{fields,..}) => {
                return fields.types();
            },
            Def::Enum(EnumDef{variants,..}) => {
                let mut all = Vec::new();
                for (i, fields) in variants.iter() {
                    all.extend(fields.types().into_iter());
                }
                return all;
            }
        }
    }
}
#[derive(Hash, Eq, Debug, Clone, PartialEq)]
struct StructDef {
    name: Ident,
    fields: ItemFields,
}
#[derive(Hash, Eq, Debug, Clone, PartialEq)]
struct EnumDef {
    name: Ident,
    variants: Vec<(Ident, ItemFields)>,
}
#[derive(Hash, Eq, Debug, Clone, PartialEq)]
enum ItemFields {
    Unit,
    TupleLike(Vec<Ty>),
    StructLike(Vec<(Ident, Ty)>)
}
#[derive(Hash, Eq, Debug, Clone, PartialEq)]
enum Ty {
    Box(Box<Ty>),
    Vec(Box<Ty>),
    Map(Box<(Ty, Ty)>),
    Hash(Expr),
    Ingroup(Ident),
    Other(Type),
}
impl Ty {
    fn type_token_stream(&self) -> TwokenStream {
        match self {
            Ty::Box(inner) => {
                let inner_stream = inner.type_token_stream();
                quote! { Box<#inner_stream> }
            },
            Ty::Vec(inner) => {
                let inner_stream = inner.type_token_stream();
                quote! { Vec<#inner_stream> }
            },
            Ty::Map(inner) => {
                let (k,v) = inner.deref();
                let key = k.type_token_stream();
                let val = v.type_token_stream();
                quote! { std::collections::BTreeMap<#key, #val> }
            },
            Ty::Hash(_) => {
                quote! { rf0::core::Hash }
            },
            Ty::Ingroup(id) => {
                quote!{#id}
            },
            Ty::Other(inner) => {
                quote!{#inner}
            },
        }
    }
}

impl ItemFields {
    fn types(&self) -> Vec<Ty> {
        match self {
            ItemFields::Unit => Vec::new(),
            ItemFields::TupleLike(fs) => fs.clone(),
            ItemFields::StructLike(fs) => {
                fs.iter().map(|(_,ty)|ty).cloned().collect()
            }
        }
    }
    fn len(&self) -> usize {
        match self {
            ItemFields::Unit => 0,
            ItemFields::TupleLike(fs) => fs.len(),
            ItemFields::StructLike(fs) => fs.len(),
        }
    }
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
        match to_simple_path(path).deref() {
            "Box" => return Ty::Box(Box::new(get_main_generic_args(path, in_group)[0].clone())),
            "Vec" => return Ty::Vec(Box::new(get_main_generic_args(path, in_group)[0].clone())),
            "Hash" | "core::Hash" | "rf0::core::Hash" | "crate::core::Hash" => panic!("Use the Hash!(*TypeRef expression*) macro form to annotate Hash types"),
            "BTreeMap" | "collections::BTreeMap" | "std::collections::BTreeMap" => {
                let args = get_main_generic_args(path, in_group);
                return Ty::Map(Box::new((args[0].clone(), args[1].clone())));
            }
            _ => ()
        }
        if let Some(n) = in_group.iter().find(|n| path.is_ident(*n)) {
            return Ty::Ingroup(n.clone());
        }
    }
    if let Type::Macro(TypeMacro{mac:Macro{path, tokens,..}}) = ty {
        if to_simple_path(path) == "Hash" {
            return Ty::Hash(parse2::<Expr>(tokens.clone()).unwrap());
        }
    }
    return Ty::Other(ty.clone());
}

struct StripHashMacros;
impl VisitMut for StripHashMacros {
    fn visit_field_mut(&mut self, f: &mut Field) {
        if let Type::Macro(TypeMacro{mac: Macro{path,..}}) = &f.ty {
            if to_simple_path(path) == "Hash" {
                let hash = quote! { rf0::core::Hash };
                let p = parse2::<Type>(hash).unwrap();
                f.ty = p;
            }
        } else {
            visit_mut::visit_field_mut(self, f);
        }
    }
    fn visit_generic_argument_mut(&mut self, g: &mut GenericArgument) {
        if let GenericArgument::Type(Type::Macro(TypeMacro{mac: Macro{path,..}})) = g {
            if to_simple_path(path) == "Hash" {
                let hash = quote! { rf0::core::Hash };
                let p = parse2::<Type>(hash).unwrap();
                *g = GenericArgument::Type(p);
            }
        } else {
            visit_mut::visit_generic_argument_mut(self, g);
        }
    }
}

fn get_main_generic_args(path: &Path, in_group: &[Ident]) -> Vec<Ty> {
    let args = sure!(&path.segments.last().unwrap().arguments, PathArguments::AngleBracketed(AngleBracketedGenericArguments{args,..}) => args);
    args.iter().map(|garg| {
        let t = sure!(garg, GenericArgument::Type(t) => t);
        interpret_type(t, in_group)
    }).collect()
}

fn to_simple_path(path: &Path) -> String {
    let mut s = String::new();
    let mut first = true;
    for seg in path.segments.iter() {
        if first {
            first = false;
        } else {
            s.push_str("::");
        }
        s.push_str(&seg.ident.to_string());
    }
    s
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
