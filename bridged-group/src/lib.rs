extern crate proc_macro;

use proc_macro::TokenStream;
use quote::*;
use syn::*;
use syn::visit_mut::VisitMut;
use syn::visit::*;

#[proc_macro]
pub fn bridged_group(ts: TokenStream) -> TokenStream {
    let t: File = parse(ts).unwrap();

    let mut v = InGroupCollector(Vec::new());
    v.visit_file(&t);
    // println!("hey {:?}", v.0);
    let kinds: Vec<Ident> = v.0.into_iter().cloned().collect();
    let names: Vec<String> = kinds.iter().map(|i|i.to_string()).collect();
    let n = names[0].clone();

    let impls = kinds.iter().map(|i| {
        let s = i.to_string();
        quote! {
            impl #i {
                fn all() -> Vec<String> {
                    vec![#(String::from(#names)),*]
                }
            }
        }
    });

    TokenStream::from(quote! {
        #t
        #(#impls)*
    })
    // t.into_token_stream().into()
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

/*
enum Kind {
    Vector(Box<Kind>),
    Tuple(Vec<Kind>),
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
        self.0.push(f.ident.clone().unwrap());
        visit_field(self, f);
    }
}
*/

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
