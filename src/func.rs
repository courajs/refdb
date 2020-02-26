// type that represents a function signature
// type that represents a function's dependencies, argument types, and return type
// call a function given a dependency map and arguments

use std::collections::HashMap;
use std::iter::FromIterator;
use std::ops::Deref;
use std::sync::Arc;

use lazy_static::lazy_static;
use rhai::{
    Engine, Scope,
    RegisterFn,
    Any as AnyClone
};
use rhai::engine::{
    FnSpec,
    FnIntExt,
};

use crate::core::*;
use crate::types::*;

#[derive(Debug, Clone, PartialEq)]
struct FullType {
    radt: RADT,
    item: usize,
}

#[derive(Debug, Clone, PartialEq)]
enum Value {
    Blob(Blob),
    Typing(Typing),
    Value(RADTValue),
    Function(FunctionReference),
    // BuiltinFunction(usize),
    // DefinedFunction(FunctionDefinition),
}
impl Value {
    fn into_any(self) -> Box<dyn AnyClone> {
        match self {
            Value::Blob(inner) => Box::new(inner),
            Value::Typing(inner) => Box::new(inner),
            Value::Value(inner) => Box::new(inner),
            Value::Function(inner) => Box::new(inner),
        }
    }

    fn conforms(&self, k: &Kind) -> bool {
        match (k, self) {
            (Kind::Blob, Value::Blob(_)) => true,
            (Kind::Typing, Value::Typing(_)) => true,
            (Kind::Value(typ), Value::Value(val)) => {
                todo!()
            },
            (Kind::Function(sig), Value::Function(reference)) => {
                // So, really, we'd want to be comparing the expected signature
                // to the real signature. But this is only a reference, so we
                // don't have the acutal signature here.
                // Instead, maybe we can typecheck this externally?
                // Or, we can check builtins with a global lookup table, and
                // require passing the *actual* function for defined funcs.
                true
            },
            _ => false,
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
enum Kind {
    Blob,
    Typing,
    Value(FullType),
    Function(FunctionSignature),
}
use std::any::TypeId;
impl Kind {
    fn to_value_type_id(&self) -> TypeId {
        match self {
            Kind::Blob => TypeId::of::<Blob>(),
            Kind::Typing => TypeId::of::<Typing>(),
            Kind::Value(_) => TypeId::of::<RADTValue>(),
            Kind::Function(_) => TypeId::of::<FunctionReference>(),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
struct FunctionSignature {
    inputs: Vec<Kind>,
    out: Box<Kind>,
}

enum FunctionValue {
    Builtin(BuiltinFunction),
    Defined(FunctionDefinition),
}

struct BuiltinFunction {
    signature: FunctionSignature,
    f: Box<dyn Fn(Vec<Value>) -> (Value, Vec<Value>)>,
}

impl BuiltinFunction {
    fn call(&self, args: Vec<Value>) -> (Value, Vec<Value>) {
        if self.signature.inputs.len() != args.len() {
            panic!("builtin called with wrong number of inputs")
        }
        for (arg, typ) in args.iter().zip(self.signature.inputs.iter()) {
            if !arg.conforms(typ) {
                panic!("builtin call parameter mismatch")
            }
        }
        let (val, aux) = (self.f)(args);
        if !val.conforms(&self.signature.out) {
            panic!("unexpected return type from builtin function")
        }
        (val, aux)
    }
}

fn generate_builtin_map() -> Vec<(FnSpec, Arc<FnIntExt>)> {
    println!("generate");
        let mut eng = Engine::new();

        let mut idx = -1;
        let mut next = || {
            idx += 1;
            println!("{}", idx);
            format!("{}", idx)
        };

        // 0: New empty blob
        eng.register_fn(&next(), || Blob {bytes: Vec::new()});
        // 1: Blob length (usually len(b))
        eng.register_fn(&next(), |b: &mut Blob| b.bytes.len() as i64);
        // 2: Get a single byte from a blob by index
        eng.register_fn(&next(), |b: &mut Blob, idx: i64| b.bytes[idx as usize]);
        // 3: Set a single byte in a blob by index
        eng.register_fn(&next(), |b: &mut Blob, idx: i64, val: i64| b.bytes[idx as usize] = val as u8);
        // 4: Add a byte to the end of a blob
        eng.register_fn(&next(), |b: &mut Blob, val: i64| b.bytes.push(val as u8));

        let mut result: Vec<(FnSpec, Arc<FnIntExt>)> = eng.fns.into_iter()
            .filter(|(spec,_)| {
                usize::from_str_radix(&spec.ident, 10).is_ok()
            }).collect();
        result.sort_by_key(|(spec,_)| {
            usize::from_str_radix(&spec.ident, 10).unwrap()
        });

        result
}

trait Callable {
    fn call(&self, args: Vec<Value>) -> (Value, Vec<Value>);
}

struct PreparedFunction {
    signature: FunctionSignature,
    engine: Engine,
    body: String,
}
impl PreparedFunction {
    fn call(&mut self, args: Vec<Value>) -> (Value, Vec<Value>) {
        if self.signature.inputs.len() != args.len() {
            panic!("builtin called with wrong number of inputs")
        }
        for (arg, typ) in args.iter().zip(self.signature.inputs.iter()) {
            if !arg.conforms(typ) {
                panic!("builtin call parameter mismatch")
            }
        }
        let mut scope = Scope::new();
        for (i, val) in args.into_iter().enumerate() {
            scope.push((format!("arg{}", i), val.into_any()));
        }

        match self.signature.out.deref() {
            Kind::Blob => {
                let blob = self.engine.eval_with_scope::<Blob>(&mut scope, &self.body).expect("ahhh");
                (Value::Blob(blob), Vec::new())
            },
            Kind::Typing => {
                let t = self.engine.eval_with_scope::<Typing>(&mut scope, &self.body).expect("ahhh");
                (Value::Typing(t), Vec::new())
            },
            Kind::Value(typ) => {
                todo!()
            },
            Kind::Function(sig) => {
                todo!()
            },
        }
    }
    fn register_as(self, ident: String, eng: &mut Engine) {
        let arg_types = self.signature.inputs.iter().map(Kind::to_value_type_id).collect();
        let inner = self.engine;
        eng.register_fn_raw(ident.clone(), Some(arg_types), Box::new(move |args| {
            inner.call_fn_raw(ident.clone(), args)
        }));
    }
}

#[derive(Debug, Clone, PartialEq)]
struct FunctionDefinition {
    signature: FunctionSignature,
    dependencies: HashMap<String, FunctionReference>,
    body: String,
}

impl FunctionDefinition {
    fn builtin_deps(&self) -> Vec<usize> {
        self.dependencies.iter().filter_map(|(_,fref)| match fref {
            FunctionReference::Builtin(i) => Some(*i),
            _ => None,
        }).collect()
    }

    fn db_deps(&self) -> Vec<Hash> {
        self.dependencies.iter().filter_map(|(_,fref)| match fref {
            FunctionReference::Definition(h) => Some(*h),
            _ => None,
        }).collect()
    }

    fn prepare(self, builtins: &[(FnSpec, Arc<FnIntExt>)]) -> PreparedFunction {
        let mut engine = Engine::new();
        for (name, fref) in self.dependencies {
            match fref {
                FunctionReference::Builtin(i) => {
                    engine.fns.insert(FnSpec {
                        ident: name.clone(),
                        args: builtins[i].0.args.clone(),
                    }, builtins[i].1.clone());
                },
                FunctionReference::Definition(_) => todo!(),
            }
        }
        PreparedFunction {
            signature: self.signature,
            engine,
            body: self.body,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
enum FunctionReference {
    Builtin(usize),
    Definition(Hash),
}

#[cfg(test)]
mod tests {
    use super::*;

    fn add(v: Vec<Value>) -> (Value, Vec<Value>) {
        let a = sure!(&v[0], Value::Blob(Blob { bytes }) => bytes[0]);
        let b = sure!(&v[1], Value::Blob(Blob { bytes }) => bytes[0]);
        (Value::Blob(Blob { bytes: vec![a+b] }), Vec::new())
    }

    #[test]
    fn test_call_builtin() {
        let builtin = BuiltinFunction {
            signature: FunctionSignature {
                inputs: vec![Kind::Blob, Kind::Blob],
                out: Box::new(Kind::Blob),
            },
            f: Box::new(add),
        };

        let a = Value::Blob(Blob {bytes: vec![2]});
        let b = Value::Blob(Blob {bytes: vec![3]});

        assert_eq!(builtin.call(vec![a, b]), (Value::Blob(Blob{bytes:vec![5]}), Vec::new()));
    }

    #[test]
    fn test_call_def_with_builtin_deps() {
        let def = FunctionDefinition {
            signature: FunctionSignature {
                inputs: vec![Kind::Blob],
                out: Box::new(Kind::Blob),
            },
            dependencies: HashMap::from_iter([
                 (String::from("blob"), FunctionReference::Builtin(0)),
                 (String::from("len"), FunctionReference::Builtin(1)),
                 (String::from("push"), FunctionReference::Builtin(4)),
            ].iter().cloned()),
            body: String::from("let b = blob(); b.push(arg0.len()); b"),
        };
        let builtins = generate_builtin_map();
        let mut prepped = def.prepare(&builtins);
        let (v,_) = prepped.call(vec![Value::Blob(Blob{bytes: vec![12,12,12]})]);
        assert_eq!(v, Value::Blob(Blob{bytes:vec![3]}));
    }
}


