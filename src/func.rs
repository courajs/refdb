// type that represents a function signature
// type that represents a function's dependencies, argument types, and return type
// call a function given a dependency map and arguments

use std::collections::HashMap;
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
}

#[derive(Debug, Clone, PartialEq)]
enum Kind {
    Blob,
    Typing,
    Value(FullType),
    Function(FunctionSignature),
}
impl Value {
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

#[derive(Debug, Clone, PartialEq)]
struct FunctionDefinition {
    signature: FunctionSignature,
    dependencies: HashMap<String, FunctionReference>,
    body: String,
}

fn generate_builtin_map() -> Vec<(FnSpec, Arc<FnIntExt>)> {
        let mut eng = Engine::new();

        let mut idx = -1;
        let next = || {
            idx += 1;
            format!("{}", idx)
        };

        // Blob length (usually len(b))
        eng.register_fn(&next(), |b: &mut Blob| b.bytes.len());
        // Get a single byte from a blob by index
        eng.register_fn(&next(), |b: &mut Blob, idx: i64| b.bytes[idx as usize]);
        // Set a single byte in a blob by index
        eng.register_fn(&next(), |b: &mut Blob, idx: i64, val: i64| b.bytes[idx as usize] = val as u8);
        // Add a byte to the end of a blob
        eng.register_fn(&next(), |b: &mut Blob, val: i64| b.bytes.push(val as u8));

        let mut result: Vec<(FnSpec, Arc<FnIntExt>)> = eng.fns.into_iter().collect();
        result.sort_by_key(|(spec,_)| usize::from_str_radix(&spec.ident, 10));

        result
}

lazy_static! {
    pub static ref BUILTINS: Vec<(FnSpec, Arc<FnIntExt>)> = generate_builtin_map();
}

impl FunctionDefinition {
    fn call(&self, args: Vec<Value>, deps: &HashMap<FunctionReference, FunctionValue>) -> (Value, Vec<Value>) {
        if self.signature.inputs.len() != args.len() {
            panic!("builtin called with wrong number of inputs")
        }
        for (arg, typ) in args.iter().zip(self.signature.inputs.iter()) {
            if !arg.conforms(typ) {
                panic!("builtin call parameter mismatch")
            }
        }
        let mut engine = Engine::new();
        let mut scope = Scope::new();

        engine.register_get("len", |b: &mut Blob| b.bytes.len());
        engine.register_fn("get", |b: &mut Blob, idx: i64| b.bytes[idx as usize]);
        engine.register_fn("set", |b: &mut Blob, idx: i64, val: i64| b.bytes[idx as usize] = val as u8);
        engine.register_fn("push", |b: &mut Blob, val: i64| b.bytes.push(val as u8));

        // args.into_iter().map(Value::into_any).enumerate().map(|(i, val)| (format!("arg{}", i), val))

        for (i, val) in args.into_iter().enumerate() {
            scope.push((format!("arg{}", i), val.into_any()));
        }

        match self.signature.out.deref() {
            Kind::Blob => {
                let blob = engine.eval_with_scope::<Blob>(&mut scope, &self.body).expect("ahhh");
                (Value::Blob(blob), Vec::new())
            },
            Kind::Typing => {
                let t = engine.eval_with_scope::<Typing>(&mut scope, &self.body).expect("ahhh");
                (Value::Typing(t), Vec::new())
            },
            Kind::Value(typ) => {
                todo!()
            },
            Kind::Function(sig) => {
                todo!()
            },
        }
        // todo!();
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
    fn test_call_basic_def() {
        let def = FunctionDefinition {
            signature: FunctionSignature {
                inputs: vec![Kind::Blob],
                out: Box::new(Kind::Blob),
            },
            dependencies: HashMap::new(),
            body: String::from("arg0.push(19); arg0"),
        };

        let a = Value::Blob(Blob {bytes: vec![5]});

        let deps: HashMap<FunctionReference, FunctionValue> = HashMap::new();

        assert_eq!(def.call(vec![a], &deps), (Value::Blob(Blob{bytes:vec![5, 19]}), Vec::new()));
    }
}


