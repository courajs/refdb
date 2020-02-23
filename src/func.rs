// type that represents a function signature
// type that represents a function's dependencies, argument types, and return type
// call a function given a dependency map and arguments

use std::collections::HashMap;
use std::any::Any;

use rhai::{Engine, Scope, RegisterFn};

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
                todo!()
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

        // engine.register_get("len", |b: &mut Blob| b.bytes.len());
        // engine.register_fn("get", |b: &mut Blob, idx: i64| b.bytes[idx as usize]);
        // engine.register_fn("set", |b: &mut Blob, idx: i64, val: i64| b.bytes[idx as usize] = val as u8);

        let b = sure!(&args[0], Value::Blob(Blob{bytes}) => bytes.clone());
        scope.push((String::from("arg"), Box::new(b)));

        let bytes = engine.eval_with_scope::<Vec<u8>>(&mut scope, &self.body).expect("ahhh");
        (Value::Blob(Blob{bytes}), Vec::new())
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
            body: String::from("arg"),
        };

        let a = Value::Blob(Blob {bytes: Vec::new()});

        let deps: HashMap<FunctionReference, FunctionValue> = HashMap::new();

        assert_eq!(def.call(vec![a.clone()], &deps), (a, Vec::new()));
    }
}


