// type that represents a function signature
// type that represents a function's dependencies, argument types, and return type
// call a function given a dependency map and arguments

use std::collections::HashMap;

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
        (self.f)(args)
    }
}

#[derive(Debug, Clone, PartialEq)]
struct FunctionDefinition {
    signature: FunctionSignature,
    dependencies: HashMap<String, FunctionReference>,
    body: String,
}

#[derive(Debug, Clone, PartialEq)]
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
    fn test_raw_call_builtin() {
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
}
