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
    // Typing(Typing),
    // Value(RADTValue),
    // Function(Function),
}

#[derive(Debug, Clone, PartialEq)]
enum Kind {
    Blob,
    // Typing,
    // Value(FullType),
    // Function(FunctionSignature),
}

#[derive(Debug, Clone, PartialEq)]
struct FunctionSignature {
    inputs: Vec<Kind>,
    out: Box<Kind>,
    aux_outs: Vec<Kind>,
}

struct Function {
    signature: FunctionSignature,
    implementation: FunctionImplementation,
}

enum FunctionImplementation {
    Builtin(Box<dyn Fn(Vec<Value>) -> (Value, Vec<Value>)>),
    Defined(FunctionDefinition),
}

impl FunctionImplementation {
    fn call(&self, v: Vec<Value>) -> (Value, Vec<Value>) {
        match self {
            FunctionImplementation::Builtin(f) => {
                f(v)
            },
            FunctionImplementation::Defined(def) => {
                todo!()
            }
        }
    }
}

struct FunctionDefinition {
    dependencies: HashMap<String, Function>,
    body: String,
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
        let builtin = FunctionImplementation::Builtin(Box::new(add));
        let a = Value::Blob(Blob {bytes: vec![2]});
        let b = Value::Blob(Blob {bytes: vec![3]});

        assert_eq!(builtin.call(vec![a, b]), (Value::Blob(Blob{bytes:vec![5]}), Vec::new()));
    }
}
