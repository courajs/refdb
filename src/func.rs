// type that represents a function signature
// type that represents a function's dependencies, argument types, and return type
// call a function given a dependency map and arguments

use std::collections::HashMap;
use std::collections::BTreeMap;
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
use bridged_group::*;


use crate as rf0;
bridged_group! {
    #![uniq(*b"core:function---")]

    #[derive(Debug, Clone, PartialEq, Eq)]
    pub struct FunctionDefinition {
        pub signature: FunctionSignature,
        pub dependencies: BTreeMap<String, FunctionReference>,
        pub body: String,
    }

    #[derive(Debug, Clone, PartialEq, Eq)]
    pub struct FunctionSignature {
        pub inputs: Vec<Kind>,
        pub out: Box<Kind>,
    }

    #[derive(Debug, Clone, PartialEq, Eq, Hash)]
    pub enum FunctionReference {
        Builtin(usize),
        // TODO: HACK: this really points to a FunctionDefinition.
        // But because this is used to generate the radt, it affects
        // itself. Really this is because split point vs inline is conflated
        // with ExternalType vs CycleRef. If we add ability to split at
        // CycleRefs we can clean this up.
        Definition(Hash!(ANY_TYPE_REF)),
    }

    #[derive(Debug, Clone, PartialEq, Eq)]
    pub enum Kind {
        Blob,
        Typing,
        Value(FullType),
        Function(FunctionSignature),
    }

    #[derive(Debug, Clone, PartialEq, Eq)]
    pub struct FullType {
        pub radt: RADT,
        pub item: usize,
    }
}




#[derive(Debug, Clone, PartialEq)]
pub enum Value {
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
    fn from_any(any: &dyn AnyClone) -> Result<Value, &dyn AnyClone> {
        any.downcast_ref::<Value>().map(Value::clone)
            .or_else(|| any.downcast_ref::<Blob>().map(|b|Value::Blob(b.clone())))
            .or_else(|| any.downcast_ref::<Typing>().map(|t|Value::Typing(t.clone())))
            .or_else(|| any.downcast_ref::<RADTValue>().map(|v|Value::Value(v.clone())))
            .or_else(|| any.downcast_ref::<FunctionReference>().map(|f|Value::Function(f.clone())))
            .ok_or(any)
    }
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

    fn discriminated_any(&self, v: Value) -> Result<Box<dyn AnyClone>, Value> {
        if v.conforms(self) {
            Ok(match v {
                Value::Blob(inner) => Box::new(inner),
                Value::Value(inner) => Box::new(inner),
                Value::Typing(inner) => Box::new(inner),
                Value::Function(inner) => Box::new(inner),
            })
        } else {
            Err(v)
        }
    }
}

impl FunctionSignature {
    fn args_to_type_ids(&self) -> Vec<TypeId> {
        self.inputs.iter().map(Kind::to_value_type_id).collect()
    }
}

pub enum FunctionValue {
    Builtin(BuiltinFunction),
    Defined(FunctionDefinition),
}

pub struct BuiltinFunction {
    signature: FunctionSignature,
    f: Box<dyn Fn(Vec<Value>) -> Value>,
}

impl BuiltinFunction {
    fn call(&self, args: Vec<Value>) -> Value {
        if self.signature.inputs.len() != args.len() {
            panic!("builtin called with wrong number of inputs")
        }
        for (arg, typ) in args.iter().zip(self.signature.inputs.iter()) {
            if !arg.conforms(typ) {
                panic!("builtin call parameter mismatch")
            }
        }
        let val = (self.f)(args);
        if !val.conforms(&self.signature.out) {
            panic!("unexpected return type from builtin function")
        }
        val
    }
}

fn generate_builtin_map() -> Vec<(FnSpec, Arc<FnIntExt>)> {
        let mut eng = Engine::new();

        let mut idx = -1;
        let mut next = || {
            idx += 1;
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
            // Have to ignore builtins like +
            .filter(|(spec,_)| {
                usize::from_str_radix(&spec.ident, 10).is_ok()
            }).collect();
        result.sort_by_key(|(spec,_)| {
            usize::from_str_radix(&spec.ident, 10).unwrap()
        });

        result
}

pub struct PreparedFunction {
    signature: FunctionSignature,
    engine: Engine,
    body: String,
}
impl PreparedFunction {
    fn call(&self, args: Vec<Value>) -> Result<Value, rhai::engine::EvalAltResult> {
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
                let blob = self.engine.clone().eval_with_scope::<Blob>(&mut scope, &self.body)?;
                Ok(Value::Blob(blob))
                // match blob {
                //     Value::Blob(b) => Ok(Value::Blob(b)),
                //     _ => panic!("ahh wrong output type"),
                // }
                // Ok(Value::Blob(blob))
            },
            Kind::Typing => {
                todo!()
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
        let inner = self.engine;
        eng.register_fn_raw(ident.clone(), Some(self.signature.args_to_type_ids()), Box::new(move |args| {
            inner.call_fn_raw(ident.clone(), args)
        }));
    }
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

    fn prepare(self, mut db_deps: HashMap<Hash, PreparedFunction>, builtins: &[(FnSpec, Arc<FnIntExt>)]) -> PreparedFunction {
        let mut engine = Engine::new();
        for (name, fref) in self.dependencies {
            match fref {
                FunctionReference::Builtin(i) => {
                    engine.fns.insert(FnSpec {
                        ident: name.clone(),
                        args: builtins[i].0.args.clone(),
                    }, builtins[i].1.clone());
                },
                FunctionReference::Definition(h) => {
                    let mut f = db_deps.remove(&h).expect("should pass all dependencies!");
                    let sig = self.signature.clone();
                    engine.register_fn_raw(name, Some(self.signature.args_to_type_ids()), Box::new(move |args| {
                        f.call(args.into_iter().map(|a|Value::from_any(a).expect("ahh non-value argument")).collect())
                            .map(|result|-> Box<dyn AnyClone> {
                                sig.out.discriminated_any(result).expect("ahh wrong return type")
                            })
                    }));
                    // todo!();
                    // let f = db_deps.remove(&h)
                }
            }
        }
        PreparedFunction {
            signature: self.signature,
            engine,
            body: self.body,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn add(v: Vec<Value>) -> Value {
        let a = sure!(&v[0], Value::Blob(Blob { bytes }) => bytes[0]);
        let b = sure!(&v[1], Value::Blob(Blob { bytes }) => bytes[0]);
        Value::Blob(Blob { bytes: vec![a+b] })
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

        assert_eq!(builtin.call(vec![a, b]), Value::Blob(Blob{bytes:vec![5]}));
    }

    #[test]
    fn test_call_def_with_builtin_deps() {
        let def = FunctionDefinition {
            signature: FunctionSignature {
                inputs: vec![Kind::Blob],
                out: Box::new(Kind::Blob),
            },
            dependencies: BTreeMap::from_iter([
                 (String::from("blob"), FunctionReference::Builtin(0)),
                 (String::from("len"), FunctionReference::Builtin(1)),
                 (String::from("push"), FunctionReference::Builtin(4)),
            ].iter().cloned()),
            body: String::from("let b = blob(); b.push(arg0.len()); b"),
        };
        let builtins = generate_builtin_map();
        let mut prepped = def.prepare(HashMap::new(), &builtins);
        let v = prepped.call(vec![Value::Blob(Blob{bytes: vec![12,12,12]})]).expect("ahh");
        assert_eq!(v, Value::Blob(Blob{bytes:vec![3]}));
    }

    #[test]
    fn fn_def_with_def_deps() {
        let sub_def = FunctionDefinition {
            signature: FunctionSignature {
                inputs: vec![Kind::Blob],
                out: Box::new(Kind::Blob),
            },
            dependencies: BTreeMap::from_iter([
                 (String::from("blob"), FunctionReference::Builtin(0)),
                 (String::from("len"), FunctionReference::Builtin(1)),
                 (String::from("push"), FunctionReference::Builtin(4)),
            ].iter().cloned()),
            body: String::from("let b = blob(); b.push(arg0.len()); b"),
        };
        let builtins = generate_builtin_map();
        let mut sub_prepped = sub_def.prepare(HashMap::new(), &builtins);

        let main_def = FunctionDefinition {
            signature: FunctionSignature {
                inputs: vec![Kind::Blob],
                out: Box::new(Kind::Blob),
            },
            dependencies: BTreeMap::from_iter([
                (String::from("sub"), FunctionReference::Definition(Hash::of(b"owl"))),
            ].iter().cloned()),
            body: String::from("sub(arg0)"),
        };
        let mut deps = HashMap::new();
        deps.insert(Hash::of(b"owl"), sub_prepped);
        let mut main_prepped = main_def.prepare(deps, &builtins);
        let v = main_prepped.call(vec![Value::Blob(Blob{bytes: vec![12,12,12]})]).expect("ahh");
        assert_eq!(v, Value::Blob(Blob{bytes:vec![3]}));
    }
}
