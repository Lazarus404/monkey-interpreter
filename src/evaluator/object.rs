use crate::parser::ast::*;
use crate::evaluator::env::*;
use std::cell::RefCell;
use std::collections::HashMap;
use std::fmt;
use std::hash::{Hash, Hasher};
use std::rc::Rc;

pub type BuiltinFunc = fn(Vec<Object>) -> Object;

#[derive(PartialEq, Clone, Debug)]
pub enum Object {
  Int(i64),
  String(String),
  Bool(bool),
  Array(Vec<Object>),
  Hash(HashMap<Object, Object>),
  Function(Vec<Identity>, BlockStmt, Rc<RefCell<Env>>),
  Builtin(i32, BuiltinFunc),
  Null,
  ReturnValue(Box<Object>),
  Error(String),
  Quote(Expr),
}

impl fmt::Display for Object {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    match *self {
      Object::Int(ref value) => write!(f, "{}", value),
      Object::Bool(ref value) => write!(f, "{}", value),
      Object::String(ref value) => write!(f, "{}", value),
      Object::Array(ref objects) => {
        let mut result = String::new();
        for (i, obj) in objects.iter().enumerate() {
          if i < 1 {
            result.push_str(&format!("{}", obj));
          } else {
            result.push_str(&format!(", {}", obj));
          }
        }
        write!(f, "[{}]", result)
      }
      Object::Hash(ref hash) => {
        let mut result = String::new();
        for (i, (k, v)) in hash.iter().enumerate() {
          if i < 1 {
            result.push_str(&format!("{}: {}", k, v));
          } else {
            result.push_str(&format!(", {}: {}", k, v));
          }
        }
        write!(f, "{{{}}}", result)
      }
      Object::Function(ref params, _, _) => {
        let mut result = String::new();
        for (i, Identity(ref s)) in params.iter().enumerate() {
          if i < 1 {
            result.push_str(&format!("{}", s));
          } else {
            result.push_str(&format!(", {}", s));
          }
        }
        write!(f, "fn({}) {{ ... }}", result)
      }
      Object::Builtin(_, _) => write!(f, "[builtin function]"),
      Object::Null => write!(f, "null"),
      Object::ReturnValue(ref value) => write!(f, "{}", value),
      Object::Error(ref value) => write!(f, "{}", value),
      Object::Quote(ref value) => write!(f, "QUOTE({:?})", value),
    }
  }
}

impl Eq for Object {}

impl Hash for Object {
  fn hash<H: Hasher>(&self, state: &mut H) {
    match *self {
      Object::Int(ref i) => i.hash(state),
      Object::Bool(ref b) => b.hash(state),
      Object::String(ref s) => s.hash(state),
      _ => "".hash(state),
    }
  }
}
