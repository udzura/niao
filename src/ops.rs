use crate::types::Type;

#[derive(Debug, Clone, PartialEq)]
pub enum Op {
    Constant { value: Immidiate },
    Plus,
    GetGlobal { ident: String },
    Call { args: usize },
}

#[derive(Debug, Clone, PartialEq)]
pub enum Immidiate {
    Nil,
    Bool(bool),
    Number(i32),
    ObjString(String),
}

#[derive(Debug, Clone, PartialEq)]
pub struct Value {
    pub ty: Box<Type>,
    pub contained: Container,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Container {
    Nil,
    Bool(bool),
    Number(i32),
    ObjString(String),
    ObjArray { values: Vec<Value> },
    ObjTupple { values: Vec<Value> },
}
