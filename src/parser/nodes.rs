use crate::{token::TokenType, types::Type};

#[derive(Debug, Clone, PartialEq)]
pub struct Block {
    pub statements: Vec<Stmt>,
    pub retstmt: Option<RetStmt>,
}

pub type IdentValue = String;
pub type ConstValue = String;

#[derive(Debug, Clone, PartialEq)]
pub struct VarWithCaller {
    pub name: IdentValue,
    pub callers: Vec<IdentValue>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Stmt {
    Void,
    Solo {
        expr: Box<Expr>,
    },
    DefVar {
        ident: IdentValue,
        expr: Box<Expr>,
    },
    Assign {
        ident: Box<Expr>,
        expr: Box<Expr>,
    },
    IfElse {
        condition: Box<Expr>,
        ifblk: Box<Block>,
        elseblk: Option<Box<Block>>,
    },
    For {
        iter: ForIter,
        block: Box<Block>,
    },
    DefStruct {
        name: ConstValue,
        members: Vec<Arg>,
    },
    DefFun {
        name: IdentValue,
        args: Vec<Arg>,
        ret: Type,
        block: Box<Block>,
    },
    Continue {},
    Break {},
    Annotation {
        name: IdentValue,
        args: Vec<AnnotLit>,
    },
}

#[derive(Debug, Clone, PartialEq)]
pub struct Arg {
    pub name: IdentValue,
    pub ty: Type,
}

#[derive(Debug, Clone, PartialEq)]
pub struct RetStmt {
    pub value: Option<Box<Expr>>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum AnnotLit {
    Str(String),
    Num(isize),
}

#[derive(Debug, Clone, PartialEq)]
pub struct ForIter {
    pub varname: IdentValue,
    pub how: IterMethod,
}

#[derive(Debug, Clone, PartialEq)]
pub enum IterMethod {
    StepIter {
        init: Box<Expr>,
        fini: Box<Expr>,
        step: Option<Box<Expr>>,
    },
    RangeIter {
        expr: Box<Expr>,
    },
    WhileIter {
        expr: Box<Expr>,
    },
}

#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
    Void,
    BoolLit {
        value: bool,
    },
    NumLit {
        value: isize,
    },
    StrLit {
        value: String,
    },
    TupleDef {
        values: Vec<Expr>,
    },
    StructDef {
        name: ConstValue,
        values: Vec<Expr>,
    },
    FunCall {
        callee: Box<Expr>,
        args: Vec<Expr>,
    },
    Var {
        value: VarWithCaller,
    },
    BinOp {
        op: TokenType,
        lhs: Box<Expr>,
        rhs: Box<Expr>,
    },
    UnOp {
        op: TokenType,
        operand: Box<Expr>,
    },
}
