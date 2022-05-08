use crate::token::TokenType;

#[derive(Debug, Clone, PartialEq)]
pub struct Block {
    pub statements: Vec<Stmt>,
    pub retstmt: Option<RetStmt>,
}

pub type IdentValue = String;
pub type ConstValue = String;
#[allow(dead_code)]
#[derive(Debug, Clone, PartialEq)]
pub struct VarWithCaller {
    pub name: IdentValue,
    pub callers: Vec<IdentValue>,
}

#[allow(dead_code)]
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
        members: Vec<IdentValue>,
    },
    DefFun {
        name: Box<IdentValue>,
        args: Vec<IdentValue>,
        block: Box<Block>,
    },
    Continue {},
    Break {},
    Annotation {
        name: Box<IdentValue>,
        args: Vec<AnnotLit>,
    },
}

#[allow(dead_code)]
#[derive(Debug, Clone, PartialEq)]
pub struct RetStmt {
    value: Option<Box<Expr>>,
}

#[allow(dead_code)]
#[derive(Debug, Clone, PartialEq)]
pub enum AnnotLit {
    Str(String),
    Num(isize),
}

#[allow(dead_code)]
#[derive(Debug, Clone, PartialEq)]
pub struct ForIter {
    varname: IdentValue,
    how: IterMethod,
}

#[allow(dead_code)]
#[derive(Debug, Clone, PartialEq)]
pub enum IterMethod {
    StepIter {
        init: Box<Expr>,
        fini: Box<Expr>,
        step: Box<Expr>,
    },
    RangeIter {
        expr: Box<Expr>,
    },
    WhileIter {
        expr: Box<Expr>,
    },
}

#[allow(dead_code)]
#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
    Void,
    True,
    False,
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
