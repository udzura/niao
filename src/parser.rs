extern crate combine;
use combine::{choice, token, ParseError, Parser, Stream};

use crate::token::{Token as NiaoToken, TokenType};

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
    name: IdentValue,
    callers: Vec<IdentValue>,
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
        ident: VarWithCaller,
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
    varname: Box<NiaoToken>,
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

pub fn expr<Input>() -> impl combine::Parser<Input, Output = Expr>
where
    Input: Stream<Token = NiaoToken>,
    Input::Error: ParseError<Input::Token, Input::Range, Input::Position>,
{
    use crate::token::TokenType::*;

    choice((
        token(NiaoToken::of(StringLiteral)).map(|tok| Expr::StrLit {
            value: stringlit_to_value(&tok),
        }),
        token(NiaoToken::of(Numeric)).map(|tok| Expr::NumLit {
            value: number_to_value(&tok),
        }),
    ))
}

pub fn stmt<Input>() -> impl combine::Parser<Input, Output = Stmt>
where
    Input: Stream<Token = NiaoToken>,
    Input::Error: ParseError<Input::Token, Input::Range, Input::Position>,
{
    use crate::token::TokenType::*;

    (
        token(NiaoToken::of(Ident)),
        token(NiaoToken::of(Define)),
        expr(),
    )
        .map(
            |(ident, _, expr): (NiaoToken, NiaoToken, Expr)| Stmt::DefVar {
                ident: ident_to_value(&ident),
                expr: Box::new(expr),
            },
        )
}

pub fn stmts<Input>() -> impl combine::Parser<Input, Output = Vec<Stmt>>
where
    Input: Stream<Token = NiaoToken>,
    Input::Error: ParseError<Input::Token, Input::Range, Input::Position>,
{
    combine::many1(stmt())
}

pub fn block<Input>() -> impl combine::Parser<Input, Output = Block>
where
    Input: Stream<Token = NiaoToken>,
    Input::Error: ParseError<Input::Token, Input::Range, Input::Position>,
{
    use crate::token::TokenType::*;
    (stmts(), token(NiaoToken::of(Eof))).map(|(v, _): (Vec<Stmt>, _)| Block {
        statements: v,
        retstmt: None,
    })
}

fn number_to_value(token: &NiaoToken) -> isize {
    match token.token_type {
        TokenType::Numeric => token.lexeme[..].parse().unwrap(),
        _ => {
            unreachable!("Parser maybe has bug")
        }
    }
}

fn stringlit_to_value(token: &NiaoToken) -> String {
    match token.token_type {
        TokenType::StringLiteral => {
            let start = 1;
            let end = token.lexeme.len() - 1;
            token.lexeme[start..end].to_owned()
        }
        _ => {
            unreachable!("Parser maybe has bug")
        }
    }
}

fn ident_to_value(token: &NiaoToken) -> IdentValue {
    match token.token_type {
        TokenType::Ident => token.lexeme[..].to_owned(),
        _ => {
            unreachable!("Parser maybe has bug")
        }
    }
}

#[allow(dead_code)]
fn const_to_value(token: &NiaoToken) -> ConstValue {
    match token.token_type {
        TokenType::ConstIdent => token.lexeme[..].to_owned(),
        _ => {
            unreachable!("Parser maybe has bug")
        }
    }
}

#[cfg(test)]
pub mod tests;
