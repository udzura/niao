extern crate combine;
use combine::{token, ParseError, Parser, Stream};

use crate::token::Token as NiaoToken;

#[derive(Debug, Clone)]
pub struct Block {
    pub statements: Vec<Stmt>,
    pub retstmt: Option<Stmt>,
}

#[allow(dead_code)]
#[derive(Debug, Clone, PartialEq)]
pub enum Stmt {
    Solo {
        expr: Box<Expr>,
    },
    DefVar {
        ident: Box<NiaoToken>,
        expr: Box<Expr>,
    },
}

#[allow(dead_code)]
#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
    Void,
    Lit { literal: Box<NiaoToken> },
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
        token(NiaoToken::of(Numeric)),
    )
        .map(|(ident, _, expr): (NiaoToken, NiaoToken, NiaoToken)| {
            let expr = Expr::Lit {
                literal: Box::new(expr),
            };
            Stmt::DefVar {
                ident: Box::new(ident),
                expr: Box::new(expr),
            }
        })
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

#[cfg(test)]
pub mod tests;
