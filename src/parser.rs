extern crate combine;
use combine::{token, ParseError, Parser, Stream};

use crate::token::TokenIndex;

#[derive(Debug, Clone)]
pub struct Block {
    pub statements: Vec<Stmt>,
    pub retstmt: Option<Stmt>,
}

#[allow(dead_code)]
#[derive(Debug, Clone, PartialEq)]
pub enum Stmt {
    Solo { expr: Box<Expr> },
    DefVar { ident: usize, expr: Box<Expr> },
}

#[allow(dead_code)]
#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
    Void,
    Lit { literal: usize },
}

pub fn stmt<Input>() -> impl combine::Parser<Input, Output = Stmt>
where
    Input: Stream<Token = TokenIndex>,
    Input::Error: ParseError<Input::Token, Input::Range, Input::Position>,
{
    use crate::token::TokenType::*;

    (
        token(TokenIndex::of(Ident)),
        token(TokenIndex::of(Define)),
        token(TokenIndex::of(Numeric)),
    )
        .map(|(ident, _, expr): (TokenIndex, TokenIndex, TokenIndex)| {
            let expr = Expr::Lit { literal: expr.1 };
            Stmt::DefVar {
                ident: ident.1,
                expr: Box::new(expr),
            }
        })
}

pub fn stmts<Input>() -> impl combine::Parser<Input, Output = Vec<Stmt>>
where
    Input: Stream<Token = TokenIndex>,
    Input::Error: ParseError<Input::Token, Input::Range, Input::Position>,
{
    combine::many1(stmt())
}

pub fn block<Input>() -> impl combine::Parser<Input, Output = Block>
where
    Input: Stream<Token = TokenIndex>,
    Input::Error: ParseError<Input::Token, Input::Range, Input::Position>,
{
    stmts().map(|v| Block {
        statements: v,
        retstmt: None,
    })
}
