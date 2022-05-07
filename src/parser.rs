extern crate combine;
use combine::{choice, token, ParseError, Parser, Stream};

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

pub fn expr<Input>() -> impl combine::Parser<Input, Output = Expr>
where
    Input: Stream<Token = NiaoToken>,
    Input::Error: ParseError<Input::Token, Input::Range, Input::Position>,
{
    use crate::token::TokenType::*;

    choice((
        token(NiaoToken::of(StringLiteral)),
        token(NiaoToken::of(Numeric)),
    ))
    .map(|expr| Expr::Lit {
        literal: Box::new(expr),
    })
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
                ident: Box::new(ident),
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

#[cfg(test)]
pub mod tests;
