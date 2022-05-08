extern crate combine;
pub mod nodes;

pub use nodes::*;

type NiaoToken = crate::token::Token;

//use crate::token::TokenType;
use crate::token::TokenType::*;

use combine::*;

pub fn var<Input>() -> impl combine::Parser<Input, Output = Expr>
where
    Input: Stream<Token = NiaoToken>,
    Input::Error: ParseError<Input::Token, Input::Range, Input::Position>,
{
    (
        token(NiaoToken::of(Ident)),
        many(
            (token(NiaoToken::of(Dot)), token(NiaoToken::of(Ident)))
                .map(|(_, ident): (_, NiaoToken)| ident_to_value(&ident)),
        ),
    )
        .map(|(tok, idents)| {
            let name = ident_to_value(&tok);
            let callers = idents;
            Expr::Var {
                value: VarWithCaller { name, callers },
            }
        })
}

pub fn expr_<Input>() -> impl combine::Parser<Input, Output = Expr>
where
    Input: Stream<Token = NiaoToken>,
    Input::Error: ParseError<Input::Token, Input::Range, Input::Position>,
{
    choice((
        (
            token(NiaoToken::of(LParen)),
            expr(),
            token(NiaoToken::of(RParen)),
        )
            .map(|(_, ex, _): (_, Expr, _)| ex),
        token(NiaoToken::of(StringLiteral)).map(|tok| Expr::StrLit {
            value: stringlit_to_value(&tok),
        }),
        token(NiaoToken::of(Numeric)).map(|tok| Expr::NumLit {
            value: number_to_value(&tok),
        }),
        var(),
    ))
}

pub fn binop_muldiv<Input>() -> impl combine::Parser<Input, Output = Expr>
where
    Input: Stream<Token = NiaoToken>,
    Input::Error: ParseError<Input::Token, Input::Range, Input::Position>,
{
    let op =
        choice((token(NiaoToken::of(Aster)), token(NiaoToken::of(Slash)))).map(|tok: NiaoToken| {
            move |lhs: Expr, rhs: Expr| Expr::BinOp {
                op: tok.token_type,
                lhs: Box::new(lhs),
                rhs: Box::new(rhs),
            }
        });
    chainl1(expr_(), op)
}

pub fn binop_plusminus<Input>() -> impl combine::Parser<Input, Output = Expr>
where
    Input: Stream<Token = NiaoToken>,
    Input::Error: ParseError<Input::Token, Input::Range, Input::Position>,
{
    let op =
        choice((token(NiaoToken::of(Plus)), token(NiaoToken::of(Minus)))).map(|tok: NiaoToken| {
            move |lhs: Expr, rhs: Expr| Expr::BinOp {
                op: tok.token_type,
                lhs: Box::new(lhs),
                rhs: Box::new(rhs),
            }
        });
    chainl1(binop_muldiv(), op)
}

pub fn binop_ltgt<Input>() -> impl combine::Parser<Input, Output = Expr>
where
    Input: Stream<Token = NiaoToken>,
    Input::Error: ParseError<Input::Token, Input::Range, Input::Position>,
{
    let op = choice((token(NiaoToken::of(Less)), token(NiaoToken::of(Greater)))).map(
        |tok: NiaoToken| {
            move |lhs: Expr, rhs: Expr| Expr::BinOp {
                op: tok.token_type,
                lhs: Box::new(lhs),
                rhs: Box::new(rhs),
            }
        },
    );
    chainl1(binop_plusminus(), op)
}

pub fn binop_eql<Input>() -> impl combine::Parser<Input, Output = Expr>
where
    Input: Stream<Token = NiaoToken>,
    Input::Error: ParseError<Input::Token, Input::Range, Input::Position>,
{
    use crate::token::TokenType::*;
    let op =
        choice((token(NiaoToken::of(Eql)), token(NiaoToken::of(Neql)))).map(|tok: NiaoToken| {
            move |lhs: Expr, rhs: Expr| Expr::BinOp {
                op: tok.token_type,
                lhs: Box::new(lhs),
                rhs: Box::new(rhs),
            }
        });
    chainl1(binop_ltgt(), op)
}

pub fn binop_and<Input>() -> impl combine::Parser<Input, Output = Expr>
where
    Input: Stream<Token = NiaoToken>,
    Input::Error: ParseError<Input::Token, Input::Range, Input::Position>,
{
    let op = token(NiaoToken::of(And)).map(|tok: NiaoToken| {
        move |lhs: Expr, rhs: Expr| Expr::BinOp {
            op: tok.token_type,
            lhs: Box::new(lhs),
            rhs: Box::new(rhs),
        }
    });
    chainl1(binop_eql(), op)
}

pub fn binop_or<Input>() -> impl combine::Parser<Input, Output = Expr>
where
    Input: Stream<Token = NiaoToken>,
    Input::Error: ParseError<Input::Token, Input::Range, Input::Position>,
{
    let op = token(NiaoToken::of(Or)).map(|tok: NiaoToken| {
        move |lhs: Expr, rhs: Expr| Expr::BinOp {
            op: tok.token_type,
            lhs: Box::new(lhs),
            rhs: Box::new(rhs),
        }
    });
    chainl1(binop_and(), op)
}

parser! {
    pub fn expr[Input]() (Input) -> Expr
    where [
        Input: Stream<Token = NiaoToken>,
        Input::Error: ParseError<Input::Token, Input::Range, Input::Position>,
    ] {
        binop_or()
    }
}

pub fn declare<Input>() -> impl combine::Parser<Input, Output = Stmt>
where
    Input: Stream<Token = NiaoToken>,
    Input::Error: ParseError<Input::Token, Input::Range, Input::Position>,
{
    attempt((
        token(NiaoToken::of(Ident)),
        token(NiaoToken::of(Define)),
        expr(),
    ))
    .map(
        |(ident, _, expr): (NiaoToken, NiaoToken, Expr)| Stmt::DefVar {
            ident: ident_to_value(&ident),
            expr: Box::new(expr),
        },
    )
}

pub fn stmt<Input>() -> impl combine::Parser<Input, Output = Stmt>
where
    Input: Stream<Token = NiaoToken>,
    Input::Error: ParseError<Input::Token, Input::Range, Input::Position>,
{
    choice((
        declare(),
        expr().map(|expr| Stmt::Solo {
            expr: Box::new(expr),
        }),
    ))
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
    (stmts(), token(NiaoToken::of(Eof))).map(|(v, _): (Vec<Stmt>, _)| Block {
        statements: v,
        retstmt: None,
    })
}

fn number_to_value(token: &NiaoToken) -> isize {
    match token.token_type {
        Numeric => token.lexeme[..].parse().unwrap(),
        _ => {
            unreachable!("Parser maybe has bug")
        }
    }
}

fn stringlit_to_value(token: &NiaoToken) -> String {
    match token.token_type {
        StringLiteral => {
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
        Ident => token.lexeme[..].to_owned(),
        _ => {
            unreachable!("Parser maybe has bug")
        }
    }
}

#[allow(dead_code)]
fn const_to_value(token: &NiaoToken) -> ConstValue {
    match token.token_type {
        ConstIdent => token.lexeme[..].to_owned(),
        _ => {
            unreachable!("Parser maybe has bug")
        }
    }
}

#[cfg(test)]
pub mod tests;
