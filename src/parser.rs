extern crate combine;
pub mod nodes;

pub use nodes::*;

type NiaoToken = crate::token::Token;

//use crate::token::TokenType;
use crate::token::TokenType::*;

use combine::*;

fn var<Input>() -> impl combine::Parser<Input, Output = Expr>
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

fn expr_<Input>() -> impl combine::Parser<Input, Output = Expr>
where
    Input: Stream<Token = NiaoToken>,
    Input::Error: ParseError<Input::Token, Input::Range, Input::Position>,
{
    (
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
        )),
        optional(
            (
                token(NiaoToken::of(LParen)),
                sep_by(expr(), token(NiaoToken::of(Comma))),
                token(NiaoToken::of(RParen)),
            )
                .map(|(_, args, _)| args),
        ),
    )
        .map(|(exp, parens)| match parens {
            Some(args) => Expr::FunCall {
                callee: Box::new(exp),
                args,
            },
            None => exp,
        })
}

fn binop_muldiv<Input>() -> impl combine::Parser<Input, Output = Expr>
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

fn binop_plusminus<Input>() -> impl combine::Parser<Input, Output = Expr>
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

fn binop_ltgt<Input>() -> impl combine::Parser<Input, Output = Expr>
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

fn binop_eql<Input>() -> impl combine::Parser<Input, Output = Expr>
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

fn binop_and<Input>() -> impl combine::Parser<Input, Output = Expr>
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

fn binop_or<Input>() -> impl combine::Parser<Input, Output = Expr>
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
    fn expr[Input]() (Input) -> Expr
    where [
        Input: Stream<Token = NiaoToken>,
        Input::Error: ParseError<Input::Token, Input::Range, Input::Position>,
    ] {
        binop_or()
    }
}

fn declare<Input>() -> impl combine::Parser<Input, Output = Stmt>
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

fn assign<Input>() -> impl combine::Parser<Input, Output = Stmt>
where
    Input: Stream<Token = NiaoToken>,
    Input::Error: ParseError<Input::Token, Input::Range, Input::Position>,
{
    attempt((var(), token(NiaoToken::of(Assign)), expr())).map(|(var, _, expr): (Expr, _, Expr)| {
        Stmt::Assign {
            ident: Box::new(var),
            expr: Box::new(expr),
        }
    })
}

fn if_stmt<Input>() -> impl combine::Parser<Input, Output = Stmt>
where
    Input: Stream<Token = NiaoToken>,
    Input::Error: ParseError<Input::Token, Input::Range, Input::Position>,
{
    (
        token(NiaoToken::of(If)),
        expr(),
        token(NiaoToken::of(LBrace)),
        block(),
        token(NiaoToken::of(RBrace)),
        optional(
            (
                token(NiaoToken::of(Else)),
                token(NiaoToken::of(LBrace)),
                block(),
                token(NiaoToken::of(RBrace)),
            )
                .map(|(_, _, blk, _)| blk),
        ),
    )
        .map(|(_, exp, _, ifblk, _, elseblk)| Stmt::IfElse {
            condition: Box::new(exp),
            ifblk: Box::new(ifblk),
            elseblk: elseblk.map(|b| Box::new(b)),
        })
}

fn for_stmt<Input>() -> impl combine::Parser<Input, Output = Stmt>
where
    Input: Stream<Token = NiaoToken>,
    Input::Error: ParseError<Input::Token, Input::Range, Input::Position>,
{
    (
        token(NiaoToken::of(For)),
        token(NiaoToken::of(Ident)),
        itermethod(),
        token(NiaoToken::of(LBrace)),
        block(),
        token(NiaoToken::of(RBrace)),
    )
        .map(|(_, ident, im, _, blk, _)| {
            let foriter = ForIter {
                varname: ident_to_value(&ident),
                how: im,
            };
            Stmt::For {
                iter: foriter,
                block: Box::new(blk),
            }
        })
}

fn itermethod_step<Input>() -> impl combine::Parser<Input, Output = IterMethod>
where
    Input: Stream<Token = NiaoToken>,
    Input::Error: ParseError<Input::Token, Input::Range, Input::Position>,
{
    (
        attempt((token(NiaoToken::of(In)), token(NiaoToken::of(Step)))),
        token(NiaoToken::of(LParen)),
        expr(),
        token(NiaoToken::of(Comma)),
        expr(),
        optional((token(NiaoToken::of(Comma)), expr()).map(|(_, expr)| expr)),
        token(NiaoToken::of(RParen)),
    )
        .map(|(_, _, init, _, fini, step, _)| IterMethod::StepIter {
            init: Box::new(init),
            fini: Box::new(fini),
            step: step.map(|e| Box::new(e)),
        })
}

fn itermethod_range<Input>() -> impl combine::Parser<Input, Output = IterMethod>
where
    Input: Stream<Token = NiaoToken>,
    Input::Error: ParseError<Input::Token, Input::Range, Input::Position>,
{
    (
        attempt((token(NiaoToken::of(In)), token(NiaoToken::of(Range)))),
        token(NiaoToken::of(LParen)),
        expr(),
        token(NiaoToken::of(RParen)),
    )
        .map(|(_, _, expr, _)| IterMethod::RangeIter {
            expr: Box::new(expr),
        })
}

fn itermethod_while<Input>() -> impl combine::Parser<Input, Output = IterMethod>
where
    Input: Stream<Token = NiaoToken>,
    Input::Error: ParseError<Input::Token, Input::Range, Input::Position>,
{
    (
        token(NiaoToken::of(While)),
        token(NiaoToken::of(LParen)),
        expr(),
        token(NiaoToken::of(RParen)),
    )
        .map(|(_, _, expr, _)| IterMethod::WhileIter {
            expr: Box::new(expr),
        })
}

fn itermethod<Input>() -> impl combine::Parser<Input, Output = IterMethod>
where
    Input: Stream<Token = NiaoToken>,
    Input::Error: ParseError<Input::Token, Input::Range, Input::Position>,
{
    choice((itermethod_step(), itermethod_range(), itermethod_while()))
}

fn fun<Input>() -> impl combine::Parser<Input, Output = Stmt>
where
    Input: Stream<Token = NiaoToken>,
    Input::Error: ParseError<Input::Token, Input::Range, Input::Position>,
{
    (
        token(NiaoToken::of(Fun)),
        token(NiaoToken::of(Ident)),
        token(NiaoToken::of(LParen)),
        sep_by(
            token(NiaoToken::of(Ident)).map(|i| ident_to_value(&i)),
            token(NiaoToken::of(Comma)),
        ),
        token(NiaoToken::of(RParen)),
        token(NiaoToken::of(LBrace)),
        block(),
        token(NiaoToken::of(RBrace)),
    )
        .map(|(_, name, _, args, _, _, blk, _)| Stmt::DefFun {
            name: ident_to_value(&name),
            args,
            block: Box::new(blk),
        })
}

fn structdef<Input>() -> impl combine::Parser<Input, Output = Stmt>
where
    Input: Stream<Token = NiaoToken>,
    Input::Error: ParseError<Input::Token, Input::Range, Input::Position>,
{
    (
        token(NiaoToken::of(Struct)),
        token(NiaoToken::of(ConstIdent)),
        token(NiaoToken::of(LDoubleBrace)),
        sep_by(
            token(NiaoToken::of(Ident)).map(|i| ident_to_value(&i)),
            token(NiaoToken::of(Comma)),
        ),
        token(NiaoToken::of(RDoubleBrace)),
    )
        .map(|(_, name, _, members, _)| Stmt::DefStruct {
            name: const_to_value(&name),
            members,
        })
}

fn annotation<Input>() -> impl combine::Parser<Input, Output = Stmt>
where
    Input: Stream<Token = NiaoToken>,
    Input::Error: ParseError<Input::Token, Input::Range, Input::Position>,
{
    let annotlit = choice((
        token(NiaoToken::of(StringLiteral)),
        token(NiaoToken::of(Numeric)),
    ))
    .map(|tok: NiaoToken| match tok.token_type {
        StringLiteral => AnnotLit::Str(stringlit_to_value(&tok)),
        Numeric => AnnotLit::Num(number_to_value(&tok)),
        _ => unreachable!("Maybe parser has bug"),
    });
    (
        token(NiaoToken::of(At)),
        token(NiaoToken::of(Ident)),
        optional(
            (
                token(NiaoToken::of(LParen)),
                sep_by1(annotlit, token(NiaoToken::of(Comma))),
                token(NiaoToken::of(RParen)),
            )
                .map(|(_, vec, _)| vec),
        ),
    )
        .map(|(_, ident, args)| match args {
            Some(args) => Stmt::Annotation {
                name: ident_to_value(&ident),
                args,
            },
            None => Stmt::Annotation {
                name: ident_to_value(&ident),
                args: vec![],
            },
        })
}

parser! {
    fn stmt[Input]() (Input) -> Stmt
    where [
        Input: Stream<Token = NiaoToken>,
        Input::Error: ParseError<Input::Token, Input::Range, Input::Position>,
    ] {
    choice((
        declare(),
        assign(),
        if_stmt(),
        for_stmt(),
        fun(),
        structdef(),
        token(NiaoToken::of(Continue)).map(|_| Stmt::Continue {  }),
        token(NiaoToken::of(Break)).map(|_| Stmt::Break{  }),
        annotation(),
        expr().map(|expr| Stmt::Solo {
            expr: Box::new(expr),
        }),
    ))
    }
}

fn stmts<Input>() -> impl combine::Parser<Input, Output = Vec<Stmt>>
where
    Input: Stream<Token = NiaoToken>,
    Input::Error: ParseError<Input::Token, Input::Range, Input::Position>,
{
    combine::many1(stmt())
}

fn block<Input>() -> impl combine::Parser<Input, Output = Block>
where
    Input: Stream<Token = NiaoToken>,
    Input::Error: ParseError<Input::Token, Input::Range, Input::Position>,
{
    stmts().map(|v: Vec<Stmt>| Block {
        statements: v,
        retstmt: None,
    })
}

pub fn chunk<Input>() -> impl combine::Parser<Input, Output = Block>
where
    Input: Stream<Token = NiaoToken>,
    Input::Error: ParseError<Input::Token, Input::Range, Input::Position>,
{
    (block(), token(NiaoToken::of(Eof))).map(|(v, _): (Block, _)| v)
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
