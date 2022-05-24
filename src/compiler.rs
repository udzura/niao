use std::error::Error;

use crate::ops::{Immidiate, Op};
use crate::parser::{Block, Expr, Stmt};
use crate::token::TokenType;

#[derive(Debug)]
pub struct Compiler {}

pub fn compile(chunk: Block, dest: &mut Vec<Op>) -> Result<(), Box<dyn Error>> {
    for stmt in chunk.statements.iter() {
        match stmt {
            Stmt::Solo { expr } => compile_expr(expr.as_ref(), dest)?,
            _ => {
                todo!("Lets go")
            }
        };
    }
    Ok(())
}

pub fn compile_expr(expr: &Expr, dest: &mut Vec<Op>) -> Result<(), Box<dyn Error>> {
    match expr {
        Expr::NumLit { value } => {
            let value = Immidiate::Number(*value as i32);
            let op = Op::Constant { value };
            dest.push(op)
        }
        Expr::Var { value } => {
            if value.callers.len() == 0 {
                let ident = value.name.clone();
                let op = Op::GetGlobal { ident };
                dest.push(op)
            } else {
                todo!("caller")
            }
        }
        Expr::BinOp { op, lhs, rhs } => {
            compile_expr(lhs.as_ref(), dest)?;
            compile_expr(rhs.as_ref(), dest)?;
            match op {
                TokenType::Plus => {
                    let op = Op::Plus;
                    dest.push(op)
                }
                _ => {
                    todo!("support me")
                }
            }
        }
        Expr::FunCall { callee, args } => {
            compile_expr(callee.as_ref(), dest)?;
            for arg in args.iter() {
                compile_expr(arg, dest)?;
            }
            let op = Op::Call { args: args.len() };
            dest.push(op)
        }
        _ => {
            todo!("Lets go")
        }
    };
    Ok(())
}
