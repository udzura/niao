#[cfg(test)]
mod stmt {
    use combine::{error::UnexpectedParse, Parser};

    use crate::{
        parser::Stmt,
        token::{Token, TokenStream},
    };

    #[test]
    fn test_parse_defvar() -> Result<(), Box<dyn std::error::Error>> {
        let source = include_str!("../../testcase/defvar.ni");
        let mut scanner = crate::scanner::Scanner::new(source);
        scanner.scan()?;

        let mut parser = crate::parser::block();
        let stream = scanner.token_stream();

        let (result, input) = parser.parse(stream)?;

        let stmts = &result.statements;
        assert!(stmts.len() == 3);
        if let Stmt::DefVar { ident, .. } = &stmts[0] {
            assert_eq!("foo", ident);
        } else {
            unreachable!("Parser failed: {:?}", &stmts);
        }

        if let Stmt::DefVar { ident, .. } = &stmts[1] {
            assert_eq!("bar", ident);
        } else {
            unreachable!("Parser failed: {:?}", &stmts);
        }

        if let Stmt::DefVar { ident, .. } = &stmts[2] {
            assert_eq!("buz", ident);
        } else {
            unreachable!("Parser failed: {:?}", &stmts);
        }

        assert!(input.stream.len() == 0);

        Ok(())
    }

    #[test]
    fn test_should_be_end_by_eof() -> Result<(), Box<dyn std::error::Error>> {
        let stream = TokenStream {
            stream: vec![
                Token {
                    token_type: crate::token::TokenType::Ident,
                    lexeme: "foo".to_string(),
                    pos: 0,
                    line: 0,
                },
                Token {
                    token_type: crate::token::TokenType::Define,
                    lexeme: ":=".to_string(),
                    pos: 0,
                    line: 0,
                },
                Token {
                    token_type: crate::token::TokenType::Numeric,
                    lexeme: "123".to_string(),
                    pos: 0,
                    line: 0,
                },
            ],
        };

        let mut parser = crate::parser::block();
        let result = parser.parse(stream);

        if let Err(e) = result {
            if let UnexpectedParse::Eoi = e {
                assert!(true);
                return Ok(());
            }
        }
        unreachable!("Should not succeed: {:?}", &result);
    }
}

#[cfg(test)]
mod expr {
    use crate::{parser::Expr, parser::Stmt};
    use combine::Parser;

    #[test]
    fn test_parse_basic_expr() -> Result<(), Box<dyn std::error::Error>> {
        let source = include_str!("../../testcase/simple.ni");
        let mut scanner = crate::scanner::Scanner::new(source);
        scanner.scan()?;

        let mut parser = crate::parser::block();
        let stream = scanner.token_stream();

        let (result, _) = parser.parse(stream)?;

        let stmts = &result.statements;
        assert!(stmts.len() == 2);
        loop {
            if let Stmt::DefVar { expr, .. } = &stmts[0] {
                if let Expr::NumLit { value } = expr.as_ref() {
                    assert_eq!(1234, *value);
                    break;
                }
            }
            unreachable!("Should be parsed as expr: {:?}", &stmts)
        }

        loop {
            if let Stmt::DefVar { expr, .. } = &stmts[1] {
                if let Expr::StrLit { value } = expr.as_ref() {
                    assert_eq!("any", value);
                    break;
                }
            }
            unreachable!("Should be parsed as expr: {:?}", &stmts)
        }

        Ok(())
    }
}
