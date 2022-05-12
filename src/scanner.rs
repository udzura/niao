use std::error::Error;
use std::fmt;

#[derive(Debug)]
pub struct ScanError {
    start: usize,
    current: usize,
    line: usize,
}

impl ScanError {
    pub fn raise(s: &Scanner) -> Self {
        Self {
            start: s.start,
            current: s.current,
            line: s.line,
        }
    }
}

impl fmt::Display for ScanError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "ScanError at {}..{} (line %{})",
            self.start, self.current, self.line
        )
    }
}

impl Error for ScanError {}

use crate::token::Token;
use crate::token::TokenStream;
use crate::token::TokenType;
use crate::token::TokenType::*;
use crate::types::Type;

#[derive(Debug)]
pub struct Scanner<'source> {
    pub source: &'source str,
    pub tokens: Vec<Token>,
    start: usize,
    current: usize,
    line: usize,
    // current_index: usize,
    // needs context?
}

impl<'source> Scanner<'source> {
    pub fn new(source: &'source str) -> Self {
        let tokens = Vec::new();
        Self {
            source,
            tokens,
            start: 0,
            current: 0,
            line: 1,
        }
    }

    pub fn scan(&mut self) -> Result<usize, ScanError> {
        while !self.is_at_end() {
            self.start = self.current;
            self.scan_token()?;
        }

        self.push(Token::new(Eof, "", self.start, self.line));
        Ok(self.tokens.len())
    }

    pub fn token_stream(&mut self) -> TokenStream {
        TokenStream {
            stream: self.tokens.clone(),
        }
    }

    fn scan_token(&mut self) -> Result<(), ScanError> {
        let c = self.advance()?;
        match c {
            '@' => {
                self.push_token(At);
            }
            '(' => {
                self.push_token(LParen);
            }
            ')' => {
                self.push_token(RParen);
            }
            '{' => {
                let tok = if self.test('{')? {
                    LDoubleBrace
                } else {
                    LBrace
                };
                self.push_token(tok);
            }
            '}' => {
                let tok = if self.test('}')? {
                    RDoubleBrace
                } else {
                    RBrace
                };
                self.push_token(tok);
            }
            ',' => {
                self.push_token(Comma);
            }
            '.' => {
                self.push_token(Dot);
            }
            '-' => {
                self.push_token(Minus);
            }
            '+' => {
                self.push_token(Plus);
            }
            '&' => {
                self.push_token(Amp);
            }
            '*' => {
                self.push_token(Aster);
            }
            '=' => {
                let tok = if self.test('=')? { Eql } else { Assign };
                self.push_token(tok);
            }
            '<' => {
                self.push_token(Less);
            }
            '>' => {
                self.push_token(Greater);
            }
            '/' => {
                self.push_token(Slash);
            }
            ':' => {
                if self.test('=')? {
                    self.push_token(Define);
                } else {
                    self.typelit()?;
                }
            }

            ' ' | '\r' | '\t' => {
                // Ignore whitespace.
            }
            '\n' => {
                self.line += 1;
            }
            '"' => {
                self.string('"')?;
            }

            c => {
                if is_digit(c) {
                    self.number()?;
                } else if is_capital(c) {
                    self.constident()?;
                } else if is_alpha(c) {
                    self.keyword_or_ident()?;
                } else {
                    eprintln!("unexpected character: {}", c);
                    return Err(ScanError::raise(self));
                }
            }
        }
        Ok(())
    }

    fn string(&mut self, quote: char) -> Result<(), ScanError> {
        while self.peek()? != quote && !self.is_at_end() {
            if self.peek()? == '\n' {
                eprintln!("cannot contain linebreak");
                return Err(ScanError::raise(self));
            }
            self.advance()?;
        }

        if self.is_at_end() {
            eprintln!("Unterminated string");
            return Err(ScanError::raise(self));
        }

        // The closing quote.
        self.advance()?;
        self.push_token(StringLiteral);

        Ok(())
    }

    fn number(&mut self) -> Result<(), ScanError> {
        while is_digit(self.peek()?) {
            self.advance()?;
        }

        self.push_token(Numeric);

        Ok(())
    }

    fn constident(&mut self) -> Result<(), ScanError> {
        while is_alphanumeric(self.peek()?) {
            self.advance()?;
        }

        self.push_token(ConstIdent);
        Ok(())
    }

    fn typelit(&mut self) -> Result<(), ScanError> {
        while self.test(' ')? {
            // advance
        }
        let mut is_ptr = false;
        if self.test('*')? {
            is_ptr = true;
        }

        // TODO: tuple
        if is_alpha(self.peek()?) {
            let name_start = self.current;
            self.advance()?;
            while is_alphanumeric(self.peek()?) {
                self.advance()?;
            }
            let end = self.current;
            let name = &self.source[name_start..end];

            let base = Type::basic(name.to_string());
            if is_ptr {
                let ty = Type::pointer(base);
                self.push_type_token(ty);
            } else {
                self.push_type_token(base);
            }
        } else {
            eprintln!("unexpected character: {}", self.peek()?);
            return Err(ScanError::raise(self));
        }
        Ok(())
    }

    fn keyword_or_ident(&mut self) -> Result<(), ScanError> {
        while is_alphanumeric(self.peek()?) {
            self.advance()?;
        }

        let start = self.start;
        let end = self.current;
        let text = &self.source[start..end];

        let tok = match text {
            "and" => And,
            "break" => Break,
            "continue" => Continue,
            "else" => Else,
            "false" => False,
            "for" => For,
            "fun" => Fun,
            "if" => If,
            "in" => In,
            "or" => Or,
            "range" => Range,
            "return" => Return,
            "step" => Step,
            "struct" => Struct,
            "true" => True,
            "while" => While,
            _ => Ident,
        };

        self.push_token(tok);
        Ok(())
    }

    fn advance(&mut self) -> Result<char, ScanError> {
        let c = self.getchar(self.current as usize)?;
        self.current += 1;
        Ok(c)
    }

    fn test(&mut self, expected: char) -> Result<bool, ScanError> {
        if self.is_at_end() {
            return Ok(false);
        }
        let c = self.getchar(self.current)?;
        if c != expected {
            return Ok(false);
        }

        self.current += 1;
        Ok(true)
    }

    fn getchar(&mut self, nth: usize) -> Result<char, ScanError> {
        self.source
            .chars()
            .nth(nth)
            .ok_or_else(|| ScanError::raise(self))
    }

    fn peek(&mut self) -> Result<char, ScanError> {
        if self.is_at_end() {
            Ok('\0')
        } else {
            self.getchar(self.current)
        }
    }

    // fn peek_next(&mut self) -> Result<char, ScanError> {
    //     if self.current + 1 >= self.source.len() {
    //         Ok('\0')
    //     } else {
    //         self.getchar(self.current + 1)
    //     }
    // }

    fn is_at_end(&self) -> bool {
        self.current >= self.source.len()
    }

    fn push_token(&mut self, token_type: TokenType) {
        let lexeme = &self.source[self.start..self.current];
        self.push(Token::new(token_type, lexeme, self.start, self.line));
    }

    fn push_type_token(&mut self, ty: Type) {
        let lexeme = &self.source[self.start..self.current];
        self.push(Token::newtype(ty, lexeme, self.start, self.line));
    }

    fn push(&mut self, token: Token) {
        self.tokens.push(token);
    }
}

fn is_digit(c: char) -> bool {
    c >= '0' && c <= '9'
}

fn is_capital(c: char) -> bool {
    c >= 'A' && c <= 'Z'
}

fn is_alpha(c: char) -> bool {
    (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || c == '_'
}

fn is_alphanumeric(c: char) -> bool {
    is_alpha(c) || is_digit(c)
}
