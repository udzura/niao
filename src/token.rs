use combine::{stream::ResetStream, Positioned, StreamOnce};

use crate::types::Type;

#[derive(Debug, Clone)]
pub struct Token {
    pub token_type: TokenType,
    pub lexeme: String,
    pub pos: usize,
    pub line: usize,
    pub ty: Option<Type>,
}

impl PartialEq for Token {
    fn eq(&self, other: &Self) -> bool {
        self.token_type == other.token_type
    }

    fn ne(&self, other: &Self) -> bool {
        !(self.eq(other))
    }
}
impl Eq for Token {}

impl PartialOrd for Token {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        self.pos.partial_cmp(&other.pos)
    }
}
impl Ord for Token {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.pos.cmp(&other.pos)
    }
}

impl Default for Token {
    fn default() -> Self {
        Self::new(TokenType::Undefined, "", 0, 0)
    }
}

impl Token {
    pub fn new(token_type: TokenType, lexeme: impl Into<String>, pos: usize, line: usize) -> Self {
        Self {
            token_type,
            lexeme: lexeme.into(),
            pos,
            line,
            ty: None,
        }
    }

    pub fn newtype(ty: Type, lexeme: impl Into<String>, pos: usize, line: usize) -> Self {
        Self {
            token_type: TokenType::Ty,
            lexeme: lexeme.into(),
            pos,
            line,
            ty: Some(ty),
        }
    }

    pub fn of(token_type: TokenType) -> Self {
        Self {
            token_type,
            ..Default::default()
        }
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum TokenType {
    // Punct
    At,
    Define,
    Assign,
    Plus,
    Minus,
    Aster,
    Slash,
    Less,
    Greater,
    Eql,
    Neql,
    Bang,
    Amp,
    LParen,
    RParen,
    LBrace,
    RBrace,
    LDoubleBrace,
    RDoubleBrace,
    Dot,
    Comma,

    // Keyword
    And,
    Or,
    True,
    False,
    If,
    Else,
    For,
    Struct,
    Fun,
    In,
    Step,
    Range,
    While,
    Return,
    Continue,
    Break,

    // Literal
    Numeric,
    StringLiteral,
    Ident,
    ConstIdent,

    // Types
    Ty,

    Eof,

    Undefined,
}

#[derive(PartialEq, PartialOrd, Debug, Clone)]
pub struct TokenStream {
    pub stream: Vec<Token>,
}

impl Positioned for TokenStream {
    fn position(&self) -> Self::Position {
        self.stream
            .first()
            .unwrap_or(&Token::of(TokenType::Undefined))
            .clone()
    }
}

impl ResetStream for TokenStream {
    type Checkpoint = Self;

    fn checkpoint(&self) -> Self::Checkpoint {
        self.clone()
    }

    fn reset(&mut self, checkpoint: Self::Checkpoint) -> Result<(), Self::Error> {
        *self = checkpoint;
        Ok(())
    }
}

impl<'a> StreamOnce for TokenStream {
    type Token = Token;
    type Range = Vec<Token>;
    type Position = Token;
    type Error = combine::error::UnexpectedParse;

    fn is_partial(&self) -> bool {
        false
    }

    fn uncons(&mut self) -> Result<Self::Token, combine::stream::StreamErrorFor<Self>> {
        match self.stream.split_first() {
            Some((first, rest)) => {
                let ret = first.clone();
                let rest = rest.to_vec();
                self.stream.splice(.., rest);
                Ok(ret)
            }
            None => {
                return Err(combine::error::UnexpectedParse::Eoi);
            }
        }
    }
}
