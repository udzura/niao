use combine::{
    stream::{position::DefaultPositioned, ResetStream},
    Positioned, Stream, StreamOnce,
};

#[derive(Debug, Clone)]
pub struct Token {
    pub token_type: TokenType,
    pub lexeme: String,
    pub line: usize,
}

impl PartialEq for Token {
    fn eq(&self, other: &Self) -> bool {
        use TokenType::*;
        match self.token_type {
            Numeric | StringLiteral | Ident | ConstIdent => {
                self.token_type == other.token_type && self.lexeme == other.lexeme
            }
            _ => self.token_type == other.token_type,
        }
    }

    fn ne(&self, other: &Self) -> bool {
        !(self.eq(other))
    }
}

impl Token {
    pub fn new(token_type: TokenType, lexeme: impl Into<String>, line: usize) -> Self {
        Self {
            token_type,
            lexeme: lexeme.into(),
            line,
        }
    }

    pub fn of(token_type: TokenType) -> Self {
        Self {
            token_type,
            lexeme: "".to_string(),
            line: 0,
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

    Eof,
}

#[derive(Debug, Copy, Clone)]
pub struct TokenIndex(pub TokenType, pub usize);

impl PartialEq for TokenIndex {
    fn eq(&self, other: &Self) -> bool {
        self.0 == other.0
    }

    fn ne(&self, other: &Self) -> bool {
        !(self.eq(other))
    }
}
impl Eq for TokenIndex {}

impl PartialOrd for TokenIndex {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        self.1.partial_cmp(&other.1)
    }
}
impl Ord for TokenIndex {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.1.cmp(&other.1)
    }
}

impl Default for TokenIndex {
    fn default() -> Self {
        Self(TokenType::At, Default::default())
    }
}

impl TokenIndex {
    pub fn of(tt: TokenType) -> Self {
        Self(tt, 0)
    }
}

#[derive(PartialEq, PartialOrd, Debug, Clone)]
pub struct TokenStream {
    pub stream: Vec<TokenIndex>,
    pub current: usize,
}

impl Positioned for TokenStream {
    fn position(&self) -> Self::Position {
        self.stream.get(self.current).unwrap().clone()
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
    type Token = TokenIndex;
    type Range = Vec<TokenIndex>;
    type Position = TokenIndex;
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
