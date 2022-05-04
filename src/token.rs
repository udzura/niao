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

    pub fn stub(token_type: TokenType) -> Self {
        Self {
            token_type,
            lexeme: "".to_string(),
            line: 0,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
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
