use logos::{Lexer, Logos};

#[derive(Clone, Eq, PartialEq, Debug, Logos)]
pub enum Token {
    #[token("bind")]
    Bind,

    #[token("in")]
    In,

    #[token("=")]
    Equals,

    #[token(",")]
    Comma,

    #[token(".")]
    Period,

    #[token(";")]
    Semicolon,

    #[token("(")]
    LeftParan,

    #[token(")")]
    RightParan,

    #[regex("[a-zA-Z][a-zA-Z0-9_]*", |lex| lex.slice().to_string())]
    Identifier(String),

    #[error]
    #[regex(r"[ \t\n\f]+", logos::skip)]
    Error,
}

#[derive(Clone, Debug)]
pub struct Tokens<'source>(pub Lexer<'source, Token>);
