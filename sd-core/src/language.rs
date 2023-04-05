use std::fmt::{Display, Write};

#[rust_sitter::grammar("sdlanguage")]
pub mod grammar {
    #[rust_sitter::language]
    #[derive(Clone, Debug)]
    pub enum Expr {
        Val(Value),
        Bind(
            #[rust_sitter::leaf(text = "bind")] (),
            Variable,
            #[rust_sitter::leaf(text = "=")] (),
            Term,
            #[rust_sitter::leaf(text = "in")] (),
            Box<Expr>,
        ),
    }

    #[derive(Clone, Debug)]
    pub enum Term {
        Val(Value),
        ActiveOp(
            ActiveOp,
            #[rust_sitter::leaf(text = "(")] (),
            #[rust_sitter::delimited(
                #[rust_sitter::leaf(text = ",")]
                ()
            )]
            Vec<Value>,
            #[rust_sitter::leaf(text = ")")] (),
        ),
        Thunk(Box<Thunk>),
    }

    #[derive(Clone, Debug)]
    pub enum Value {
        Var(Variable),
        PassiveOp(
            PassiveOp,
            #[rust_sitter::leaf(text = "(")] (),
            #[rust_sitter::delimited(
                #[rust_sitter::leaf(text = ",")]
                ()
            )]
            Vec<Value>,
            #[rust_sitter::leaf(text = ")")] (),
        ),
    }

    #[allow(clippy::manual_non_exhaustive)]
    #[derive(Clone, Debug)]
    pub struct Thunk {
        pub args: Vec<Variable>,
        #[rust_sitter::leaf(text = ".")]
        _dot: (),
        pub body: Expr,
    }

    #[derive(Clone, Copy, Debug, Eq, PartialEq)]
    pub enum ActiveOp {
        Plus(#[rust_sitter::leaf(text = "+")] ()),
        Times(#[rust_sitter::leaf(text = "*")] ()),
    }

    #[derive(Clone, Copy, Debug, Eq, PartialEq)]
    pub enum PassiveOp {
        Int(#[rust_sitter::leaf(pattern = r"\d+", transform = |v| v.parse().unwrap())] usize),
    }

    #[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
    pub struct Variable {
        #[rust_sitter::leaf(pattern = r"[a-zA-Z][a-zA-Z0-9_]*", transform = |v| v.to_string())]
        pub var: String,
    }

    #[rust_sitter::extra]
    struct Whitespace {
        #[rust_sitter::leaf(pattern = r"\s")]
        _whitespace: (),
    }
}

impl Display for grammar::ActiveOp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Plus(()) => f.write_char('+'),
            Self::Times(()) => f.write_char('x'),
        }
    }
}

impl Display for grammar::PassiveOp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Int(n) => f.write_str(&n.to_string()),
        }
    }
}

pub const HIGHLIGHT_QUERY: &str = include_str!("../../highlights.scm");

use tree_sitter_highlight::{HighlightConfiguration, Highlighter};

pub fn highlighter_config() -> HighlightConfiguration {
    let highlight_names = &[
        "keyword",
        "operator",
        "variable",
        "punctuation.bracket",
        "punctuation.delimiter",
    ];

    let mut config = HighlightConfiguration::new(
        crate::language::grammar::language(),
        HIGHLIGHT_QUERY,
        "",
        "",
    )
    .unwrap();
    config.configure(highlight_names);
    config
}

pub fn highlight(
    config: &HighlightConfiguration,
    source: &str,
) -> Vec<tree_sitter_highlight::HighlightEvent> {
    Highlighter::new()
        .highlight(config, source.as_bytes(), None, |_| None)
        .unwrap()
        .collect::<Result<Vec<_>, _>>()
        .unwrap()
}
