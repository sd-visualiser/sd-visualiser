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
            Vec<Variable>,
            #[rust_sitter::leaf(text = ";")] (),
            #[rust_sitter::delimited(
                #[rust_sitter::leaf(text = ",")]
                ()
            )]
            Vec<Thunk>,
            #[rust_sitter::leaf(text = ")")] (),
        ),
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
            Vec<Variable>,
            #[rust_sitter::leaf(text = ";")] (),
            #[rust_sitter::delimited(
                #[rust_sitter::leaf(text = ",")]
                ()
            )]
            Vec<Thunk>,
            #[rust_sitter::leaf(text = ")")] (),
        ),
    }

    #[derive(Clone, Debug)]
    pub struct Thunk {
        pub args: Vec<Variable>,
        #[rust_sitter::leaf(text = ".")]
        _dot: (),
        pub body: Expr,
    }

    #[derive(Clone, Debug)]
    pub enum ActiveOp {
        Plus(#[rust_sitter::leaf(text = "+")] ()),
        Times(#[rust_sitter::leaf(text = "*")] ()),
    }

    #[derive(Clone, Debug)]
    pub enum PassiveOp {
        Int(#[rust_sitter::leaf(pattern = r"\d+", transform = |v| v.parse().unwrap())] usize),
    }

    #[derive(Clone, Debug)]
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
