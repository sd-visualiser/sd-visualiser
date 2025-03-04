use dir_test::{Fixture, dir_test};
use sd_core::{
    language::sd_lang::Expr,
    lp::Solver,
    monoidal::{graph::MonoidalGraph, wired_graph::from_graph},
};
use sd_graphics::{layout::layout, render::generate_shapes, shape::Shapes};
use svg::Document;

fn to_svg(expr: &Expr) -> Document {
    let solver = Solver::default();
    let graph = expr.to_graph(false).unwrap();
    let monoidal_term = from_graph(&graph, solver);
    let monoidal_graph = MonoidalGraph::from(&monoidal_term);
    let layout = layout(&monoidal_graph, solver).unwrap();
    let mut shapes = Vec::new();
    generate_shapes(&mut shapes, &layout, true);
    let shapes = Shapes {
        shapes,
        size: layout.size(),
    };
    shapes.to_svg()
}

#[dir_test(dir: "$CARGO_MANIFEST_DIR/../examples", glob: "**/with*.sd", loader: sd_core::language::sd_lang::parse_sd_lang, postfix: "external_inputs")]
fn external_inputs(fixture: Fixture<(&str, Expr)>) {
    let (name, expr) = fixture.content();
    let svg = to_svg(expr);
    insta::assert_binary_snapshot!(
        &format!("external_inputs_{name}.svg"),
        svg.to_string().into(),
        &svg.to_string()
    );
}
