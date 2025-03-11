use std::collections::HashMap;

use dir_test::{Fixture, dir_test};
use sd_core::{
    common::Direction,
    examples::{self, DummyCtx, DummyEdge, DummyOperation},
    language::{Expr, Language, llvm_ir::parse_llvm_ir, sd_lang::parse_sd_lang},
    lp::Solver,
    monoidal::{
        MonoidalTerm, Slice,
        graph::{MonoidalGraph, MonoidalOp},
        wired_graph::from_graph,
    },
};
use sd_graphics::{layout::layout, render::generate_shapes, shape::Shapes};
use svg::Document;

fn to_svg<T: Language + 'static>(expr: &Expr<T>) -> Document {
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

#[dir_test(dir: "$CARGO_MANIFEST_DIR/../examples", glob: "**/with*.sd", loader: parse_sd_lang, postfix: "external_inputs")]
#[dir_test(dir: "$CARGO_MANIFEST_DIR/../examples", glob: "llvm-ir/bug-97.ll", loader: parse_llvm_ir)]
fn svg_test<T: Language + 'static>(fixture: Fixture<(&str, Expr<T>)>) {
    let (name, expr) = fixture.content();
    let svg = to_svg(expr);
    insta::assert_binary_snapshot!(
        &format!("{name}.svg"),
        svg.to_string().into(),
        &svg.to_string()
    );
}

#[test]
fn examples() {
    let terms: HashMap<&str, MonoidalTerm<DummyCtx, _>> = [
        (
            "id",
            MonoidalGraph {
                free_inputs: vec![],
                bound_inputs: vec![DummyEdge],
                slices: vec![],
                free_outputs: vec![],
                bound_outputs: vec![DummyEdge],
            },
        ),
        (
            "double_id",
            MonoidalGraph {
                free_inputs: vec![],
                bound_inputs: vec![DummyEdge; 2],
                slices: vec![],
                free_outputs: vec![],
                bound_outputs: vec![DummyEdge; 2],
            },
        ),
        (
            "copy",
            MonoidalGraph {
                free_inputs: vec![],
                bound_inputs: vec![DummyEdge],
                slices: vec![Slice {
                    ops: vec![MonoidalOp::Copy {
                        addr: DummyEdge,
                        copies: 2,
                    }],
                }],
                free_outputs: vec![],
                bound_outputs: vec![DummyEdge; 2],
            },
        ),
        (
            "delete",
            MonoidalGraph {
                free_inputs: vec![],
                bound_inputs: vec![DummyEdge],
                slices: vec![Slice {
                    ops: vec![MonoidalOp::Copy {
                        addr: DummyEdge,
                        copies: 0,
                    }],
                }],
                free_outputs: vec![],
                bound_outputs: vec![],
            },
        ),
        (
            "swap",
            MonoidalGraph {
                free_inputs: vec![],
                bound_inputs: vec![DummyEdge; 2],
                slices: vec![Slice {
                    ops: vec![MonoidalOp::Swap {
                        addrs: vec![
                            (DummyEdge, Direction::Forward),
                            (DummyEdge, Direction::Forward),
                        ],
                        out_to_in: vec![1, 0],
                    }],
                }],
                free_outputs: vec![],
                bound_outputs: vec![DummyEdge; 2],
            },
        ),
        (
            "double_swap",
            MonoidalGraph {
                free_inputs: vec![],
                bound_inputs: vec![DummyEdge; 2],
                slices: vec![
                    Slice {
                        ops: vec![MonoidalOp::Swap {
                            addrs: vec![
                                (DummyEdge, Direction::Forward),
                                (DummyEdge, Direction::Forward),
                            ],
                            out_to_in: vec![1, 0],
                        }],
                    },
                    Slice {
                        ops: vec![MonoidalOp::Swap {
                            addrs: vec![
                                (DummyEdge, Direction::Forward),
                                (DummyEdge, Direction::Forward),
                            ],
                            out_to_in: vec![1, 0],
                        }],
                    },
                ],
                free_outputs: vec![],
                bound_outputs: vec![DummyEdge; 2],
            },
        ),
        ("int", examples::int()),
        ("copy_left_assoc", examples::copy()),
        (
            "copy_right_assoc",
            MonoidalGraph {
                free_inputs: vec![],
                bound_inputs: vec![DummyEdge],
                slices: vec![
                    Slice {
                        ops: vec![MonoidalOp::Copy {
                            addr: DummyEdge,
                            copies: 2,
                        }],
                    },
                    Slice {
                        ops: vec![
                            MonoidalOp::Copy {
                                addr: DummyEdge,
                                copies: 1,
                            },
                            MonoidalOp::Copy {
                                addr: DummyEdge,
                                copies: 2,
                            },
                        ],
                    },
                ],
                free_outputs: vec![],
                bound_outputs: vec![DummyEdge; 3],
            },
        ),
        (
            "copy_left_unit",
            MonoidalGraph {
                free_inputs: vec![],
                bound_inputs: vec![DummyEdge],
                slices: vec![
                    Slice {
                        ops: vec![MonoidalOp::Copy {
                            addr: DummyEdge,
                            copies: 2,
                        }],
                    },
                    Slice {
                        ops: vec![
                            MonoidalOp::Copy {
                                addr: DummyEdge,
                                copies: 0,
                            },
                            MonoidalOp::Copy {
                                addr: DummyEdge,
                                copies: 1,
                            },
                        ],
                    },
                ],
                free_outputs: vec![],
                bound_outputs: vec![DummyEdge; 1],
            },
        ),
        (
            "copy_right_unit",
            MonoidalGraph {
                free_inputs: vec![],
                bound_inputs: vec![DummyEdge],
                slices: vec![
                    Slice {
                        ops: vec![MonoidalOp::Copy {
                            addr: DummyEdge,
                            copies: 2,
                        }],
                    },
                    Slice {
                        ops: vec![
                            MonoidalOp::Copy {
                                addr: DummyEdge,
                                copies: 1,
                            },
                            MonoidalOp::Copy {
                                addr: DummyEdge,
                                copies: 0,
                            },
                        ],
                    },
                ],
                free_outputs: vec![],
                bound_outputs: vec![DummyEdge; 1],
            },
        ),
        ("thunk", examples::thunk()),
    ]
    .into_iter()
    .collect();
    for (name, term) in &terms {
        let layout = layout(term, Solver::default()).unwrap();
        let mut shapes = Vec::new();
        generate_shapes(&mut shapes, &layout, true);
        let shapes = Shapes {
            shapes,
            size: layout.size(),
        };
        let svg = shapes.to_svg();
        insta::assert_binary_snapshot!(
            &format!("{name}.svg"),
            svg.to_string().into(),
            &svg.to_string()
        );
    }
}
