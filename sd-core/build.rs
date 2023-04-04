use std::path::PathBuf;

fn main() {
    println!("cargo:rerun-if-changed=src/language.rs");
    rust_sitter_tool::build_parsers(&PathBuf::from("src/lib.rs"));

    // Uncomment to write grammar
    // let strings = rust_sitter_tool::generate_grammars(&PathBuf::from("src/lib.rs"));
    // std::fs::write("grammar.json", strings.join("\n")).expect("Unable to write file");
}
