extern crate lalrpop;

fn main() {
    println!("cargo:rerun-if-changed=src/syntax/parser/grammar.lalrpop");

    lalrpop::Configuration::new()
        .always_use_colors()
        .process()
        .unwrap();
}
