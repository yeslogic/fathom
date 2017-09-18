extern crate lalrpop;

fn main() {
    println!("cargo:rerun-if-changed=src/parser/grammar.lalrpop");

    lalrpop::Configuration::new()
        .always_use_colors()
        .process()
        .unwrap();
}
