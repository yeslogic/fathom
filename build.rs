extern crate lalrpop;

fn main() {
    println!("cargo:rerun-if-changed=src/parser/grammar.lalrpop");
    lalrpop::process_root().unwrap();
}
