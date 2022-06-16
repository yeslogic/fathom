/// Command line integration tests, run with the [trycmd] crate.

#[test]
fn cli_tests() {
    std::env::set_current_dir("..").unwrap();
    trycmd::TestCases::new().case("tests/cmd/*.md");
}
