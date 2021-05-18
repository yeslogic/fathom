fn main() {
    let args = libtest_mimic::Arguments::from_args();

    std::env::set_current_dir("..").unwrap();

    let tests = std::iter::empty()
        .chain(fathom_test::find_fathom_files("examples").map(fathom_test::simple_test))
        .chain(fathom_test::find_fathom_files("tests").map(fathom_test::full_test))
        .collect();
    let run_test = fathom_test::run_test(env!("CARGO_BIN_EXE_fathom"));

    libtest_mimic::run_tests(&args, tests, run_test).exit();
}
