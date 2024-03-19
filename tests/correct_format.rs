use std::fs;

#[test]
fn no_changes() {
    let config = typstfmt::Config {
        indent_space: 2,
        max_line_length: 80,
        experimental_args_breaking_consecutive: false,
        line_wrap: false,
    };
    for path in fs::read_dir("tests/no_changes/").unwrap() {
        let path = path.unwrap().path();
        println!("testing {}...", path.display());
        let file = fs::read_to_string(path).unwrap();

        let result = typstfmt::format(&file, config);

        similar_asserts::assert_eq!(file, result);
    }
}
