extern crate tractus;

use std::fs::File;
use std::io::prelude::*;

use insta::assert_debug_snapshot_matches;

use tractus::{parse, parse_dependency_graph};

#[test]
fn parses_keyboard() {
    let mut file = File::open(concat!(
        env!("CARGO_MANIFEST_DIR"),
        "/tests/snapshots/keyboard.R"
    ))
    .unwrap();
    let mut code = String::new();
    file.read_to_string(&mut code).unwrap();

    let parsed = parse(&code).expect("Parsing should not fail.");
    assert_debug_snapshot_matches!(parsed);
    assert_debug_snapshot_matches!(parse_dependency_graph(&parsed).graph()); // The map's order is not guaranteed, leading to false failures.
}
