extern crate tractus;

use std::fs;
use std::io::prelude::*;

use insta::assert_debug_snapshot_matches;

use tractus::Tractus;

#[test]
fn snapshots() {
    let snapshot_dir = concat!(env!("CARGO_MANIFEST_DIR"), "/tests/snapshots");
    let snapshot_files = fs::read_dir(snapshot_dir)
        .unwrap()
        .filter_map(|maybe_entry| {
            maybe_entry
                .map(|entry| {
                    let path = entry.path();
                    if path.extension()? == "R" {
                        Some(path)
                    } else {
                        None
                    }
                })
                .unwrap_or(None)
        });
    for snapshot_path in snapshot_files {
        let mut file = fs::File::open(&snapshot_path).unwrap();
        let mut code = String::new();
        file.read_to_string(&mut code).unwrap();

        let parsed =
            Tractus::parse(&code).unwrap_or_else(|e| panic!("Parsing should not fail: {}", e));
        let file_stem = snapshot_path
            .as_path()
            .file_stem()
            .unwrap()
            .to_string_lossy();
        assert_debug_snapshot_matches!(format!("{}-parsed", file_stem), parsed);
        assert_debug_snapshot_matches!(
            format!("{}-dependencies", file_stem),
            parsed.generate_hypothesis_tree()
        );
    }
}
