extern crate tractus;

use std::fs;
use std::io::prelude::*;
use std::path;

use insta::assert_debug_snapshot_matches;

use tractus::Tractus;

#[test]
fn snapshots() {
    let snapshot_dirs = ["tests/snapshots", "tests/snapshots/hidden"]
        .iter()
        .map(get_dir);

    let snapshot_files: Vec<path::PathBuf> = snapshot_dirs
        .filter_map(|snapshot_dir| fs::read_dir(snapshot_dir).ok())
        .flatten()
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
        })
        .collect();
    assert!(!snapshot_files.is_empty(), "No snapshot files were found!");

    for snapshot_path in snapshot_files {
        let mut file = fs::File::open(&snapshot_path).unwrap();
        let mut code = String::new();
        file.read_to_string(&mut code).unwrap();

        let parsed = Tractus::parse(&code).unwrap_or_else(|e| {
            panic!("Parsing failed on file {}: {}", snapshot_path.display(), e)
        });
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

fn get_dir<P: std::convert::AsRef<std::path::Path>>(relative_path: P) -> path::PathBuf {
    path::Path::new(env!("CARGO_MANIFEST_DIR")).join(relative_path)
}
