extern crate tractus;

use std::fs;
use std::io::prelude::*;
use std::path;

use insta::assert_debug_snapshot_matches;

#[test]
fn snapshots() {
    let snapshot_dir = get_snapshot_dir("tests/snapshots");
    test_snapshots_in(&snapshot_dir, None).unwrap_or_else(|e| {
        panic!(
            "Snapshot directory `{}` could not be read: {}",
            snapshot_dir.display(),
            e
        )
    });
}

#[test]
#[ignore]
fn hidden_snapshots() {
    test_snapshots_in(&get_snapshot_dir("tests/snapshots/hidden"), Some("hidden"))
        .unwrap_or_default(); // Is allowed to fail when directory is missing.
}

fn get_snapshot_dir<P: std::convert::AsRef<std::path::Path>>(relative_path: P) -> path::PathBuf {
    path::Path::new(env!("CARGO_MANIFEST_DIR")).join(relative_path)
}

fn test_snapshots_in(
    snapshot_dir: &path::PathBuf,
    maybe_prefix: Option<&'static str>,
) -> Result<(), std::io::Error> {
    let snapshot_files: Vec<path::PathBuf> = fs::read_dir(&snapshot_dir)?
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

    for snapshot_file in snapshot_files {
        let result = std::panic::catch_unwind(|| test_snapshot(&snapshot_file, maybe_prefix));
        if result.is_err() {
            panic!("File {} failed.", snapshot_file.display())
        }
    }

    Ok(())
}

fn test_snapshot(snapshot_path: &path::PathBuf, maybe_prefix: Option<&'static str>) {
    let mut file = fs::File::open(&snapshot_path).unwrap();
    let mut code = String::new();
    file.read_to_string(&mut code).unwrap();

    let parsed = tractus::parser::parse_statements(&code)
        .unwrap_or_else(|e| panic!("Parsing failed on file {}: {}", snapshot_path.display(), e));
    let file_stem = snapshot_path
        .as_path()
        .file_stem()
        .unwrap()
        .to_string_lossy();
    let snapshot_name = maybe_prefix
        .map(|prefix| format!("{}-{}", prefix, file_stem))
        .unwrap_or_else(|| file_stem.into_owned());
    assert_debug_snapshot_matches!(format!("{}-parsed", snapshot_name), parsed);
    let dependency_graph = tractus::DependencyGraph::from_input(&parsed);
    let tree = 
        tractus::parse_hypothesis_tree(&parsed, &dependency_graph);
    assert_debug_snapshot_matches!(
        format!("{}-dependencies", snapshot_name),
        tractus::DisplayTree::with(&tree, &mut |stmt_id| parsed[stmt_id].clone())
    );
}
