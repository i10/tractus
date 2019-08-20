# Tractus
Dependency analyzer for R code that visualizes the decisions taken during exploratory programming.

# Usage
Tractus reads R source files and outputs a hypotheses tree as a JSON file.
```bash
tractus --input <path_to_r_source> --output <path_to_store_json_at>
```

You can also watch the input file for changes and automatically output an updated tree.
```bash
tractus watch --input <path_to_r_source> --output <path_to_store_json_at>
```

We also have special support for RStudio's `history_desktop` file. You can find information on how to locate it in [RStudio Support](https://support.rstudio.com/hc/en-us/articles/200534577-Resetting-RStudio-Desktop-s-State). With this you can automatically analyze what you type into RStudio's console with tractus.
```bash
tractus watch --input <path_to_r_source> --output <path_to_store_json_at> --history
```

Actually, the `--history` flag just tells Tractus that the file will only be appended to and to filter out the timestamp in front. You can customize this behavior even without the `--history` flag, by using the `--append` and `--clean` flags yourself.

For more information, you can always open the help.
```bash
tractus help
```

# Development
## Testing
```
cargo test
```

[insta](https://docs.rs/insta/) is used for snapshot testing and is installed with `cargo install cargo-insta`. To [update the snapshots](https://docs.rs/insta/0.8.2/insta/#snapshot-updating), run `cargo insta test --review`.