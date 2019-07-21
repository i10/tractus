# Tractus
[![Build Status](https://travis-ci.com/Y0hy0h/tractus.svg?branch=master)](https://travis-ci.com/Y0hy0h/tractus)

Dependency analyzer for R code that visualizes the decisions taken during exploratory programming.

# Development
The following cargo extensions are recommended:
- [`cargo-edit`](https://github.com/killercup/cargo-edit) for managing dependencies
- [`insta`](https://docs.rs/insta/) for snapshot management

## Testing
```
cargo test
```
To [update the snapshots](https://docs.rs/insta/0.8.2/insta/#snapshot-updating), run `cargo insta review`.