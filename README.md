# Tractus
Dependency analyzer for R code that visualizes the decisions taken during exploratory programming.

# Usage
`tractus serve` will listen to `ws://127.0.0.1:2794` under the protocol `tractus-websocket`. Whenever the hypotheses tree is updated, it will push a message on all connected websockets in JSON form.

If you run `tractus serve --input <path>`, it will watch the file at that path. Whenever that file is saved to, all connected websockets will receive the updated hypotheses tree.

If you do not specify an `--input`, i. e. `tractus serve`, then you can also push messages to the websocket at `ws://127.0.0.1:2794` of the form:
```
{
    "statement": "<some R statement>",
    "meta": <whatever you want>
}
```
The `meta` information will be available in the output hypotheses tree for all statements in `statement` exactly as it was given to tractus. This way you can push line by line with additional information that you can have access to in the output.

In the `tractus serve` mode without `--input`, when stopping tractus all information is lost. To save tractus's state, you can specify an output file with `tractus serve --output <path>`. All changes will be written to that file. If there is already something written in that file, when you start tractus with `tractus serve --output <path>` it will load the state that was saved at path. (Note that if that file is not written by tractus, it might abort.)

# Development
## Testing
```
cargo test
```

[insta](https://docs.rs/insta/) is used for snapshot testing and is installed with `cargo install cargo-insta`. To [update the snapshots](https://docs.rs/insta/0.8.2/insta/#snapshot-updating), run `cargo insta test --review`.