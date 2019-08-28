# Tractus
Dependency analyzer for R code that visualizes the decisions taken during exploratory programming.

# Quickstart
Listen to websocket input:
```
tractus serve --store ./tractus
```

Watch an RStudio `history_database` file:
```
tractus serve --input ~/.rstudio-desktop/history_database
```

# Usage
`tractus serve` will listen to `ws://127.0.0.1:2794` under the protocol `tractus-websocket`. Whenever the hypotheses tree is updated, it will push a message on all connected websockets in JSON form.

If you run `tractus serve --input <path>`, it will watch the file at that path. Whenever that file is saved to, all connected websockets will receive the updated hypotheses tree. If you watch an RStudio `history_database` file (info on how to locate it is at [RStudio Support](https://support.rstudio.com/hc/en-us/articles/200534577-Resetting-RStudio-Desktop-s-State)), then you can specify so with `--history-database`.

If you do not specify an `--input`, i. e. run simply `tractus serve`, then you can also push messages to the websocket at `ws://127.0.0.1:2794` of the form:
```
{
    "statement": "<some R statement>",
    "meta": <whatever you want>
}
```
The `meta` information will be available in the output hypotheses tree for all statements in `statement` exactly as it was given to tractus. This way you can push line by line with additional information that you can have access to in the output.

In the `tractus serve` mode without `--input`, when stopping tractus all information is lost. To save tractus's state, you can specify an output file with `tractus serve --store <path>`. All changes will be written to that file. If there is already something written in that file, when you start tractus with `tractus serve --store <path>` it will load the state that was saved at path. (Note that if that file was not created by tractus, it might abort due to illegal content.)

# Development
## Testing
```
cargo test
```

[insta](https://docs.rs/insta/) is used for snapshot testing and is installed with `cargo install cargo-insta`. To [update the snapshots](https://docs.rs/insta/0.8.2/insta/#snapshot-updating), run `cargo insta test --review`.