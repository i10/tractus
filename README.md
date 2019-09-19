# Tractus
Dependency analyzer for R code that visualizes the decisions taken during exploratory programming.

> Please note that this project is a research protoype. We focused on showcasing a broad range of features instead of fleshing out individual features. Nonetheless, we expect Tractus to work for common analyses.

# Usage
## Installation
Currently, we do not provide prebuilt binaries. To build Tractus yourself, clone this repository, install [Rust](https://www.rust-lang.org/) using [rustup](https://rustup.rs/), and then from inside the folder of this README run:
```
cargo build --release
```
The Tractus binary will be available at `./target/release/tractus`. You can copy it to a location of your pleasing and add it to your `PATH` such that it is available as simply `tractus`, as the following instructions assume.

To install the RStudio addin, open the `./src/rstudio-addin/tractusAddin.Rproj` in RStudio and execute the menu entry `Build > Load All`. The addin should be available in the toolbar under `Addins` and called `Tractus`.

## Quickstart
To watch the RStudio console, start Tractus in a terminal:
```
tractus serve --store ./tractus
```

Then start the Tractus addin in RStudio by selecting in in the toolbar under `Addins > Tractus`.
> The addin might not be able to find RStudio's `history_database` file. If you see the corresponding error, find out where it is on your computer by reading [this RStudio Support document](https://support.rstudio.com/hc/en-us/articles/200534577-Resetting-RStudio-Desktop-s-State). Then, in RStudio, set the global variable `historyDatabaseOverride` to that path, by executing in the console:
> ```
> historyDatabaseOverride <- "<path to history_database>"
> ```

Starting the addin will open the job pane, but you can switch back to the console and begin executing code there. Note that the visualization shown in the viewer loads its dependencies from the web, so an internet connection is required.

> If you want to restart Tractus, make sure that the addin's old job is stopped, by going to the job pane's overview. (In case you only see the addin's output, then click on the back arrow in the top left to get to the overview, where you can stop the old job.)

Alternatively, you can tell Tractus to watch a file by executing in a terminal:
```
tractus serve --input <path_to_file>
```

The visualization can again be shown in RStudio via the addin (but console executions will be ignored, since Tractus visualizes a specific file). If you prefer working without RStudio, it can also be shown in an external browser by opening the visualization file at `./src/rstudio-addin/inst/vis.html`.

## Overview
Running `tractus serve --input <path>` will watch the file at that path. When the visualization is opened, it automatically connects to the server started by that command. Whenever that file is saved to, the visualization will receive the new hypotheses tree and update itself.

In addition to the `serve` subcommand, Tractus also can work with file-based input and output via the `run` subcommand. For example, executing `tractus run --input <path>` will read that file and output the hypothesis tree on stdout. This allows integrating Tractus with other systems that cannot use websockets.

Further information with extended detail is available by running `tractus help`.

# Development
## Integrate with Tractus
To explain how to integrate new tools with Tractus, we explain how the provided RStudio addin interacts with Tractus.

When Tractus is started using `tractus serve`, it starts a websocket server that listens on `ws://127.0.0.1:2794`. The RStudio addin starts a websocket client that connects to that address.

Whenever the RStudio addin detects that a new statement was executed in the console, it sends that statement along with its execution result to Tractus as a JSON payload:
```
{
    "statement": "print(\"Hello, World!\"",
    "meta": { "result": "[1] Hello, World!" }
}
```
Since Tractus keeps the `meta` field associated to the statement, it will be available in the hypothesis tree, where the visualization can access it.

The `meta` field can be filled with whatever data you like, and you can provide a custom visualization that uses your custom `meta` information. In this way, you can extend Tractus to work in a variety of environments.

## Testing
```
cargo test
```

[insta](https://docs.rs/insta/) is used for snapshot testing and is installed with `cargo install cargo-insta`. To [update the snapshots](https://docs.rs/insta/0.8.2/insta/#snapshot-updating), run `cargo insta test --review`.

# Limitations
As a research prototype, unfortuntely Tractus currently comes with some caveats.

- The custom R parser does not correctly handle all possible R code.
  - With `serve`, multi-line statements like typical `if`-`else` statements are not correctly detected.
  - Comments nested inside of statements are not supported. Only proper tail comments are supported. (E. g. `if (isOk()) # sanity check {...`)
  - Nesting `if`-statements too deeply degrades performance considerably.
- The websocket server does not close connections properly and might be inefficient.
- RStudio does not offer access to the console, so the addin detects executed statements via the `history_database` file which might not always work correctly.
- There is a bug in code block detection, where newlines do not create a new block.