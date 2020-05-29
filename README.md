# Tractus
Tractus can analyze dependencies in R code and parse through source code to identify experiments and group them into hypotheses. It has three components: An RStudio addin, a parser, and a web app that visualizes the results of the parser. Please find more details including the publication on the [Tractus homepage](https://hci.rwth-aachen.de/tractus).

![](https://github.com/i10/tractus/blob/master/interface-blank-full.jpg)

# Usage
## Installation
Currently, we do not provide prebuilt binaries. To build Tractus yourself, clone this repository, install [Rust](https://www.rust-lang.org/) using [rustup](https://rustup.rs/). Then, from the folder where this README file is, run the following bash command:
```
cargo build --release
```
The Tractus binary will be available at `./target/release/tractus`. You can copy it to a location you find convenient, and add it to your `PATH` so that it is available as `tractus`. Note: The following instructions assume you have done this and that the binary is available in the terminal as `tractus`.

To install the RStudio addin, open the `./src/rstudio-addin/tractusAddin.Rproj` in RStudio and execute the menu entry `Build > Load All`. The addin should then be available in the toolbar under `Addins` and called `Tractus`.

## Quickstart
To watch the RStudio console, start Tractus in a terminal:
```
tractus serve
```

The above command will watch the R session in a non-persistent manner, i.e., only program statements executed in the current R session will be parsed and visualized. If you want to use Tractus in a persistent manner, use the following command instead:

```
tractus serve  --store ./tractus
```

For more options, check out the command-line help for Tractus:

```
tractus --help
```

Then start the Tractus addin in RStudio by selecting in in the toolbar under `Addins > Tractus`.
> The addin might not be able to find RStudio's `history_database` file. If you see the corresponding error, find out where it is on your computer by reading [this RStudio Support document](https://support.rstudio.com/hc/en-us/articles/200534577-Resetting-RStudio-Desktop-s-State). Then, in RStudio, set the global variable `historyDatabaseOverride` to that path, by executing in the console:
> ```
> historyDatabaseOverride <- "<path to history_database>"
> ```

Starting the addin will open the job pane, but you can switch back to the console and begin executing code there. (We cannot instruct RStudio to do this automatically.) Note that the visualization shown in the viewer loads its dependencies from the web, so an internet connection is required.

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
Here we explain how you can integrate new tools into Tractus. This requires how the RStudio addin interacts with Tractus.

When Tractus is started using `tractus serve`, it starts a websocket server that listens on `ws://127.0.0.1:2794`. The RStudio addin starts a websocket client that connects to that address.

Whenever the RStudio addin detects that a new statement was executed in the console, it sends that statement along with its execution result to Tractus as a JSON payload:
```
{
    "statement": "print(\"Hello, World!\"",
    "meta": { "result": "[1] Hello, World!" }
}
```

We refer to this as a hypothesis tree. Since Tractus keeps the `meta` field associated to the statement, it will be available in the hypothesis tree, where the visualization can access it.

The `meta` field can be filled with whatever data you like (e.g., a special tag by the user), and you can provide a custom visualization that uses your custom `meta` information. In this way, you can extend Tractus to work in a variety of environments.

## Testing
```
cargo test
```

[insta](https://docs.rs/insta/) is used for snapshot testing and is installed with `cargo install cargo-insta`. To [update the snapshots](https://docs.rs/insta/0.8.2/insta/#snapshot-updating), run `cargo insta test --review`.

# Limitations
We developed Tractus to work for most R files, but not all. Also, there are some hard limitations imposed by the RStudio environment. This means, Tractus has some caveats:

- Tractus currently does not capture graphic output. This is because RStudio does not provide an easy way to access graphic outputs. Doing this would require storing the output on the user's hard disk, retrieving it, and displaying it in the web app.
- The parser does not correctly handle R code with the following properties:
  - With `serve`, multi-line statements like typical `if`-`else` statements are not correctly detected.
  - Comments nested inside of statements are not supported. Only proper tail comments are supported. (E.g., `if (isOk()) # sanity check {...` is not supported.)
  - Nesting `if`-statements too deeply works, but degrades performance immensely.
  - Tractus detects a few ways of subsetting and selecting data, but not all ways. This is a tradeoff we made to be able to build the app in a reasonable time, and used the results of the parser validation to identify the prominent ways to subset and select data.
- The websocket server may occasionally not close connections completely, and might be inefficient.
- RStudio does not offer access to the console, so the addin detects executed statements via the `history_database` file which might not always work correctly. E.g., if there's a second RStudio, it won't always detect all the R code in the session.
- Navigating the R code using the visualization, i.e., clicking on a node in the visualization to select the corresponding statement in R script file, is not possible in this version of Tractus. This is because (a) selecting statements in an R script file that is open on RStudio requires using rstudioapi via the RStudio addin, (b) the RStudio addin should be run asynchronously for it to not block the R session, and (c) we cannot run the RStudio addin asynchronously because it needs to listen to the websocket for input.
  - In the previous version of Tractus, attached as a supplement, navigation is possible, but the R session is blocked when the addin is running. 
- Code blocks are detected correctly when the comment is followed by one or more lines of code. But when a new line is interspersed into the lines of code, or between the comment and the lines of code, the detection is faulty.
- Tractus visualizes blocks even if the entire block is commented out, since it currently does not detect if the entire block is a block comment for the next block or if it just code that is commented out.
- RStudio's viewer pane has certain limitations: It does not support autocomplete of HTML text boxes and doesn't allow copying text to clipboard.
- We also made a design decision for the visualization in Tractus: In situations where there are multiple parent dependencies, we pick the chronologically recent parent to retain a tree structure. A directed acyclic graph reflects the multiple dependencies more precisely, but our tree representation is simpler, and reflects the source code more precisely.
