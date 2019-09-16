library(websocket)
library(foreach)
library(stringr)

read <- function() {
    # Read the current history database
    history <- read.delim("~/.rstudio-desktop/history_database", header = FALSE, sep = "\r", quote=NULL)
    history$V1
}

history <- read()
nextLine <<- length(history) + 1

update <- function() {
    history <- tryCatch(read(), error={return})

    if (nextLine <= length(history)) {
        for(index in c(nextLine:(length(history)))) {
            # Send the lines of code along with output
            statement = str_remove(history[index], "^\\d+:")

            # Capture result
            # Do not run install.packages statements
            isPackageInstallation = str_detect(statement, "install.packages") | str_detect(statement, "install()")
            isPluginExecution = str_detect(statement, "executeCodeAddin")

            if (isPackageInstallation | isPluginExecution) {
                result = NULL
            } else {
                result = try(capture.output(eval(parse(text = statement))))
                print(result)
            }

            # Escape the quotes
            statement = str_replace_all(statement, '"', '\\\\"')

            # Format: statement: { x <- 1 }, meta = { results: { 1 } }
            if (is.null(result)) {
                JSONToSend = paste('{"statement": "', statement, '", "meta": { "result": {} } }', sep = "")
            } else {
                output = paste(result, collapse = '\\n')
                output = str_replace_all(output, '"', '\\\\"')
                JSONToSend = paste('{"statement": "', statement, '", "meta": { "result": \"', output,'\" } }', sep = "")
            }

            # Send to websocket
            if (!isPluginExecution) {
                print("Sending to websocket.")
                print(JSONToSend)
                socket$send(JSONToSend)
            }
            nextLine <<- index + 1
        }
    }
}

socket <- WebSocket$new("ws://127.0.0.1:2794", protocols = "tractus-websocket")

socket$onOpen(function(event) {
    cat("Connection opened\n")
})

socket$onClose(function(event) {
    cat("Client disconnected with code ", event$code,
        " and reason ", event$reason, "\n", sep = "")
    stop()
})

socket$onError(function(event) {
    cat("Client failed to connect: ", event$message, "\n")
    stop()
})

poll_until_connected <- function(ws, timeout = 5) {
  connected <- FALSE
  end <- Sys.time() + timeout
  while (!connected && Sys.time() < end) {
    # Need to run the event loop for websocket to complete connection.
    later::run_now(0.1)

    ready_state <- ws$readyState()
    if (ready_state == 0L) {
      # 0 means we're still trying to connect.
      # For debugging, indicate how many times we've done this.
      cat(".")
    } else if (ready_state == 1L) {
      connected <- TRUE
    } else {
      break
    }
  }

  if (!connected) {
    stop("Unable to establish websocket connection.")
  }
}
poll_until_connected(socket)

while (TRUE) {
    update()
    later::run_now(0.5)
    Sys.sleep(0.5)
}
