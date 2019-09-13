executeCodeAddin <- function() {
  # install.packages("websocket")
  library(websocket)
  library(foreach)
  library(stringr)

  # Read the current history database
  history <- read.delim("~/.rstudio-desktop/history_database", header = FALSE, sep = "\r", quote=NULL)
  history <- history$V1

  socket <- WebSocket$new("ws://127.0.0.1:2794", protocols = "tractus-websocket", autoConnect = FALSE)

  socket$onOpen(function(event) {
    cat("Connection opened\n")
    if (!exists("lastSent")) {
      lastSent <<- 1
    }

    cat("lastSent = ", lastSent, "\n")
    for(index in c(lastSent:length(history))) {
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
      }

      # Escape the quotes
      statement = str_replace_all(statement, '"', '\\\\"')

      # Format: statement: { x <- 1 }, meta = { results: { 1 } }
      if (is.null(result)) {
        JSONToSend = paste('{"statement": "', statement, '", "meta": { "result": {} } }', sep = "")
      } else {
        output = paste(result, collapse = '')
        output = str_replace_all(output, '"', '\\\\"')
        JSONToSend = paste('{"statement": "', statement, '", "meta": { "result": \"', output,'\" } }', sep = "")
      }

      # Send to websocket
      if (!isPluginExecution) {
        socket$send(JSONToSend)
      }

      lastSent <<- index
    }

    file.copy("~/Code/tractus/src/vis.html", file.path(tempdir(), "vis.html"))
    viewer <- getOption("viewer")
    viewer(file.path(tempdir(), "vis"))

    # socket$close()
  })

  socket$onMessage(function(event) {
    # cat("Client got msg: ", event$data, "\n")
  })

  socket$onClose(function(event) {
    cat("Client disconnected with code ", event$code,
        " and reason ", event$reason, "\n", sep = "")
  })

  socket$onError(function(event) {
    cat("Client failed to connect: ", event$message, "\n")
  })

  socket$connect()
}
