tractusAddin <- function() {
  file.copy(file.path(path.package("tractusAddin"), "inst/vis.html"), file.path(tempdir(), "vis.html"), overwrite = TRUE)
  viewer <- getOption("viewer")
  viewer(file.path(tempdir(), "vis.html"))

  library(rstudioapi)
  jobRunScript(file.path(path.package("tractusAddin"), "inst/watcher.R"), name = "Tractus", importEnv = TRUE)
}
