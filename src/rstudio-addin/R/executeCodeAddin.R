executeCodeAddin <- function() {
file.copy("~/Code/tractus/src/vis.html", file.path(tempdir(), "vis.html"), overwrite = TRUE)
  viewer <- getOption("viewer")
  viewer(file.path(tempdir(), "vis.html"))

  library(rstudioapi)
  jobRunScript("~/Code/tractus/src/rstudio-addin/scripts/watcher.R", name = "Tractus", importEnv = TRUE)
}