# calls Rhips::rhinit() if needed and sets hdfs.setwd()
rhinit.singleton <- function(...) {
  require(Rhipe)
  if (!exists("Rhipe.is.initialized")) {
    cat("[BlueSNP] Initializing a new Rhipe connection\n")
    rhinit(...)
    Rhipe.is.initialized <<- T
  } else {
    cat("[BlueSNP] Reusing existing Rhipe connection\n")
    Rhipe.is.initialized
  }
  hdfs.setwd(paste("/user", Sys.getenv("USER"), sep="/"))
}
