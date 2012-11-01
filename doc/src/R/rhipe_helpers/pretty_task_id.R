pretty.task.id <- function(prefix="") {
  x = Sys.getenv("mapred.task.id")
  n = strsplit(x, "_")[[1]][5]
  paste(prefix, n, sep="")
}
