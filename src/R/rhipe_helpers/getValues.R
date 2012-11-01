getValues <- function(records) {
  lapply(records, "[[", 2)
}
