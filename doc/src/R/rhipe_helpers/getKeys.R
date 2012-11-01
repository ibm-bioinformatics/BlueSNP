getKeys <- function(records) {
  do.call("rbind", lapply(records, "[[", 1)) 
}
