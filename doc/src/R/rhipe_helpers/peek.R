peek <- function(hdfs.path, max=10) {
  rhinit.singleton()
  
  records <<- rhread(hdfs.path, max=max)
  keys <<- getKeys(records)
  values <<- getValues(records)
  key <<- keys[[1]]
  value <<- values[[1]]
  
  names(value)
}
