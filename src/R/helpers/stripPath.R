stripPath <- function(txt) {
  # IN:  /the/full/path/file.txt
  # OUT: file.txt
  tail(strsplit(txt,"/")[[1]],1)
}
