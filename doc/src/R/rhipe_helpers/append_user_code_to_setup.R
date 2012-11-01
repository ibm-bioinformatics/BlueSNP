append.user.code.to.setup <- function(setup, user.code) {
  if (!is.null(user.code)) {
    text = paste(deparse(setup[[1]]), collapse="\n")
    text = sub("^\\{","", text)
    text = sub("\\}$","", text)
    text = paste(text, "source(\"", stripPath(user.code), "\")\n", sep="")
    text = paste("{", text, sep="\n")
    text = paste(text, "}", sep="\n")
    parse(text=text, srcfile=NULL)
  } else {
    setup
  }
}

