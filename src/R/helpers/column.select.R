column.select <- function(
  X,         # matrix or data.frame
  cols=NULL  # column numbers or names
) {
  
  if (is.null(cols)) {
    return(X)  # nothing to select
  }
  
  if (is.null(ncol(X))) {
    if (cols!=1) {  # a vector input has one col
      stop("Invalid column number specified for vector input")
    }
    return(as.matrix(X, ncol=1))  # nothing to select, reformat at matrix
  }
  
  if (is.numeric(cols)) {
    invalid.cols = setdiff(cols, 1:ncol(X))
    if (length(invalid.cols)) {
      stop(paste("Invalid column numbers specified:", paste(invalid.cols, sep=", ")))
    }
    return(as.matrix(X[,cols], ncols=length(cols)))
  }
  
  if (is.character(cols)) {
    invalid.cols = setdiff(cols, colnames(X))
    if (length(invalid.cols)) {
      stop(paste("Invalid column names specified:", paste(invalid.cols, sep=", ")))
    }
    return(as.matrix(X[,cols], ncols=length(cols)))
  }
  
  # should never get this far
  stop("There was a problem selecting columns by name/number.")
}
