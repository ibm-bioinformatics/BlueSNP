append.user.code.to.shared <- function(shared, user.code) {
  if (!is.null(user.code)) {
    c(shared, user.code)
  } else {
    shared
  }
}
