pick.copies <- function(n) {
  if (n <= 1E6)
    100
  else if (n <= 1E7)
    500
  else if (n <= 1E8)
    1000
}
