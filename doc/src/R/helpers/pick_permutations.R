pick.permutations <- function(n) {
  if (n <= 1E6)
    3000
  else if (n <= 1E7)
    6000
  else if (n <= 1E8)
    10000
}
