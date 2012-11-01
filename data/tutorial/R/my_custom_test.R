my.custom.test <- function(y, x) {
  # This is an example of a user-defined statistical test
  # PARAMETERS
  #   y is phenotype vector {0,1} = {control,case}
  #   x is genotype {0,1,2} = number of copies minor allele
  # OUTPUT
  #   list of values

  # ignore missing (NA) values
  is = !is.na(x) & !is.na(y)  
  x = x[is]
  y = y[is]

  # check for PLINK simulated {2,1} instead of {1,0} labels
  if (max(y) > 1) {
    if (max(y)==2) {
      y = y - 1
    } else {
      stop("case-control phenotype must be encoded as {1,0} or {2,1}")
    }
  }

  N = sum(is)
  stat = cor(y, x)^2 * (N - 2)

  list(n.individuals=as.numeric(N), stat=stat)
}
