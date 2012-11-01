linear.mixed.model <- function(y, x) {
  # y is phenotype vector {0,1} or {1,2} = {control, case}
  # x is genotype vector {0,1,2}

  # REQUIRED CONVENTION
  # return output names when function is called with no args
  if (nargs()==0) {  # called with no params
    return(c("ML", "delta", "ve", "vg", "n.individuals", "p.value"))
  }
  
  # select elements with values
  is = !is.na(x) & !is.na(y)  
  x = x[is]
  y = y[is]

  N = as.numeric(sum(is))

  # allow {1,2} instead of {0,1} labels
  if (max(y) > 1) {
    if (max(y)==2) {
      y = y - 1
    } else {
      stop("case-control phenotype must be encoded as {1,0} or {2,1}")
    }
  }

  library(emma)  # require() gives errors with Rhipe

  # due to lexical scoping, K is available in the calling environment
  results = emma.MLE(y, cbind(1, x/2), K)  # returns a list
  
  # emma.MLE does not return a p.value but gwas()
  # requires a p.value for filtering on p.value
  # so we fudge this with a p.value of 2 
  # to provide a clue that it's not real.
  c(results, n.individuals=N, p.value=2)
}
