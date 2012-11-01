cc.recessive <- function(y, x) {
  # y is phenotype vector {0,1} = {control,case} or {1,2} = {control,case}
  # x is genotype {0,1,2} = number of copies minor allele

  # trick to return output var names when function is called with no params
  if (nargs()==0) {  # called with no params
    y = sample(0:1, 100, replace=T)  # dummy data
    x = sample(0:2, 100, replace=T)
    return(names(cc.recessive(y, x)))
  }

  # select elements with values
  is = !is.na(x) & !is.na(y)  
  x = x[is]
  y = y[is]

  # accept {1,2} instead of {0,1} labels
  if (max(y) > 1) {
    if (max(y)==2) {
      y = y - 1
    } else {
      stop("case-control phenotype must be encoded as {1,0} or {2,1}")
    }
  }

  x = as.factor(x)
  levels(x) = c(0,1,2)

  con = table(y, x)
  m = cbind(con[,1]+con[,2], con[,3])

  t     = NA
  chisq = NA
  pval  = NA
  df    = NA

  tryCatch({
    t = chisq.test(m, correct=F)
    chisq = as.numeric(t$statistic)
    pval = as.numeric(t$p.value)
    df = 1
  }, warning = function(w) {
    t = NA
    chisq = NA
    df = NA
    pval = NA
  })

  list(chi.sq=chisq, df=df, p.value=pval)
}
