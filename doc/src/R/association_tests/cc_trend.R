cc.trend <- function(y, x) {
  # y is phenotype vector {0,1} = {control,case} or {1,2} = {control,case}
  # x is genotype {0,1,2} = number of copies minor allele

  # trick to return output var names when function is called with no params
  if (nargs()==0) {  # called with no params
    y = sample(0:1, 100, replace=T)  # dummy data
    x = sample(0:2, 100, replace=T)
    return(names(cc.trend(y, x)))
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

  t = c(0, 1, 2)
  con = table(y, x)

  R = apply(con, 1, sum)
  C = apply(con, 2, sum)
  N = sum(R)
  Tstat = R[2]*sum(t*con[1,])-R[1]*sum(t*con[2,])
  VAR = R[1]*R[2]/N*(sum(t^2*C*(N-C))-2*(t[1]*t[2]*C[1]*C[2]+t[1]*t[3]*C[1]*C[3]+t[2]*t[3]*C[2]*C[3]))
  chisq = Tstat^2 / VAR
  pval = pchisq(chisq, 1, low=F)
  
  names(chisq) = NULL
  names(pval) = NULL

  list(chi.sq=chisq, df=1, p.value=pval)
}
