qt.linear.regression <- function(y, x) {

  # trick to return output var names when function is called with no params
  if (nargs()==0) {  # called with no params
    return(names(qt.linear.regression(1:10, 2:11)))  # dummy data
  }
  
  # select elements with values
  is = !is.na(x) & !is.na(y)  
  x = x[is]
  y = y[is]

  df2 = sum(is) - 2

  cov.yx = cov(y, x)
  cov.xx = var(x)
  cov.yy = var(y)

  beta = cov.yx / cov.xx
  se = sqrt(cov.yy / cov.xx - cov.yx^2 / cov.xx / cov.xx) / sqrt(df2)
  t.stat = beta / se
  
  r2 = cov.yx^2 / cov.xx / cov.yy

  p = pt(abs(t.stat), df2, lower.tail=F) * 2

  #list(NMISS=sum(is), BETA=beta, SE=se, R2=r2, T=t.stat, P=p)  # plink names
  list(n.individuals=as.numeric(sum(is)), beta=beta, se=se, R2=r2, t.statistic=t.stat, p.value=p)  # BlueSNP names
  # BUG: Rhipe has a problem exporting NA integers, so we convert n.individuals to a numeric type
}
