# reshape the output of gwas.results() for one phenotype
gwas.results.reshape <- function(
  X # data.frame from gwas.results()
) {

  types = unique(X$type)
  row.ids = unique(X[,c("rsid", "chr", "bp")])
  rownames(row.ids) = NULL

  # build new table one col at a time
  Y = NULL
  for (i in seq_along(types)) {
    y = subset(X, type==types[i])[,5]  # col 5 is the first phenotype
    Y = cbind(Y, y)
  }
  colnames(Y) = types

  d = cbind(row.ids, Y)

  d
}

