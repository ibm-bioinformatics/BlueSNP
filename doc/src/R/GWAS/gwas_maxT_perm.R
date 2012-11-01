gwas.maxT.perm <- function(
  genotype.hdfs.path,             # genotype dir
  phenotype.hdfs.path,            # phenotype Rdata file (must fit in mapper memory)
  output.hdfs.path,               # results dir
  phenotype.cols=1,               # only one phenotype allowed
  method="qt.linear.regression",  # a built-in or custom test (see user.code.hdfs.path option)
  statistic.name="t.statistic",   # result[[statistic.name]]
  n.permutations=1000,            # total number of permutations
  hit.report.cutoff=5,            # only report SNPs with this many or fewer random hits
  user.code=NULL,                 # hdfs path to user-defined assoc tests
  minutes.until.timeout=10        # override hadoop default
) {

  # validations
  if (length(phenotype.cols)!=1)
    stop("we only allow for one phenotype in phenotype.cols when performing permutations")

  output.hdfs.root = output.hdfs.path

  # ##################################################
  # run permutations

  output.hdfs.path = paste(output.hdfs.root, "/run", sep="")
  gwas.perm.run(
    genotype.hdfs.path,
    phenotype.hdfs.path,
    output.hdfs.path,
    method,
    statistic.name,
    phenotype.cols=phenotype.cols,
    is.reduce=T,
    n.permutations=n.permutations,
    user.code=user.code,
    minutes.until.timeout=minutes.until.timeout
  )

  # ##################################################
  # find max test statistic in each round

  input.hdfs.path  = paste(output.hdfs.root, "/run", sep="")
  output.hdfs.path = paste(output.hdfs.root, "/findmax", sep="")
  gwas.maxT.perm.findmax(
    input.hdfs.path,
    output.hdfs.path,
    minutes.until.timeout=minutes.until.timeout
  )

  # ##################################################
  # p.value.adjusted

  input.hdfs.path  = paste(output.hdfs.root, "/run", sep="")
  output.hdfs.path = paste(output.hdfs.root, "/adjust", sep="")
  maxT.hdfs.path   = paste(output.hdfs.root, "/findmax", sep="")
  gwas.maxT.perm.adjust(
    input.hdfs.path,
    output.hdfs.path,
    maxT.hdfs.path,
    minutes.until.timeout=minutes.until.timeout
  )
  
  # ##################################################
  # report

  input.hdfs.path  = paste(output.hdfs.root, "/adjust", sep="")
  output.hdfs.path = paste(output.hdfs.root, "/report", sep="")
  gwas.perm.report(
    input.hdfs.path,
    output.hdfs.path,
    hit.report.cutoff=hit.report.cutoff,
    minutes.until.timeout=minutes.until.timeout
  )

}

