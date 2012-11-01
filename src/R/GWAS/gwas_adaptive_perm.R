gwas.adaptive.perm <- function(
  genotype.hdfs.path,             # genotype dir
  phenotype.hdfs.path,            # phenotype Rdata file (must fit in mapper memory)
  output.hdfs.path,               # results dir
  phenotype.cols=1,               # only one phenotype allowed
  method="qt.linear.regression",  # a built-in or custom test (see user.code.hdfs.path option)
  statistic.name="t.statistic",   # result[[statistic.name]]
  hit.report.cutoff=10,            # only report SNPs with this many or fewer random hits
  user.code=NULL,                 # hdfs path to user-defined assoc tests
  minutes.until.timeout=10,       # override hadoop default
  n.permutations=1E6,             # total number of permutations
  # expert settings for adding more permutations to an output folder
  k=0,
  total.tries=0,
  n.not.finished=1,  # a small number greater than zero
  previous.n.not.finished=Inf
) {

  rhinit.singleton()

  # validations
  if (length(phenotype.cols)!=1)
    stop("we only allow for one phenotype in phenotype.cols when performing permutations")

  # initialize
  output.hdfs.root = output.hdfs.path
  is.not.finished = T
  
  if (k==0) {  # this is a new job
    k = k + 1
    total.tries = 1E5
    output.hdfs.path = paste(output.hdfs.root, "/run/", k, sep="")
    j = gwas.perm.run(
      genotype.hdfs.path,
      phenotype.hdfs.path,
      output.hdfs.path,
      method=method,
      statistic.name=statistic.name,
      phenotype.cols=phenotype.cols,
      is.maxT=F,
      is.premature.stop=T,
      is.report.all.stats=F,
      is.reduce=T,
      n.permutations=total.tries,
      user.code=user.code,
      minutes.until.timeout=minutes.until.timeout
    )
    n.not.finished = j$counters$PROGRESS["_IS_NOT_FINISHED_"]
    is.not.finished = n.not.finished > 0
  }
  
  # ##################################################
  # main loop

  # helpers to pick reasonable numbers of copies and permutations per copy
  n.copies = pick.copies(total.tries)
  n.permutations.block = pick.permutations(total.tries)

  while (is.not.finished & total.tries < n.permutations) {

    if (n.not.finished < previous.n.not.finished) {  # the number of not-finished-snps decreased
      # re-pick the number of copies and permutations
      n.copies = pick.copies(total.tries)
      n.permutations.block = pick.permutations(total.tries)
      # broadcast
      genotype.hdfs.path = paste(output.hdfs.root, "/run/", k, sep="")
      output.hdfs.path   = paste(output.hdfs.root, "/broadcast/", k, sep="")
      j = gwas.perm.broadcast(genotype.hdfs.path, output.hdfs.path, n.copies=n.copies)
      n.not.finished = j$counters$PROGRESS["_IS_NOT_FINISHED_"]
      
    } else {
      # move previous broadcast to new location (copy and delete)
      old.hdfs.path = paste(output.hdfs.root, "/broadcast/", k-1, sep="")
      new.hdfs.path = paste(output.hdfs.root, "/broadcast/", k, sep="")
      rhcp(old.hdfs.path, new.hdfs.path)
      rhdel(old.hdfs.path)
    }

    # pick a reasonable number of reducers for gwas.perm.run() below
    mapred.reduce.tasks = min(n.not.finished, reduce.task.capacity())

    # run permutations
    genotype.hdfs.path = paste(output.hdfs.root, "/broadcast/", k, sep="")
    output.hdfs.path   = paste(output.hdfs.root, "/run/", k+1, sep="")
    j = gwas.perm.run(
      genotype.hdfs.path,
      phenotype.hdfs.path,
      output.hdfs.path,
      method=method,
      statistic.name=statistic.name,
      phenotype.cols=phenotype.cols,
      is.maxT=F,
      is.premature.stop=T,
      is.report.all.stats=F,
      is.reduce=T,
      n.permutations=n.permutations.block,
      mapred.reduce.tasks=mapred.reduce.tasks,
      user.code=NULL,
      minutes.until.timeout=10,
      job.tag=k+1
    )
    previous.n.not.finished = n.not.finished  # remember previous number of candidate SNPs
    n.not.finished = j$counters$PROGRESS["_IS_NOT_FINISHED_"]
    is.not.finished = n.not.finished > 0

    total.tries = total.tries + n.copies * n.permutations.block
    k = k + 1
  }

  # collect
  input.hdfs.path  = paste(output.hdfs.root, "/run", sep="")
  output.hdfs.path = paste(output.hdfs.root, "/collect", sep="")
  gwas.perm.collect(input.hdfs.path, output.hdfs.path)
  
  # report
  input.hdfs.path = paste(output.hdfs.root, "/collect", sep="")
  output.hdfs.path = paste(output.hdfs.root, "/report", sep="")
  gwas.perm.report(input.hdfs.path, output.hdfs.path, hit.report.cutoff=hit.report.cutoff)

}

