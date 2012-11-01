gwas.perm.run <- function(
  genotype.hdfs.path,                        # hdfs path to genotype dir
  phenotype.hdfs.path,                       # hdfs path to phenotype Rdata file (must fit in memory)
  output.hdfs.path,                          # hdfs path to results dir
  method="qt.linear.regression",             # a built-in or custom test
  statistic.name="t.statistic",              # result[[statistic.name]]
  phenotype.cols=1,                          # subset of phenotype matrix col name/number
  is.maxT=T,                                 # default is maxT mode
  is.premature.stop=F,                       # default for maxT is to NOT do a premature stop
  is.report.all.stats=T,                     # default for maxT is to report all test stats
  is.reduce=T,                               # reduce unnecessarry when SNP records not replicated
  n.permutations=1000,
  user.code=NULL,                            # hdfs path to user-defined assoc tests
  mapred.reduce.tasks=reduce.task.capacity(),  # might want fewer
  minutes.until.timeout=10,                  # override hadoop default
  job.tag=NULL
) {

  # WARNING: the value of random.seed is crucial for maxT and adaptive permutation
  # to work properly
  random.seed = NULL  # the seed must be NULL for adaptive
  if (is.maxT)
    random.seed=sample(999999999, 1)  # the seed must take a value for maxT

  # ################################################################################
  # Run the permutations, emit updated SNP records (without snp.vector to save space)
  # ################################################################################

  if (length(phenotype.cols)!=1)
    stop("we only allow for one phenotype in phenotype.cols when performing permutations")

  rhinit.singleton()
  
  # at mapper startup, these files are copied from the hdfs to the mapper local fs at ./tmp/
  shared = append.user.code.to.shared(phenotype.hdfs.path, user.code)

  setup = expression({
    suppressPackageStartupMessages(library(BlueSNP))
  
    load(PHENOTYPE_FILE)             # variable substitution
    phenotype.cols = PHENOTYPE.COLS  # variable substitution
    
    # restrict Y to just the columns we are interested in
    # WARNING: output column number no longer corresponds to input column number
    Y = column.select(Y, phenotype.cols)
    phenotype.cols = 1:ncol(Y)  # redefine. overwrites NULL
    
    # phenotype.names default to colnames(), fallback to col numbers 
    # WARNING: phenotype.names[j] is the jth entry of the selected cols
    phenotype.names = colnames(Y)
    if (is.null(phenotype.names))
      phenotype.names = phenotype.cols
  })
  
  # expressionBuilder() performs variable substitution on ALL_CAPS
  setup = expressionBuilder(setup, 
    PHENOTYPE_FILE=deparse(stripPath(phenotype.hdfs.path)),
    PHENOTYPE.COLS=deparse(phenotype.cols)
  )
  setup = append.user.code.to.setup(setup, user.code)

  # ################################################################################

  map = expression({
    method              = METHOD               # variable substitution
    statistic.name      = STATISTIC.NAME       # variable substitution
    n.permutations      = N.PERMUTATIONS       # variable substitution
    random.seed         = RANDOM.SEED          # variable substitution
    is.premature.stop   = IS.PREMATURE.STOP    # variable substitution
    is.report.all.stats = IS.REPORT.ALL.STATS  # variable substitution
    
    rhcounter("PROGRESS", "_TASK_", 1)
    
    for (i in seq_along(map.values)) {  # each snp record
      rhcounter("PROGRESS", "_IN_", 1)
      key = map.keys[[i]]
      value = map.values[[i]]

      if (is.null(value$thecount))  # initialize the counter if it is absent (first time)
        value$thecount = PermutationCounter()

      if (is.finished(value$thecount)) {  # skip this record if it is finished
        # this check lets us broadcast fewer times
        rhcollect(key, value)
        rhcounter("PROGRESS", "_OUT_", 1)
        next
      }
      # if we made it this far, this record is not finished

      chromosome = value$chromosome
      rsid       = value$rsid
      distance   = value$distance
      position   = value$position
      x          = value$snp.vector

      statistic.real = eval(call(method, Y, x))[[statistic.name]]

      result = permutation.loop(
        thecount=value$thecount,
        method, 
        statistic.name, 
        Y, 
        x, 
        statistic.real, 
        n.permutations, 
        keepalive=i, 
        random.seed=random.seed, 
        is.premature.stop=is.premature.stop,
        is.report.all.stats=is.report.all.stats
      )

      new.value = value
      new.value$thecount       = value$thecount + result$thecount  # value$thecount in broadcast mode
      new.value$statistic.real = statistic.real
      if (is.report.all.stats)
        new.value$statistic.random = result$statistic.random
      new.value$phenotype.name = phenotype.names  # should be just one phenotype name

      # emit snp record
      rhcollect(rsid, new.value)
      rhcounter("PROGRESS", "_OUT_", 1)
    }
  })

  map = expressionBuilder(map, 
    METHOD=deparse(method),
    STATISTIC.NAME=deparse(statistic.name),
    N.PERMUTATIONS=deparse(n.permutations),
    RANDOM.SEED=deparse(random.seed),
    IS.PREMATURE.STOP=deparse(is.premature.stop),
    IS.REPORT.ALL.STATS=deparse(is.report.all.stats)
  )

  # ################################################################################

  reduce.setup = expression({
    suppressPackageStartupMessages(library(BlueSNP))
  })

  reduce = reduce.collect

  # ################################################################################
  
  if (!is.reduce)
    mapred.reduce.tasks=0

  inout = c("sequence", "sequence")
  ifolder = genotype.hdfs.path
  ofolder = output.hdfs.path

  jobname = "gwas.perm.run"
  if (!is.null(job.tag))
    jobname = paste(jobname, job.tag, sep="-")
  
  # new setup format
  setup = as.expression(list(map=setup[[1]], reduce=reduce.setup[[1]]))
  
  z = rhmr(
    jobname=jobname,
    setup=setup,
    map=map,
    reduce=reduce,
    inout=inout,
    ifolder=ifolder,
    ofolder=ofolder,
    shared=shared,
    orderby="character",
    partitioner=list(lims=1, type="string"),
    mapred = list(
      mapred.reduce.tasks=mapred.reduce.tasks,
      mapred.task.timeout=ONEMIN*minutes.until.timeout,
      rhipe_map_buffsize=RHIPE_MAP_BUFFSIZE,
      rhipe_reduce_buffsize=RHIPE_REDUCE_BUFFSIZE
    )
  )
    
  rhex(z, async=F)

}

