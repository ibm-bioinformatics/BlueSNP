gwas <- function(
  genotype.hdfs.path,             # hdfs path to genotype dir
  phenotype.hdfs.path,            # hdfs path to phenotype Rdata file (must fit in memory)
  output.hdfs.path,               # hdfs path to results dir
  method="qt.linear.regression",  # a built-in or custom test
  phenotype.cols=NULL,            # subset of phenotype matrix col name/number
  pvalue.report.cutoff=1E-5,      # only report p-values less than this
  split.factor=1,                 # integer > 1 decreases the number of splits
  is.sparse.output=F,             # don't create a txt report
  user.code=NULL,                 # hdfs path to user-defined assoc tests
  minutes.until.timeout=10        # override hadoop default
) {

  mapred.min.split.size=128*1024*1024*split.factor

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
  
  # ##################################################
  # MAP: Test SNP i for association with phenotype j
  # ##################################################

  map = expression({
    method = METHOD                               # variable substitution
    pvalue.report.cutoff = PVALUE.REPORT.CUTOFF   # variable substitution
    
    # trick to get the names of the assoc test output
    output.variable.names = eval(call(method))

    rhcounter("PROGRESS", "_TASK_", 1)

    for (i in seq_along(map.values)) {  # each snp record
      rhcounter("PROGRESS", "_IN_", 1)
      key = map.keys[[i]]
      value = map.values[[i]]

      chromosome = value$chromosome
      rsid       = value$rsid
      distance   = value$distance
      position   = value$position
      x          = value$snp.vector

      for (j in phenotype.cols) {  # each phenotype

        rhstatus(paste("SNP", i, "Phenotype", j))  # keepalive

        y = Y[,j]

        # statistical hypothesis test
        result = eval(call(method, y, x))

        if (result$p.value <= pvalue.report.cutoff) {  # only report if significant

          for (k in seq_along(output.variable.names)) {  # one report for each output variable
            statistic.name = output.variable.names[k]
            
            # collect all results for this rsid/stat compound key
            new.key = c(rsid, statistic.name)  # key like c("rs123", "p.value")

            new.value = list(
              chromosome=chromosome,
              rsid=rsid,
              distance = value$distance,
              position=position,
              statistic.name=statistic.name,
              statistic.number=k,
              statistic.value=result[[statistic.name]],
              phenotype.name=phenotype.names[j],
              phenotype.number=j,
              total.phenotype.count=length(phenotype.cols)  # tell reduce() output vector length
            )

            rhcollect(new.key, new.value)
            rhcounter("PROGRESS", "_OUT_", 1)
          }
        }
      }
    }
  })

  map = expressionBuilder(map, 
    METHOD=deparse(method),
    PVALUE.REPORT.CUTOFF=deparse(pvalue.report.cutoff)
  )

  # ##################################################
  # REDUCE: Reduce on key like c("rsid123", "p.value")
  # to collect all phenotypes for a compound key
  # ##################################################

  reduce = expression(
    pre={
      rhcounter("PROGRESS", "_TASK_", 1)
      key = reduce.key
      collected.results = NULL  # collect one line of results
    },
    reduce={
      for (i in seq_along(reduce.values)) {
        rhcounter("PROGRESS", "_IN_", 1)
        value = reduce.values[[i]]

        # these values are the same for every record in this reduce task
        chromosome            = value$chromosome
        rsid                  = value$rsid
        position              = value$position
        statistic.name        = value$statistic.name
        total.phenotype.count = value$total.phenotype.count

        # start to build the output, add collected.results in post={}
        new.value = list(
          statistic.name=statistic.name,
          rsid=rsid,
          chromosome=chromosome,
          position=position
        )

        # these values are specific to this record
        phenotype.number = value$phenotype.number
        phenotype.name   = value$phenotype.name
        statistic.value  = value$statistic.value

        # keepalive
        rhstatus(paste("Stat", statistic.name, "SNP", rsid, "Pheno", phenotype.number))

        # build the row: put statistic.value in the correct position
        collected.results[phenotype.number] = statistic.value
      }
    },
    post={
      # ensure collected results is the correct length
      tmp = as.numeric(rep(NA, total.phenotype.count))
      idx = which(!is.na(collected.results))
      tmp[idx] = collected.results[idx]
      collected.results = tmp
      
      #collected.results[is.na(collected.results)] = ""  # replace NA with "" (cosmetic)

      #new.value = c(new.value, collected.results=collected.results)
      new.value$collected.results = collected.results

      # emit one table row (could belong to any table)
      rhcollect(key, new.value)
      rhcounter("PROGRESS", "_OUT_", 1)
    }
  )
  
  # ##################################################

  inout = c("sequence", "sequence")
  ifolder = genotype.hdfs.path

  ofolder = NULL
  mapred.reduce.tasks=reduce.task.capacity()
  if (is.sparse.output) {
    ofolder = paste(output.hdfs.path, "/sparse", sep="")
    mapred.reduce.tasks = 0
  } else {
    ofolder = paste(output.hdfs.path, "/run", sep="")
  }

  jobname = "gwas.compute"
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
    partitioner=list(lims=c(1,2), type="string"),  # key like c("rs123", "p.value")
    mapred=list(
      mapred.map.tasks=map.task.capacity(),
      mapred.reduce.tasks=mapred.reduce.tasks,
      mapred.task.timeout=ONEMIN*minutes.until.timeout,
      rhipe_map_buffsize=RHIPE_MAP_BUFFSIZE,
      rhipe_reduce_buffsize=RHIPE_REDUCE_BUFFSIZE,
      mapred.min.split.size=mapred.min.split.size
    )
  )
  
  rhex(z, async=F)
  
  # At this point, each output row is assembled, but the rows are not yet
  # grouped by rsid. Do this by partitioning on rsid in another MR.

  if (!is.sparse.output) {

    # ##################################################
    # sort / report
    # ##################################################
    
    map = identity.map
    reduce = identity.reduce
    
    inout = c("sequence", "text")
    ifolder = paste(output.hdfs.path, "/run", sep="")
    ofolder = paste(output.hdfs.path, "/report", sep="")
    
    jobname = "gwas.report"
    z = rhmr(
      jobname=jobname,
      map=map,
      reduce=reduce,
      inout=inout,
      ifolder=ifolder,
      ofolder=ofolder,
      orderby="character",
      partitioner=list(lims=1, type="string"),  # partition on rsid
      mapred=list(
        mapred.map.tasks=map.task.capacity(),
        mapred.reduce.tasks=reduce.task.capacity(),
        mapred.task.timeout=ONEMIN*minutes.until.timeout,
        rhipe_map_buffsize=RHIPE_MAP_BUFFSIZE,
        rhipe_reduce_buffsize=RHIPE_REDUCE_BUFFSIZE,
        mapred.textoutputformat.usekey=F,
        mapred.field.separator="\t",
        mapred.textoutputformat.separator="\t"
      )
    )
    
    rhex(z, async=F)
  
  }

  #reduce.cleanup = expression({
  #  # write the col headers of the output format to the local fs and copy to hdfs
  #  header = paste(c("type", "rsid", "chromosome", "position", phenotype.names), collapse="\t")
  #  write.table(header,
  #    file="./tmp/header.txt",
  #    quote=F,
  #    row.names=F,
  #    col.names=F
  #  )
  #})
  

  # NOTE: reduce could be done with tall.to.wide() helper function
}

