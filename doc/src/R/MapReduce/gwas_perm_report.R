gwas.perm.report <- function(
  input.hdfs.path,
  output.hdfs.path,
  hit.report.cutoff=5,
  minutes.until.timeout=10
) {

  rhinit.singleton()

  setup = expression({
    suppressPackageStartupMessages(library(BlueSNP))
    hit.report.cutoff = HIT.REPORT.CUTOFF
  })

  setup = expressionBuilder(setup, 
    HIT.REPORT.CUTOFF=deparse(hit.report.cutoff)
  )

  map = expression({
    rhcounter("PROGRESS", "_TASK_", 1)

    for (i in seq_along(map.values)) {  # each snp record
      rhcounter("PROGRESS", "_IN_", 1)
      key = map.keys[[i]]
      value = map.values[[i]]

      # append for easy access
      value$p.value     = pval(value$thecount)
      value$hits        = hits(value$thecount)
      value$tries       = tries(value$thecount)
      value$is.finished = is.finished(value$thecount)

      fields = c(
        "phenotype.name",
        "rsid",
        "chromosome",
        "position",
        "p.value",
        "p.value.adjusted",
        "statistic.real",
        "hits",
        "tries",
        "is.finished"
      )

      new.value = value[fields]
      new.key = value$p.value  # numeric
      
      if (value$hits <= hit.report.cutoff) {  # only report if significant
        rhcollect(new.key, new.value)
        rhcounter("PROGRESS", "_OUT_", 1)
      }
    }
  })
  
  reduce = identity.reduce

  # reduce.cleanup:
  #write.table(paste(column.names, collapse="\t"),
  #  file="./tmp/header.txt",
  #  quote=F,
  #  row.names=F,
  #  col.names=F
  #)

  inout = c("sequence", "text")
  ifolder = input.hdfs.path
  ofolder = output.hdfs.path
  
  jobname = "gwas.perm.report"
  z = rhmr(
    jobname=jobname,
    setup=setup, 
    copyFiles=T,
    map=map,
    reduce=reduce, 
    inout=inout,
    ifolder=ifolder,
    ofolder=ofolder,
    orderby="numeric",
    mapred=list(
      mapred.reduce.tasks=1,
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

