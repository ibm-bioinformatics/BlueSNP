gwas.maxT.perm.adjust <- function(
  input.hdfs.path,
  output.hdfs.path,
  maxT.hdfs.path,
  minutes.until.timeout=10
) {

  # ################################################################################
  # p.value.adjusted
  # ################################################################################

  rhinit.singleton()

  maxT.hdfs.path = paste(maxT.hdfs.path, "/part-r-00000", sep="")
  
  shared = c(maxT.hdfs.path)

  setup = expression({
    suppressPackageStartupMessages(library(BlueSNP))
    myfile = MAXT_FILE
    maxT = as.numeric(read.table(myfile, header=F, row.names=1)[,1])
  })
  
  setup = expressionBuilder(setup, 
    MAXT_FILE=deparse(stripPath(maxT.hdfs.path))
  )
  
  map = expression({
    rhcounter("PROGRESS", "_TASK_", 1)
    
    for (i in seq_along(map.values)) {  # each snp record
      rhcounter("PROGRESS", "_IN_", 1)
      key = map.keys[[i]]
      value = map.values[[i]]
      
      statistic.real = value$statistic.real
      
      p.value.adjusted = sum(abs(maxT) >= abs(statistic.real)) / length(maxT)
  
      # update snp record
      value$p.value.adjusted = p.value.adjusted
      
      # emit snp record
      rhcollect(key, value)
      rhcounter("PROGRESS", "_OUT_", 1)
    }
  })
  
  inout = c("sequence", "sequence")
  ifolder = input.hdfs.path
  ofolder = output.hdfs.path
  
  jobname = "gwas.maxT.perm.adjust"
  z = rhmr(
    jobname=jobname,
    setup=setup, 
    map=map,
    inout=inout,
    ifolder=ifolder,
    ofolder=ofolder,
    shared=shared,
    mapred = list(
      mapred.reduce.tasks=0,
      mapred.task.timeout=minutes(minutes.until.timeout),
      rhipe_map_buffsize=RHIPE_MAP_BUFFSIZE,
      rhipe_reduce_buffsize=RHIPE_REDUCE_BUFFSIZE
    )
  )
  
  rhex(z, async=F)
  
}

