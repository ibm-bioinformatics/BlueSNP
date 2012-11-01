gwas.perm.broadcast <- function(
  genotype.hdfs.path,
  output.hdfs.path,
  n.copies=200,
  minutes.until.timeout=10
) {

  rhinit.singleton()

  setup = expression({
    suppressPackageStartupMessages(library(BlueSNP))
    n.copies = N_COPIES
  })

  setup = expressionBuilder(setup, 
    N_COPIES=deparse(n.copies)
  )

  map = expression({
    rhcounter("PROGRESS", "_TASK_", 1)

    for (i in seq_along(map.values)) {  # each snp record
      rhcounter("PROGRESS", "_IN_", 1)
      key = map.keys[[i]]
      value = map.values[[i]]
      
      rsid = value$rsid
      
      # put the counter in broadcast mode
      value$thecount = broadcast(value$thecount)
      
      if (!is.finished(value$thecount)) {
        rhcounter("PROGRESS", "_IS_NOT_FINISHED_", 1)
        
        for (j in 1:n.copies) {  # broadcast
          rhcollect(j, value)  # key on copy number 1, 2, ..., n.copies
          rhcounter("PROGRESS", "_OUT_", 1)
        }
      }
    }
  })

  # write N copies of the same output file
  reduce = expression(
    pre={
      rhcounter("PROGRESS", "_TASK_", 1)
      rhstatus(paste("Working on key:", reduce.key))
    },
    reduce={
      for (i in seq_along(reduce.values)) {
        rhcounter("PROGRESS", "_IN_", 1)
        value = reduce.values[[i]]
        rsid = value$rsid
        rhcollect(rsid, value)  # key on rsid
        rhcounter("PROGRESS", "_OUT_", 1)
      }
    }
  )

  inout = c("sequence", "sequence")
  ifolder = genotype.hdfs.path
  ofolder = output.hdfs.path
  
  jobname = "gwas.perm.broadcast"
  z = rhmr(
    jobname=jobname,
    setup=setup, 
    map=map,
    reduce=reduce, 
    inout=inout,
    ifolder=ifolder,
    ofolder=ofolder,
    orderby="integer",
    partitioner=list(lims=1, type="integer"),
    mapred = list(
      mapred.reduce.tasks=n.copies,
      mapred.task.timeout=ONEMIN*minutes.until.timeout,
      rhipe_map_buffsize=RHIPE_MAP_BUFFSIZE,
      rhipe_reduce_buffsize=RHIPE_REDUCE_BUFFSIZE
    )
  )

  rhex(z, async=F)

}

