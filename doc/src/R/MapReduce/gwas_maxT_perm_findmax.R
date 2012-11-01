gwas.maxT.perm.findmax <- function(
  input.hdfs.path,
  output.hdfs.path,
  minutes.until.timeout=10
) {

  # ################################################################################
  # step 1: find the max test statistic using many reducers --> sequence output
  # step 2: concat previous ouput into one ouput file --> text output
  # ################################################################################

  rhinit.singleton()

  setup = expression({
    suppressPackageStartupMessages(library(BlueSNP))
  })

  map = expression({
    rhcounter("PROGRESS", "_TASK_", 1)

    for (i in seq_along(map.values)) {  # each snp record
      rhcounter("PROGRESS", "_IN_", 1)
      key = map.keys[[i]]
      value = map.values[[i]]

      statistic.random = value$statistic.random
      for (j in seq_along(statistic.random)) {
        statistic = statistic.random[j]

        # emit
        rhcollect(j, statistic)  # find max(statistic) in the reducer
        rhcounter("PROGRESS", "_OUT_", 1)
      }
    }
  })

  reduce = expression(
    pre={
      rhcounter("PROGRESS", "_TASK_", 1)
      key = reduce.key
      value = NULL
      largest = 0
    },
    reduce={
      for (i in seq_along(reduce.values)) {
        rhcounter("PROGRESS", "_IN_", 1)
        value = reduce.values[[i]]

        if (abs(value) > largest)
          largest = abs(value)
      }
    },
    post={
      rhcollect(key, largest)  # emit one record per iteration
      rhcounter("PROGRESS", "_OUT_", 1)
    }
  )

  inout = c("sequence", "sequence")
  ifolder = input.hdfs.path
  ofolder = paste(output.hdfs.path, "_temp", sep="")
  
  jobname = "gwas.maxT.perm.findmax.step1"
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
      mapred.reduce.tasks=reduce.task.capacity(),
      mapred.task.timeout=ONEMIN*minutes.until.timeout,
      rhipe_map_buffsize=RHIPE_MAP_BUFFSIZE,
      rhipe_reduce_buffsize=RHIPE_REDUCE_BUFFSIZE
    )
  )

  rhex(z, async=F)

  # ##################################################
  # step 2: concat previous ouput into one ouput file --> text output
  # ##################################################    
  
  map = identity.map
  reduce = identity.reduce

  inout = c("sequence", "text")
  ifolder = ofolder
  ofolder = output.hdfs.path

  jobname = "gwas.maxT.perm.findmax.step2"
  z = rhmr(
    jobname=jobname,
    setup=setup, 
    map=map,
    reduce=reduce, 
    inout=inout,
    ifolder=ifolder,
    ofolder=ofolder,
    orderby="integer",
    mapred = list(
      mapred.reduce.tasks=1,
      mapred.task.timeout=ONEMIN*minutes.until.timeout,
      rhipe_map_buffsize=RHIPE_MAP_BUFFSIZE,
      rhipe_reduce_buffsize=RHIPE_REDUCE_BUFFSIZE,
      mapred.textoutputformat.usekey=T,
      mapred.field.separator="\t",
      mapred.textoutputformat.separator="\t"
    )
  )

  rhex(z, async=F)

}

