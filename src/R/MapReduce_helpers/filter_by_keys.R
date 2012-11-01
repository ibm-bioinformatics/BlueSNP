filter.by.keys <- function(
  ifolder, 
  ofolder,
  mykeys, # c("key1", "key2", ...)
  inout=c("sequence", "sequence"),
  split.factor=1
) {

  mapred.min.split.size=128*1024*1024*split.factor

  rhinit.singleton()

  setup = expression({
    suppressPackageStartupMessages(library(BlueSNP))
    mykeys = MYKEYS
  })

  setup = expressionBuilder(setup, 
    MYKEYS=paste(deparse(mykeys), collapse="")  # TODO: paste() required for vector
  )

  map = expression({
    rhcounter("PROGRESS", "_TASK_", 1)
    for (i in seq_along(map.values)) {
      rhcounter("PROGRESS", "_IN_", 1)
      key = map.keys[[i]]
      value = map.values[[i]]

      if (sum(key==mykeys)) {  # key is on the list
        rhcollect(key, value)
        rhcounter("PROGRESS", "_OUT_", 1)
      }
    }
  })
  
  reduce = identity.reduce

  jobname = "filter.by.keys"
  z = rhmr(
    jobname=jobname,
    setup=setup,
    map=map,
    reduce=reduce,
    inout=inout,
    ifolder=ifolder,
    ofolder=ofolder,
    shared=shared,
    mapred = list(
      mapred.map.tasks=map.task.capacity(),
      mapred.reduce.tasks=reduce.task.capacity(),
      mapred.task.timeout=ONEMIN*minutes.until.timeout,
      rhipe_map_buffsize=RHIPE_MAP_BUFFSIZE,
      rhipe_reduce_buffsize=RHIPE_REDUCE_BUFFSIZE,
      mapred.min.split.size=mapred.min.split.size
    )
  )

  rhex(z, async=F)

}

