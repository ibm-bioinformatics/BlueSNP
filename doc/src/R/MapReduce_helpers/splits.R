splits <- function(
  A.hdfs.path,
  B.hdfs.path,
  n.splits=map.task.capacity(),
  minutes.until.timeout=10
) {

  rhinit.singleton()

  map = identity.map
  reduce = identity.reduce

  inout = c("sequence", "sequence")
  ifolder = A.hdfs.path
  ofolder = B.hdfs.path

  jobname = "splits"
  z = rhmr(
    jobname=jobname,
    map=map,
    reduce=reduce,
    inout=inout,
    ifolder=ifolder,
    ofolder=ofolder,
    mapred = list(
      mapred.reduce.tasks=n.splits,
      mapred.task.timeout=ONEMIN*minutes.until.timeout,
      rhipe_map_buffsize=RHIPE_MAP_BUFFSIZE,
      rhipe_reduce_buffsize=RHIPE_REDUCE_BUFFSIZE
    )
  )

  rhex(z, async=F)

}
