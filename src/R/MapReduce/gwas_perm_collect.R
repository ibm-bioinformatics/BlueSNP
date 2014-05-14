gwas.perm.collect <- function(
  input.hdfs.path,
  output.hdfs.path,
  minutes.until.timeout=10
  ) {

  # ################################################################################
  # Collect multiple permutation records into a single permutation record
  # ################################################################################

  rhinit.singleton()

  # new setup format
  setup = expression(
    reduce={
      suppressPackageStartupMessages(library(BlueSNP))
    }
  )

  map = identity.map
  reduce = reduce.collect

  inout = c("sequence", "sequence")
  ifolder = input.hdfs.path
  ofolder = output.hdfs.path

  jobname = "gwas.perm.collect"
  z = rhmr(
    jobname=jobname,
    setup=setup,
    map=map,
    reduce=reduce,
    inout=inout,
    ifolder=ifolder,
    ofolder=ofolder,
    orderby="character",
    partitioner=list(lims=1, type="string"),
    mapred = list(
      mapred.reduce.tasks=reduce.task.capacity(),
      mapred.task.timeout=minutes(minutes.until.timeout),
      rhipe_map_buffsize=RHIPE_MAP_BUFFSIZE,
      rhipe_reduce_buffsize=RHIPE_REDUCE_BUFFSIZE
    )
  )

  rhex(z, async=F)

}

