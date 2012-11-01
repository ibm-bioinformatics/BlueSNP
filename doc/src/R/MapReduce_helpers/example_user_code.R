example.user.code <- function(ifolder, ofolder, user.code=NULL) {

  jobname = "user.code" 
  rhinit.singleton()
  
  setup = expression({
    suppressPackageStartupMessages(library(BlueSNP))
  })
  
  setup = append.user.code.to.setup(setup, user.code)
  shared = append.user.code.to.shared("", user.code)
  
  map = expression({
    rhcounter("PROGRESS", "_TASK_", 1)
    for (i in seq_along(map.values)) {
      rhcounter("PROGRESS", "_IN_", 1)
      rhstatus(paste("Working on key:", map.keys[[i]]))
      value = myfunction(sample(100, 20))  # user-defined
      rhcollect(map.keys[[i]], value)
      rhcounter("PROGRESS", "_OUT_", 1)
    }
  })
  
  reduce = identity.reduce
  
  inout = c("text", "sequence")
  
  z = rhmr(
    jobname=jobname,
    setup=setup,
    shared=shared,
    map=map,
    reduce=reduce,
    inout=inout,
    ifolder=ifolder,
    ofolder=ofolder,
    mapred=list(
      mapred.map.tasks=map.task.capacity(),
      mapred.reduce.tasks=reduce.task.capacity(),
      mapred.task.timeout=ONEMIN
    )
  )
  
  rhex(z, async=F)
}
