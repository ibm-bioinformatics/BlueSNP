identityMR <- function(ifolder, ofolder, user.code=NULL) {

  jobname = "identityMR" 
  rhinit.singleton()
  
  setup = expression({
    suppressPackageStartupMessages(library(BlueSNP))
  })
  
  setup = append.user.code.to.setup(setup, user.code)
  shared = append.user.code.to.shared("", user.code)
  
  map = identity.map
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
      mapred.task.timeout=minutes(1)
    )
  )
  
  rhex(z, async=F)
}
