identity.map = expression({
  rhcounter("PROGRESS", "_TASK_", 1)
  for (i in seq_along(map.values)) {
    rhcounter("PROGRESS", "_IN_", 1)
    rhstatus(paste("Working on key:", map.keys[[i]]))
    rhcollect(map.keys[[i]], map.values[[i]])
    rhcounter("PROGRESS", "_OUT_", 1)
  }
})
