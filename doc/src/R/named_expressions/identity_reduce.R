identity.reduce = expression(
  pre={
    rhcounter("PROGRESS", "_TASK_", 1)
    rhstatus(paste("Working on key:", reduce.key))
  },
  reduce={
    for (i in seq_along(reduce.values)) {
      rhcounter("PROGRESS", "_IN_", 1)
      rhcollect(reduce.key, reduce.values[[i]])
      rhcounter("PROGRESS", "_OUT_", 1)
    }
  }
)
