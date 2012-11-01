reduce.collect = expression(
  pre = {
    rhcounter("PROGRESS", "_TASK_", 1)
    value = NULL
    new.value = NULL
  },
  reduce = {
    for (i in seq_along(reduce.values)) {
      rhcounter("PROGRESS", "_IN_", 1)
      value = reduce.values[[i]]
      
      if (is.null(new.value)) {  # first time
        new.value = value
        next
      }
      # if we got this far, this is not the first time
      
      # update value
      new.value$thecount = new.value$thecount + value$thecount
      if (!is.null(new.value$statistic.random))
        new.value$statistic.random = c(new.value$statistic.random, value$statistic.random)
    }
  },
  post = {
    if(is.finished(new.value$thecount)) {  # if we're done
      new.value = new.value[setdiff(names(value), "snp.vector")] # remove snp.vector to save space
    } else {
      rhcounter("PROGRESS", "_IS_NOT_FINISHED_", 1)  # counter
    }
    
    # emit
    rhcollect(reduce.key, new.value)
    rhcounter("PROGRESS", "_OUT_", 1)
  }
)

