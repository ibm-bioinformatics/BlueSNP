permutation.loop <- function(
  thecount,
  method, 
  statistic.name, 
  y, 
  x, 
  statistic.real, 
  n.permutations, 
  keepalive=NULL,         # keepalive gets the snp index number
  random.seed,            # value is crucial for maxT and adaptive to work properly
  is.premature.stop=F,    # default for maxT is to NOT do a premature stop
  is.report.all.stats=T   # default for maxT is to return all test stats
) {

  all.statistic.rand = NULL  # keep all of them (for maxT adjustment)

  # ##################################################
  # WARNING: the value of random.seed is crucial for maxT and adaptive permutation
  # to work properly
  
  if (!is.null(random.seed))  # only maxT is not NULL
    set.seed(random.seed)  # every mapper generates the same sample(y)
  else  # adaptive 
    set.seed(sample(999999999, 1))  # every mapper needs a different seed
  
  # ##################################################

  for (i in 1:n.permutations) {
    
    statistic.rand = eval(call(method, sample(y), x))[[statistic.name]]
    
    # increment the count
    if (abs(statistic.rand) >= abs(statistic.real))
      thecount = increment.hit(thecount)
    else
      thecount = increment.miss(thecount)

    if (is.report.all.stats)  # maxT reports all the random test statistics
      all.statistic.rand[i] = statistic.rand  # remember

    if (!is.null(keepalive))
      rhstatus(paste("SNP", keepalive, "Permutation", i))  # keepalive

    if (is.premature.stop) {  # adaptive stops permutations early if the hit limit is reached
      if (!(i %% 10)) {  # do every 10 iterations
        if (is.finished(thecount)) {
          break
        }
      }
    }

  }

  if (is.report.all.stats)  # maxT reports all the random test statistics
    list(thecount=thecount, statistic.random=all.statistic.rand)
  else
    list(thecount=thecount)
}

