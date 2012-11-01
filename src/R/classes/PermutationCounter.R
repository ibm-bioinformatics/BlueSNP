# PermutationCounter S3 object
# Robert Prill  rjprill@us.ibm.com  12-15-2011


# ##################################################
# generic method

PermutationCounter <- function(x, ...)
  UseMethod("PermutationCounter")


# ##################################################
# constructor

PermutationCounter.default <- function(hits=0, tries=0, 
  previous.hits=0, previous.tries=0, limit=5) {
  obj = list(
    hits = hits,
    tries = tries,
    previous.hits = previous.hits,    # all the hits we know about
    previous.tries = previous.tries,  # all the tries we know about
    limit = limit                     # see is.finished()
  )
  class(obj) <- "PermutationCounter"
  obj
}


# ##################################################
# getter methods

hits.PermutationCounter <- function(x)
  x[["hits"]]

tries.PermutationCounter <- function(x)
  x[["tries"]]

previous.hits.PermutationCounter <- function(x)
  x[["previous.hits"]]

previous.tries.PermutationCounter <- function(x)
  x[["previous.tries"]]


# ##################################################
# virtual attribute getter methods

is.finished.PermutationCounter <- function(x) 
  (x[["previous.hits"]] + x[["hits"]]) > x[["limit"]]

pval.PermutationCounter <- function(x) {
  #(x[["previous.hits"]] + x[["hits"]] + 1) / (x[["previous.tries"]] + x[["tries"]] + 1)
  # only base the pval on hits and tries.  
  # previous.hits and previous.tries only support an early termination
  (x[["hits"]] + 1) / (x[["tries"]] + 1)
}

# ##################################################
# setter methods

"hits<-.PermutationCounter" <- function(x, value) {
  x[["hits"]] <- value
  x
}

"tries<-.PermutationCounter" <- function(x, value) {
  x[["tries"]] <- value
  x
}

"previous.hits<-.PermutationCounter" <- function(x, value) {
  x[["previous.hits"]] <- value
  x
}

"previous.tries<-.PermutationCounter" <- function(x, value) {
  x[["previous.tries"]] <- value
  x
}


# ##################################################
# math operations

# addition
"+.PermutationCounter" <- function(x, y) {
  # validation
  #if (x[["previous.hits"]] != y[["previous.hits"]] | x[["previous.tries"]] != y[["previous.tries"]])
  #  stop("Can only add PermutationCounter objects if previous.hits and previous.tries are the same in both counters.")

  x[["hits"]]  <- x[["hits"]]  + y[["hits"]]
  x[["tries"]] <- x[["tries"]] + y[["tries"]]
  x
}

# subtraction
"-.PermutationCounter" <- function(x, y) {
  # validation
  #if (x[["previous.hits"]] != y[["previous.hits"]] | x[["previous.tries"]] != y[["previous.tries"]])
  #  stop("Can only subtract PermutationCounter objects if previous.hits and previous.tries are the same in both counters.")

  x[["hits"]]  <- x[["hits"]]  - y[["hits"]]
  x[["tries"]] <- x[["tries"]] - y[["tries"]]
  x
}


# ##################################################
# more methods

increment.hit.PermutationCounter <- function(x) {
  # handle validation that previous.hits and previous.tries must match
  y = x
  y[["hits"]]  = 1
  y[["tries"]] = 1
  
  x + y
}

increment.miss.PermutationCounter <- function(x) {
  #x + PermutationCounter.default(0, 1, 0, 0)
  y = x
  y[["hits"]]  = 0
  y[["tries"]] = 1
  x + y
}

broadcast.PermutationCounter <- function(x) {
  total.hits  = x[["hits"]]  + x[["previous.hits"]]
  total.tries = x[["tries"]] + x[["previous.tries"]]
  PermutationCounter.default(0, 0, total.hits, total.tries)
}

# ##################################################
# presentation

summary.PermutationCounter <- function(x) {
  data.frame(
    is.finished  = is.finished.PermutationCounter(x),
    p.value      = pval.PermutationCounter(x),
    hits         = hits.PermutationCounter(x),
    tries        = tries.PermutationCounter(x),
    previous.hits  = previous.hits.PermutationCounter(x),
    previous.tries = previous.tries.PermutationCounter(x)
  )
}


# ##################################################
# generic methods (i.e., objected-oriented polymorphism)

hits <- function(x, ...)
  UseMethod("hits", x)

tries <- function(x, ...)
  UseMethod("tries", x)

previous.hits <- function(x, ...)
  UseMethod("previous.hits", x)

previous.tries <- function(x, ...)
  UseMethod("previous.tries", x)

is.finished <- function(x, ...) 
  UseMethod("is.finished", x)

pval <- function(x, ...)
  UseMethod("pval", x)
  
increment.hit <- function(x, ...)
  UseMethod("increment.hit", x)

increment.miss <- function(x, ...)
  UseMethod("increment.miss", x)
  
"hits<-" <- function(x, ...)
  UseMethod("hits<-", x)

"tries<-" <- function(x, ...)
  UseMethod("tries<-", x)

"previous.hits<-" <- function(x, ...)
  UseMethod("previous.hits<-", x)

"previous.tries<-" <- function(x, ...)
  UseMethod("previous.tries<-", x)
  
broadcast <- function(x, ...)
  UseMethod("broadcast", x)

# addition and subtraction generic methods already exist

# see method list using:
# methods(class=PermutationCounter)


