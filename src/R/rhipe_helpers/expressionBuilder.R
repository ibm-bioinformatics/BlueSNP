expressionBuilder <- function(logic, ...) {
  
  # USEAGE: .variable.substitution("val=VAL", VAL=5) --> "val=5"
  .variable.substitution <- function(content, ...) {
    ARGS = unlist(list(...))
    if (!is.null(ARGS)) {
      for (i in 1:length(ARGS)) {
        content = gsub(names(ARGS)[i], ARGS[i], content)
      }
    }
    content
  }

  if (is.null(logic))
    stop("logic=expression() is required")

  if (class(logic)!="expression")
    stop("logic=expression() is required")

  new.logic = NULL

  if (!is.null(logic$pre) || 
    !is.null(logic$reduce) || 
    !is.null(logic$post)) {  
    # if logic has these parts, it is a REDUCE expression

    # convert to text
    pre    = paste(deparse(logic$pre), collapse="\n")
    reduce = paste(deparse(logic$reduce), collapse="\n")
    post   = paste(deparse(logic$post), collapse="\n")

    # substitute val=VAL --> val=5
    pre    = .variable.substitution(pre, ...)
    reduce = .variable.substitution(reduce, ...)
    post   = .variable.substitution(post, ...)

    # convert to expression
    pre    = parse(text=pre, srcfile=NULL)
    reduce = parse(text=reduce, srcfile=NULL)
    post   = parse(text=post, srcfile=NULL)

    # rebuild overall expression
    new.logic = as.expression(list(pre=pre[[1]], reduce=reduce[[1]], post=post[[1]]))
    
  } else {  # logic is a MAP expression
    
    content = paste(deparse(logic[[1]]), collapse="\n")
    content = .variable.substitution(content, ...)
    new.logic = parse(text=content, srcfile=NULL)
    
  }

  new.logic
}

# ##################################################
# tests

#logic = expression(pre = {
#    hello
#}, reduce = {
#    a = 1
#    b = 2
#    c = a + b
#}, post = {
#    c
#})
#
#expressionBuilder(logic, hello=555)
#
#logic = expression({
#    a = 1
#    b = 2
#    C = a + b
#})
#
#expressionBuilder(logic, C="ccc")

