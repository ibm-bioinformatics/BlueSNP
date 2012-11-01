environment.variable <- function(name, myclass="integer") {
  as(Sys.getenv(name), myclass)
}
