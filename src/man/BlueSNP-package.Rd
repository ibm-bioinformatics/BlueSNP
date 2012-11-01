\name{BlueSNP-package}
\alias{BlueSNP-package}
\alias{BlueSNP}
\docType{package}

\title{
  Highly scalable GWAS on Hadoop clusters
}

\description{
  BlueSNP makes it easy to test thousands of phenotypes, to use any user-defined statistic as an association test, and to estimate empirical p-values using thousands of permutations of the phenotypes.
}

\details{
  \tabular{ll}{
    Package: \tab BlueSNP\cr
    Type: \tab Package\cr
    Version: \tab 0.1.0\cr  % TODO
    Date: \tab 2012-06-02\cr  % TODO
    License: \tab TODO \cr  % TODO
    LazyLoad: \tab yes\cr
  }
  More details here.
}

\author{
  Robert J. Prill <rjprill@us.ibm.com>, Hailiang Huang, Sandeep Tata
}

\references{
  TODO
}

\keyword{package}
\keyword{SNP}
\keyword{GWAS}
\keyword{Hadoop}
\keyword{MapReduce}
\keyword{cluster}
\keyword{association}

\seealso{
  \code{\link[BlueSNP:gwas]{gwas}}, 
  \code{\link[BlueSNP:gwas.adaptive.perm]{gwas.adaptive.perm}}, 
  \code{\link[BlueSNP:gwas.maxT.perm]{gwas.maxT.perm}}, 
  \code{\link[BlueSNP:read.plink.tped]{read.plink.tped}}, 
  \code{\link[BlueSNP:read.plink.tfam]{read.plink.tfam}}
}

\examples{
  gwas("tutorial/qtl/snps", "tutorial/qtl/pheno.RData", "tutorial/qtl/results")
  
  # load output from gwas() into R workspace
  results = gwas.results("tutorial/qtl/results")
  
  # reshape results into a prettier format
  pretty = gwas.results.reshape(results)
}

