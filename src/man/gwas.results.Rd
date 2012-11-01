\name{gwas.results}
\alias{gwas.results}
\title{gwas.results}

\description{
  Read the output of \code{gwas} into the R workspace.
}

\usage{
  gwas.results(output.hdfs.path, nrows=-1, type=NULL, is.cleanup=F)
}

\arguments{
  \item{output.hdfs.path}{}
  \item{nrows}{}
  \item{type}{}
  \item{is.cleanup}{}
}

%\details{
%}

\value{
  data.frame with columns:
  \item{type}{}
  \item{rsid}{}
  \item{chr}{}
  \item{bp}{}
  \item{pheno1}{}
  \item{pheno2}{}
  \item{...}{}
}

\references{
  TODO
}

\author{
  Robert J. Prill <rjprill@us.ibm.com>, Hailiang Huang, Sandeep Tata
}

%\note{
%}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
  \code{\link[BlueSNP:gwas.results.reshape]{gwas.results.reshape}}
}

\examples{
  gwas("tutorial/qtl/snps", "tutorial/qtl/pheno.RData", "tutorial/qtl/results")
  results = gwas.results("tutorial/qtl/results")
  pretty = gwas.results.reshape(results)
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
