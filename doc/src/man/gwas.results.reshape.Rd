\name{gwas.results.reshape}
\alias{gwas.results.reshape}
\title{gwas.results.reshape}

\description{
  Reshape the data.from returned by \code{gwas.results} and \code{gwas.results.perm} 
  from tall format to wide format (mimics PLINK output).
}

\usage{gwas.results.reshape(X)}

\arguments{
  \item{X}{data.frame}
}

%\details{
%}

\value{
  data.frame with columns
  \item{rsid}{}
  \item{chr}{}
  \item{bp}{}
  \item{stat1}{}
  \item{stat2}{}
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
  \code{\link[BlueSNP:gwas.results]{gwas.results}}
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
