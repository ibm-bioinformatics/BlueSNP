\name{qt.linear.regression}
\alias{qt.linear.regression}
\title{qt.linear.regression}

\description{Linear regression association test}

\usage{qt.linear.regression(y, x)}

\arguments{
  \item{y}{quantitative phenotype numeric()}
  \item{x}{genotype encodes {0,1,2} minor (major) allele count numeric()}
}

%\details{
%}

\value{
  list of 
  \item{n.individuals}{}
  \item{beta}{}
  \item{se}{}
  \item{t.statistic}{}
  \item{p.value}{}
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
  \code{\link[BlueSNP:cc.allelic]{cc.allelic}}, 
  \code{\link[BlueSNP:cc.logistic]{cc.logistic}}
}

\examples{
  x = sample(0:2, 100, T)
  y = runif(100)
  qt.linear.regression(y, x)
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
