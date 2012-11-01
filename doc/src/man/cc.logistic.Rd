\name{cc.logistic}
\alias{cc.logistic}
\title{cc.logistic}

\description{Logistic regression association test}

\usage{cc.logistic(y, x)}

\arguments{
  \item{y}{phenotype encoded {1,0} case,control}
  \item{x}{genotype encodes {0,1,2} minor (major) allele count}
}

%\details{
%}

\value{
  list of
  \item{n.individuals}{}
  \item{stat}{}
  \item{odds.ratio}{}
  \item{p-value}{}
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
  \code{\link[BlueSNP:qt.linear.regression]{qt.linear.regression}}
}

\examples{
  x = sample(0:2, 100, T)
  y = sample(0:1, 100, T)
  cc.logistic(y, x)
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
