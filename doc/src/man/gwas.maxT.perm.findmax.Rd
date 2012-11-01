\name{gwas.maxT.perm.findmax}
\alias{gwas.maxT.perm.findmax}
\title{gwas.maxT.perm.findmax}

\description{
  Find the max random test statistic in each iteration across all SNPs
}
\usage{
  gwas.maxT.perm.findmax(input.hdfs.path, output.hdfs.path, 
    minutes.until.timeout=10)
}

\arguments{
  \item{input.hdfs.path}{}
  \item{output.hdfs.path}{}
  \item{minutes.until.timeout}{}
}

\details{
  Expert use only. This function is called by \code{gwas.maxT.perm}.
}

%\value{
%%%  ~Describe the value returned
%%%  If it is a LIST, use
%%%  \item{comp1 }{Description of 'comp1'}
%%%  \item{comp2 }{Description of 'comp2'}
%%% ...
%}

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
  \code{\link[BlueSNP:gwas.maxT.perm]{gwas.maxT.perm}}
}

%\examples{
%}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
