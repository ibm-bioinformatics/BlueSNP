\name{gwas.results.perm}
\alias{gwas.results.perm}
\title{gwas.results.perm}

\description{
  Read the output of \code{gwas.adaptive.perm} or \code{gwas.maxT.perm} into the R workspace.
}

\usage{gwas.results.perm(output.hdfs.path, nrows=-1, type=NULL, is.cleanup=F)}

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
  \item{phenotype.name}{}
  \item{rsid}{}
  \item{chromosome}{}
  \item{position}{}
  \item{p.value}{}
  \item{p.value.adjusted}{}
  \item{statistic.real}{}
  \item{hits}{}
  \item{tries}{}
  \item{is.finished}{}
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

%\seealso{
%%% ~~objects to See Also as \code{\link{help}}, ~~~
%}

\examples{
  gwas.adaptive.perm("tutorial/qtl/snps", "tutorial/qtl/pheno.RData", 
  "tutorial/qtl/adaptive", n.permutations=1e6)
  results = gwas.results.perm("tutorial/qtl/adaptive")
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
