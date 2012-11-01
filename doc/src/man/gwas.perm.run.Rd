 \name{gwas.perm.run}
\alias{gwas.perm.run}
\title{gwas.perm.run}

\description{Run permutation tests.}

\usage{
gwas.perm.run(genotype.hdfs.path, phenotype.hdfs.path, output.hdfs.path,
  method="qt.linear.regression", statistic.name="t.statistic", phenotype.cols=1, 
  is.maxT=T, is.premature.stop=F, is.report.all.stats=T, is.reduce=T, 
  n.permutations=1000, user.code=NULL, mapred.reduce.tasks=reduce.task.capacity(), 
  minutes.until.timeout=10, job.tag=NULL)
}

\arguments{
  \item{genotype.hdfs.path}{}
  \item{phenotype.hdfs.path}{}
  \item{output.hdfs.path}{}
  \item{method}{}
  \item{statistic.name}{}
  \item{phenotype.cols}{}
  \item{is.maxT}{}
  \item{is.premature.stop}{}
  \item{is.report.all.stats}{}
  \item{is.reduce}{}
  \item{n.permutations}{}
  \item{user.code}{}
  \item{mapred.reduce.tasks}{}
  \item{minutes.until.timeout}{}
  \item{job.tag}{}
}

\details{
  Expert use only. This function is called by \code{gwas.adaptive.perm} and \code{gwas.maxT.perm}
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
  \code{\link[BlueSNP:gwas.adaptive.perm]{gwas.adaptive.perm}}, 
  \code{\link[BlueSNP:gwas.maxT.perm]{gwas.maxT.perm}}, 
}

%\examples{
%}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
