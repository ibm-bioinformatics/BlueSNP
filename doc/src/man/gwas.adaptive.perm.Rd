\name{gwas.adaptive.perm}
\alias{gwas.adaptive.perm}
\title{gwas.adaptive.perm}

\description{
  Estimate p-values by permuting the data, dropping SNPs from consideration as 
  a significance threshold is violated.
}

\usage{
gwas.adaptive.perm(
  genotype.hdfs.path, phenotype.hdfs.path, output.hdfs.path, 
  phenotype.cols=1, method="qt.linear.regression", statistic.name="t.statistic", 
  hit.report.cutoff=5, user.code=NULL, minutes.until.timeout=10, n.permutations=1E6, 
  k=0, total.tries=0, n.not.finished=1, previous.n.not.finished=Inf)
}

\arguments{
  \item{genotype.hdfs.path}{genotype dir}
  \item{phenotype.hdfs.path}{phenotype Rdata file (must fit in mapper memory)}
  \item{output.hdfs.path}{results dir}
  \item{phenotype.cols=1}{only one phenotype allowed}
  \item{method="qt.linear.regression}{a built-in or custom test (see user.code.hdfs.path option)}
  \item{statistic.name="t.statistic}{result[[statistic.name]]}
  \item{hit.report.cutoff=5}{only report SNPs with this many or fewer random hits}
  \item{user.code=NULL}{hdfs path to user-defined assoc tests}
  \item{minutes.until.timeout=10}{override hadoop default}
  \item{n.permutations=1E6}{total number of permutations}
  Expert settings for adding more permutations to an output folder
  \item{k=0}{jump into this iteration}
  \item{total.tries=0}{current count}
  \item{n.not.finished=1}{a small number greater than zero}
  \item{previous.n.not.finished=Inf}{number of SNPs unfinished in previous round}
}

\details{
  \code{gwas.adaptive.perm} calls four helper functions:
  \code{gwas.perm.run}, 
  \code{gwas.perm.broadcast}, 
  \code{gwas.perm.collect}, 
  \code{gwas.perm.report}
}

\value{
  Nothing returned. Output written to HDFS at \code{output.hdfs.path}
}

\references{
  TODO
}

\author{
  Robert J. Prill <rjprill@us.ibm.com>, Hailiang Huang, Sandeep Tata
}

\note{
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
  \code{\link[BlueSNP:gwas.maxT.perm]{gwas.maxT.perm}}, 
  \code{\link[BlueSNP:gwas.perm.run]{gwas.perm.run}}, 
  \code{\link[BlueSNP:gwas.perm.broadcast]{gwas.perm.broadcast}}, 
  \code{\link[BlueSNP:gwas.perm.collect]{gwas.perm.collect}}, 
  \code{\link[BlueSNP:gwas.perm.report]{gwas.perm.report}}
}

\examples{
  gwas.adaptive.perm("tutorial/qtl/snps", "tutorial/qtl/pheno.RData", 
    "tutorial/qtl/adaptive", n.permutations=1e6)
  results = gwas.results.perm("tutorial/qtl/adaptive")
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
