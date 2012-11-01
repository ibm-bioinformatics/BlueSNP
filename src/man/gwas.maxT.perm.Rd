\name{gwas.maxT.perm}
\alias{gwas.maxT.perm}
\title{gwas.maxT.perm}

\description{
  Estimate p-values by permuting the data, then adjust using the maxT procedure.
}

\usage{
gwas.maxT.perm(genotype.hdfs.path, phenotype.hdfs.path, output.hdfs.path, 
  phenotype.cols=1, method="qt.linear.regression", statistic.name="t.statistic", 
  n.permutations=1000, hit.report.cutoff=5, user.code=NULL, minutes.until.timeout=10)
}

\arguments{
  \item{genotype.hdfs.path}{genotype dir}
  \item{phenotype.hdfs.path}{phenotype Rdata file (must fit in mapper memory)}
  \item{output.hdfs.path}{results dir}
  \item{phenotype.cols=1}{only one phenotype allowed}
  \item{method="qt.linear.regression}{a built-in or custom test (see user.code.hdfs.path option)}
  \item{statistic.name="t.statistic}{result[[statistic.name]]}
  \item{n.permutations=1000}{total number of permutations}
  \item{hit.report.cutoff=5}{only report SNPs with this many or fewer random hits}
  \item{user.code=NULL}{hdfs path to user-defined assoc tests}
  \item{minutes.until.timeout=10}{override hadoop default}
}

\details{
  \code{gwas.maxT.perm} calls four helper functions:
  \code{gwas.perm.run}, 
  \code{gwas.maxT.perm.findmax}, 
  \code{gwas.maxT.perm.adjust}, 
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

%\note{
%}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
  \code{\link[BlueSNP:gwas.adaptive.perm]{gwas.adaptive.perm}}, 
  \code{\link[BlueSNP:gwas.perm.run]{gwas.perm.run}}, 
  \code{\link[BlueSNP:gwas.maxT.perm.findmax]{gwas.maxT.perm.findmax}}, 
  \code{\link[BlueSNP:gwas.maxT.perm.adjust]{gwas.maxT.perm.adjust}}, 
  \code{\link[BlueSNP:gwas.perm.report]{gwas.perm.report}}
}

\examples{
  gwas.maxT.perm("tutorial/qtl/snps", "tutorial/qtl/pheno.RData", "tutorial/qtl/maxT", 
    n.permutations=100)
  results.maxT = gwas.results.perm("tutorial/qtl/maxT")
  subset(results.maxT, p.value.adjusted<.9)
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
