\name{gwas}
\alias{gwas}
\title{gwas}

\description{
  Run single-SNP association tests for one or more phenotypes.
}

\usage{
gwas(genotype.hdfs.path, phenotype.hdfs.path, output.hdfs.path,
  method="qt.linear.regression", phenotype.cols=NULL, pvalue.report.cutoff=1E-5,
  split.factor=1, is.sparse.output=F, user.code=NULL, minutes.until.timeout=10)
}

\arguments{
  \item{genotype.hdfs.path}{hdfs path to genotype dir}
  \item{phenotype.hdfs.path}{hdfs path to phenotype.RData file (must fit in memory)}
  \item{output.hdfs.path}{hdfs path to results dir}
  \item{method="qt.linear.regression"}{a built-in or custom association test}
  \item{phenotype.cols=NULL}{subset of phenotype matrix column name or number}
  \item{pvalue.report.cutoff=1E-5}{only report p-values less than this}
  \item{split.factor=1}{integer > 1 decreases the number of splits}
  \item{is.sparse.output=F}{don't create a txt report}
  \item{user.code=NULL}{hdfs path to user-defined assoc tests}
  \item{minutes.until.timeout=10}{override hadoop default}
}

%\details{
%}

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

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
