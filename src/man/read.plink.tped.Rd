\name{read.plink.tped}
\alias{read.plink.tped}
\title{read.plink.tped}

\description{
  Parse PLINK tped file(s) into SNP records
}

\usage{
read.plink.tped(tped.hdfs.path, output.hdfs.path, 
  mapred.reduce.tasks=0, minutes.until.timeout=10)
}

\arguments{
  \item{tped.hdfs.path}{space or tab delimited tped file, or directory of tped files}
  \item{output.hdfs.path}{where to write the output SNP records}
  \item{mapred.reduce.tasks=0}{manually control the numnber of output files (splits)}
  \item{minutes.until.timeout=10}{override Hadoop default timeout (minutes)}
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
  \code{\link[BlueSNP:gwas]{gwas}}, 
  \code{\link[BlueSNP:read.plink.tfam]{read.plink.tfam}}
}

\examples{
  # parse plink TPED file into SNP records
  read.plink.tped("tutorial/qtl/data/simulated_qt.tped", "tutorial/qtl/snps", 30)

  # parse plink TFAM file into phenotype data matrix
  read.plink.tfam("tutorial/qtl/data/simulated_qt.tfam", "tutorial/qtl/pheno.RData")
}

%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
