\name{read.plink.tfam}
\alias{read.plink.tfam}
\title{read.plink.tfam}

\description{
  Parse PLINK tfam file(s) into phenotype.RData file 
  containing the matrix Y (individuals x phenotypes)
}

\usage{
read.plink.tfam(tfam.hdfs.path, output.hdfs.path, 
  is.cleanup=T, is.quiet=T)
}

\arguments{
  \item{tfam.hdfs.path}{space or tab delimited tfam file or, directory of tfam files}
  \item{output.hdfs.path}{must end in .RData}
  \item{is.cleanup=T}{delete ./tmp/ ?}
  \item{is.quiet=T}{don't return the matrix to the console}
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
  \code{\link[BlueSNP:read.plink.tped]{read.plink.tped}}
}

\examples{
  # parse plink TPED file into SNP records
  read.plink.tped("tutorial/qtl/data/simulated_qt.tped", "tutorial/qtl/snps", 30)

  # parse plink TFAM file into phenotype data matrix
  read.plink.tfam("tutorial/qtl/data/simulated_qt.tfam", "tutorial/qtl/pheno.RData")
}

%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
