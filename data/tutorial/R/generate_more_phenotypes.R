# helper function to generate more (fake) phenotypes
generate.more.phenotypes <- function(input, output, N=10) {

  if (!file.exists("./tmp")) { 
    dir.create("./tmp") 
  } else {
    system("rm -r ./tmp")
    dir.create("./tmp") 
  }
  
  rhinit.singleton()

  rhget(input, "./tmp")

  infile = paste("./tmp", stripPath(input), sep="/")
  load(infile)

  A = matrix(NA, nrow=nrow(Y), ncol=N)
  colnames(A) = paste("pheno", 1:ncol(A), sep="")
  rownames(A) = rownames(Y)
  
  for (i in seq(1,ncol(A),2)) {  # odd ones are copies of the original
    A[,i] = Y[,1]
  }
  
  for (i in seq(2,ncol(A),2)) {  # even ones are random
    A[,i] = sample(Y[,1])
  }
  
  Y = A
  
  tmpfile = paste("./tmp", stripPath(output), sep="/")
  save(Y, file=tmpfile)
  rhput(tmpfile, output)
  
}
