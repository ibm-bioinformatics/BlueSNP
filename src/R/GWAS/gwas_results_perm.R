# read the results of gwas() into the R worksapce
gwas.results.perm <- function(
  output.hdfs.path,    # hdfs path to results dir (no trailing slash!)
  nrows = -1,          # maximum number of text rows to read from the HDFS part-xxxx files
  type = NULL, # "p.value" or "beta" or "se", etc.
  is.cleanup = F
) {
  
  rhinit.singleton()
  
  if (!file.exists("./tmp")) { 
    dir.create("./tmp") 
  } else {
    system("rm -r ./tmp")
    dir.create("./tmp") 
  }
  
  hdfs.path = paste(output.hdfs.path, "report/part*", sep="/")
  rhget(hdfs.path, "./tmp")
  if (is.null(type)) {
    system("cat ./tmp/part* > ./tmp/out.txt")
  } else {
    command = paste("cat ./tmp/part* | grep '", type, "' > ./tmp/out.txt", sep="")
    system(command)
  }
  
  d = data.frame(t(rep(NA, 10)))
  tryCatch({
    d = read.table("./tmp/out.txt", nrows=nrows)
  }, error=function(e) {
    # fails if out.txt is empty
    # e$message
  })

  names(d) = c("phenotype.name", "rsid", "chromosome", "position", 
    "p.value", "p.value.adjusted", "statistic.real", "hits", "tries", "is.finished")

  if (is.cleanup) {
    system("rm -r ./tmp")
  }
  
  d
}

