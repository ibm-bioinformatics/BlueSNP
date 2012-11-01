test.installation <- function() {

  rhinit.singleton()
  
  # simulated data files
  tped = system.file("simulated_cc.tped", package="BlueSNP")
  tfam = system.file("simulated_cc.tfam", package="BlueSNP")
  
  cat("Copying simulated_cc.tped to HDFS bluesnp_test/simulated_cc.tped\n")
  rhput(tped, "bluesnp_test/simulated_cc.tped", map.task.capacity())
  
  cat("Copying simulated_cc.tfam to HDFS bluesnp_test/simulated_cc.tfam\n")
  rhput(tfam, "bluesnp_test/simulated_cc.tfam")
  
  cat("Parsing tped file into SNP records\n")
  read.plink.tped("bluesnp_test/simulated_cc.tped", "bluesnp_test/snps")
  
  cat("Parsing tfam file into pheno.RData\n")
  read.plink.tfam("bluesnp_test/simulated_cc.tfam", "bluesnp_test/pheno.RData")
  
  cat("Run allelic test\n")
  gwas("bluesnp_test/snps", "bluesnp_test/pheno.RData", "bluesnp_test/results", 
    method="cc.allelic")
    
  cat("Fetching results\n")
  results = gwas.results("bluesnp_test/results")

  cat("Reshaping results\n")
  pretty = gwas.results.reshape(results)
  
  pretty
  
}

