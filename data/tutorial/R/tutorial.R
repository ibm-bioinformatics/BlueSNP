### R code from vignette source 'Tutorial.Snw'

###################################################
### code chunk number 1: tutorial.Snw:39-40
###################################################
library(BlueSNP)


###################################################
### code chunk number 2: tutorial.Snw:44-49
###################################################
read.plink.tped(
  tped.hdfs.path="tutorial/qt/data/simulated_qt.tped",
  output.hdfs.path="tutorial/qt/snps", 
  mapred.reduce.tasks=5
)


###################################################
### code chunk number 3: tutorial.Snw:53-57
###################################################
read.plink.tfam(
  "tutorial/qt/data/simulated_qt.tfam", 
  "tutorial/qt/pheno.RData"
)


###################################################
### code chunk number 4: tutorial.Snw:63-69
###################################################
gwas(
  "tutorial/qt/snps", 
  "tutorial/qt/pheno.RData", 
  "tutorial/qt/results", 
  pvalue.report.cutoff=.1
)


###################################################
### code chunk number 5: tutorial.Snw:73-74
###################################################
results = gwas.results("tutorial/qt/results")


###################################################
### code chunk number 6: tutorial.Snw:76-77
###################################################
head(results)


###################################################
### code chunk number 7: tutorial.Snw:81-82
###################################################
results2 = gwas.results.reshape(results)


###################################################
### code chunk number 8: tutorial.Snw:84-85
###################################################
head(results2)


###################################################
### code chunk number 9: tutorial.Snw:89-90
###################################################
plot(-log10(results2$p.value))


###################################################
### code chunk number 10: tutorial.Snw:98-103
###################################################
source("~/tutorial/R/generate_more_phenotypes.R")  # local file system
generate.more.phenotypes(
  "tutorial/qt/pheno.RData", 
  "tutorial/qt/pheno10.RData", 10
)


###################################################
### code chunk number 11: tutorial.Snw:107-114
###################################################
gwas(
  "tutorial/qt/snps",
  "tutorial/qt/pheno10.RData",
  "tutorial/qt/results-multi",
  pvalue.report.cutoff=.001, 
  phenotype.cols=1:3
)


###################################################
### code chunk number 12: tutorial.Snw:118-119
###################################################
results = gwas.results("tutorial/qt/results-multi")


###################################################
### code chunk number 13: tutorial.Snw:121-122
###################################################
head(results)


###################################################
### code chunk number 14: tutorial.Snw:126-127
###################################################
P = gwas.results("tutorial/qt/results-multi", type="p.value")


###################################################
### code chunk number 15: tutorial.Snw:129-130
###################################################
head(P)


###################################################
### code chunk number 16: tutorial.Snw:134-135
###################################################
plot(-log10(P[,5]))


###################################################
### code chunk number 17: tutorial.Snw:139-140
###################################################
results2 = gwas.results.reshape(results[,1:5])


###################################################
### code chunk number 18: tutorial.Snw:142-143
###################################################
head(results2)


###################################################
### code chunk number 19: tutorial.Snw:149-156
###################################################
gwas.maxT.perm(
  "tutorial/qt/snps", 
  "tutorial/qt/pheno.RData", 
  "tutorial/qt/results-maxT", 
  n.permutations=100
)
results = gwas.results.perm("tutorial/qt/results-maxT")


###################################################
### code chunk number 20: tutorial.Snw:158-159
###################################################
head(results)


###################################################
### code chunk number 21: tutorial.Snw:165-172
###################################################
gwas.adaptive.perm(
  "tutorial/qt/snps", 
  "tutorial/qt/pheno.RData", 
  "tutorial/qt/results-adaptive", 
  n.permutations=2e5
)
results = gwas.results.perm("tutorial/qt/results-adaptive")


###################################################
### code chunk number 22: tutorial.Snw:174-175
###################################################
head(results)


###################################################
### code chunk number 23: tutorial.Snw:190-199
###################################################
read.plink.tped(
  "tutorial/cc/data/simulated_cc.tped", 
  "tutorial/cc/snps", 
  mapred.reduce.tasks=5
)
read.plink.tfam(
  "tutorial/cc/data/simulated_cc.tfam", 
  "tutorial/cc/pheno.RData"
)


###################################################
### code chunk number 24: tutorial.Snw:203-206
###################################################
source("~/tutorial/R/generate_more_phenotypes.R")
generate.more.phenotypes("tutorial/cc/pheno.RData", 
  "tutorial/cc/pheno10.RData", 10)


###################################################
### code chunk number 25: tutorial.Snw:210-219
###################################################
gwas(
  "tutorial/cc/snps", 
  "tutorial/cc/pheno10.RData", 
  "tutorial/cc/results-allelic", 
  mytest="cc.allelic",
  pvalue.report.cutoff=0.001,
  phenotype.cols=1:3
)
results = gwas.results("tutorial/cc/results-allelic")


###################################################
### code chunk number 26: tutorial.Snw:221-222
###################################################
head(results)


###################################################
### code chunk number 27: tutorial.Snw:226-227
###################################################
results2 = gwas.results.reshape(results[,1:5])


###################################################
### code chunk number 28: tutorial.Snw:229-230
###################################################
head(results2)


###################################################
### code chunk number 29: tutorial.Snw:234-235
###################################################
P = gwas.results("tutorial/cc/results-allelic", type="p.value")


###################################################
### code chunk number 30: tutorial.Snw:237-238
###################################################
head(P)


###################################################
### code chunk number 31: tutorial.Snw:246-274
###################################################
# my_custom_test.R
my.custom.test <- function(y, x) {
  # y is phenotype vector {0,1} = {control,case} or {1,2} = {control,case}
  # x is genotype {0,1,2} = number of copies minor allele

  # REQUIRED CONVENTION
  # Return output var names when function is called with no params
  if (nargs()==0) {  # called with no params
    y = sample(0:1, 100, replace=T)  # dummy data
    x = sample(0:2, 100, replace=T)
    return(names(my.custom.test(y, x)))
  }

  # select elements with values
  is = !is.na(x) & !is.na(y)  
  x = x[is]
  y = y[is]

  # number of individuals
  N = as.numeric(sum(is))
  
  # our novel test statistic
  stat = cor(y, x)^2 * (N - 2)

  # REQUIRED CONVENTION
  # return a list of named entries
  list(n.individuals=N, stat=stat)
}


###################################################
### code chunk number 32: tutorial.Snw:294-303
###################################################
gwas.maxT.perm(
  "tutorial/cc/snps",
  "tutorial/cc/pheno.RData",
  "tutorial/cc/results-custom",
  n.permutations=200,
  user.code="tutorial/R/my_custom_test.R",
  mytest="my.custom.test",
  statistic.name="stat"
)


###################################################
### code chunk number 33: tutorial.Snw:308-309
###################################################
results = gwas.results.perm("tutorial/cc/results-custom")


###################################################
### code chunk number 34: tutorial.Snw:311-312
###################################################
head(results)


###################################################
### code chunk number 35: tutorial.Snw:316-317
###################################################
subset(results, p.value.adjusted<.01)


###################################################
### code chunk number 36: tutorial.Snw:343-346
###################################################
library(emma)
library(BlueSNP)
peek("tutorial/cc/snps", 20)


###################################################
### code chunk number 37: tutorial.Snw:350-351
###################################################
head(keys)


###################################################
### code chunk number 38: tutorial.Snw:355-356
###################################################
X = do.call("cbind", lapply(values, "[[", "snp.vector"))


###################################################
### code chunk number 39: tutorial.Snw:360-361
###################################################
K = emma.kinship(t(X/2))


###################################################
### code chunk number 40: tutorial.Snw:367-368
###################################################
rhget("tutorial/cc/pheno.RData", ".")


###################################################
### code chunk number 41: tutorial.Snw:372-373
###################################################
load("pheno.RData")


###################################################
### code chunk number 42: tutorial.Snw:377-378
###################################################
save(file="pheno_and_kinship.RData", list=c("Y", "K"))


###################################################
### code chunk number 43: tutorial.Snw:382-383
###################################################
rhput("pheno_and_kinship.RData", "tutorial/cc")


###################################################
### code chunk number 44: tutorial.Snw:387-390
###################################################
y = Y[,1]
x = X[,1]
emma.MLE(y, cbind(1, x/2), K)


###################################################
### code chunk number 45: tutorial.Snw:443-452
###################################################
library(BlueSNP)
gwas(
  genotype.hdfs.path="tutorial/cc/snps", 
  phenotype.hdfs.path="tutorial/cc/pheno_and_kinship.RData",
  output.hdfs.path="tutorial/cc/results-emma",
  user.code="tutorial/R/my_emma_test.R",
  mytest="linear.mixed.model",
  pvalue.report.cutoff=3
)


