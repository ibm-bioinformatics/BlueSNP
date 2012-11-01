rm(list=ls())
SCRIPT = "validations"
OUTDIR = paste("../OUTPUT/", SCRIPT, "/", sep="")
if (!file.exists(OUTDIR)) { dir.create(OUTDIR,recursive=TRUE) }
library(BlueSNP)

# #########################################
# Quantitative Trait Data
# #########################################

d = read.table("../data/qt/quantitative_trait.raw", header=T)
X = as.matrix(d[,-seq(1, 6)])  # genotype matrix
y = d$PHENOTYPE

# #########################################
# qt.linear.regression()

# gold standard
plink = read.table("../data/qt/linreg.qassoc", header=T)
head(plink)

bluesnp = as.data.frame(t(apply(X, 2, function(x) { unlist(qt.linear.regression(y, x)) })))
head(bluesnp)

myplot <- function(plink, bluesnp) {
  par(mfrow=c(2,3), pty="s")
  plot(bluesnp$beta, plink$BETA)
  abline(0, 1, col=2)
  plot(bluesnp$se, plink$SE)
  abline(0, 1, col=2)
  plot(bluesnp$R2, plink$R2)
  abline(0, 1, col=2)
  plot(bluesnp$t.statistic, plink$"T")
  abline(0, 1, col=2)
  plot(bluesnp$p.value, plink$P)
  abline(0, 1, col=2)
  plot(-log10(bluesnp$p.value), -log10(plink$P))
  abline(0, 1, col=2)
}

myplot(plink, bluesnp)

outfile = paste(OUTDIR, "qt_linear_regression.pdf", sep="")
pdf(outfile)
  myplot(plink, bluesnp)
dev.off()

# #########################################
# Case Control Data
# #########################################

d = read.table("../data/cc/case_control.raw", header=T)
X = as.matrix(d[,-seq(1, 6)])  # genotype matrix
y = d$PHENOTYPE

# #########################################
# cc.allelic()

plink = read.table("../data/cc/allelic.assoc", header=T)
head(plink)

bluesnp = as.data.frame(t(apply(X, 2, function(x) { unlist(cc.allelic(y, x)) })))
head(bluesnp)

myplot <- function(plink, bluesnp) {
  par(mfrow=c(2,3), pty="s")
  plot(bluesnp$chi.sq, plink$CHISQ)
  abline(0, 1, col=2)
  plot(bluesnp$odds.ratio, plink$OR)
  abline(0, 1, col=2)
  plot(bluesnp$p.value, plink$P)
  abline(0, 1, col=2)
  plot(-log10(bluesnp$p.value), -log10(plink$P))
  abline(0, 1, col=2)
}

myplot(plink, bluesnp)

outfile = paste(OUTDIR, "cc_allelic.pdf", sep="")
pdf(outfile)
  myplot(plink, bluesnp)
dev.off()

# #########################################
# cc.logistic.regression()

plink = read.table("../data/cc/logistic.assoc.logistic", header=T)
head(plink)

bluesnp = as.data.frame(t(apply(X, 2, function(x) { unlist(cc.logistic.regression(y, x)) })))
head(bluesnp)

myplot <- function(plink, bluesnp) {
  par(mfrow=c(2,3), pty="s")
  plot(bluesnp$odds.ratio, plink$OR)
  abline(0, 1, col=2)
  plot(bluesnp$stat, plink$STAT)
  abline(0, 1, col=2)
  plot(bluesnp$p.value, plink$P)
  abline(0, 1, col=2)
  plot(-log10(bluesnp$p.value), -log10(plink$P))
  abline(0, 1, col=2)
}

myplot(plink, bluesnp)

outfile = paste(OUTDIR, "cc_logistic_regression.pdf", sep="")
pdf(outfile)
  myplot(plink, bluesnp)
dev.off()

# #########################################
# cc.genotypic()

plink = read.table("../data/cc/model.geno", header=T)
head(plink)

bluesnp = as.data.frame(t(apply(X, 2, function(x) { unlist(cc.genotypic(y, x)) })))
head(bluesnp)

myplot <- function(plink, bluesnp) {
  par(mfrow=c(2,3), pty="s")
  plot(bluesnp$chi.sq, plink$CHISQ)
  abline(0, 1, col=2)
  plot(bluesnp$p.value, plink$P)
  abline(0, 1, col=2)
  plot(-log10(bluesnp$p.value), -log10(plink$P))
  abline(0, 1, col=2)
}

myplot(plink, bluesnp)

outfile = paste(OUTDIR, "cc_genotypic.pdf", sep="")
pdf(outfile)
  myplot(plink, bluesnp)
dev.off()

# #########################################
# cc.trend()

plink = read.table("../data/cc/model.trend", header=T)
head(plink)

bluesnp = as.data.frame(t(apply(X, 2, function(x) { unlist(cc.trend(y, x)) })))
head(bluesnp)

myplot <- function(plink, bluesnp) {
  par(mfrow=c(2,3), pty="s")
  plot(bluesnp$chi.sq, plink$CHISQ)
  abline(0, 1, col=2)
  plot(bluesnp$p.value, plink$P)
  abline(0, 1, col=2)
  plot(-log10(bluesnp$p.value), -log10(plink$P))
  abline(0, 1, col=2)
}

myplot(plink, bluesnp)

outfile = paste(OUTDIR, "cc_trend.pdf", sep="")
pdf(outfile)
  myplot(plink, bluesnp)
dev.off()

# #########################################
# cc.dominant()

plink = read.table("../data/cc/model.dom", header=T)
head(plink)

bluesnp = as.data.frame(t(apply(X, 2, function(x) { unlist(cc.dominant(y, x)) })))
head(bluesnp)

myplot <- function(plink, bluesnp) {
  par(mfrow=c(2,3), pty="s")
  plot(bluesnp$chi.sq, plink$CHISQ)
  abline(0, 1, col=2)
  plot(bluesnp$p.value, plink$P)
  abline(0, 1, col=2)
  plot(-log10(bluesnp$p.value), -log10(plink$P))
  abline(0, 1, col=2)
}

myplot(plink, bluesnp)

outfile = paste(OUTDIR, "cc_dominant.pdf", sep="")
pdf(outfile)
  myplot(plink, bluesnp)
dev.off()

# #########################################
# cc.recessive()

plink = read.table("../data/cc/model.rec", header=T)
head(plink)

bluesnp = as.data.frame(t(apply(X, 2, function(x) { unlist(cc.recessive(y, x)) })))
head(bluesnp)

myplot <- function(plink, bluesnp) {
  par(mfrow=c(2,3), pty="s")
  plot(bluesnp$chi.sq, plink$CHISQ)
  abline(0, 1, col=2)
  plot(bluesnp$p.value, plink$P)
  abline(0, 1, col=2)
  plot(-log10(bluesnp$p.value), -log10(plink$P))
  abline(0, 1, col=2)
}

myplot(plink, bluesnp)

outfile = paste(OUTDIR, "cc_recessive.pdf", sep="")
pdf(outfile)
  myplot(plink, bluesnp)
dev.off()


















