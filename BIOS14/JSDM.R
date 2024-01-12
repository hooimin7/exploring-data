rm(list = ls())
Y = read.csv(file="~/git/exploring-data/BIOS14/Y.csv")
str(Y)

XData = read.csv(file="~/git/exploring-data/BIOS14/XData.csv")
str(XData)
levels(XData$method)
# factor means makes it into groups
XData$method = as.factor(XData$method)
TrData = read.csv(file="~/git/exploring-data/BIOS14/TrData.csv")
str(TrData)
TrData = as.data.frame(TrData)
str(TrData)
rownames(TrData) = colnames(Y)
names(TrData) = "genus"
dfPi = read.csv(file="~/git/exploring-data/BIOS14/dfPi.csv")
str(dfPi)
dfPi$SA = as.factor(dfPi$SA)
dfPi$SU = as.factor(dfPi$SU)
str(dfPi$SA)
# eck that all the data components have the same number of samples
# and species
dim(Y)
dim(XData)
dim(TrData)
library(Hmsc)
rL1 = HmscRandomLevel(units = unique(dfPi$SA))
rL2 = HmscRandomLevel(units = unique(dfPi$SU))
rL1
rL2
# simple phylogenetic structure
XFormula = ~ method + effort + altitude + MAT + MAP + Tseason + Pseason +
  forest. + lu_het
TrFormula= ~genus
# set up the model
m1 = Hmsc(Y = as.matrix((Y>0)*1),
          XData = XData, XFormula = XFormula,
          TrData = TrData, TrFormula = TrFormula,
          distr = "probit",
          studyDesign = dfPi,
          ranLevels = list(SA=rL1, SU=rL2))
# set the sampling parameters and run the MCMC sampling
thin = 10
samples = 100
adaptNf = ceiling(0.4*samples*thin)
transient = ceiling(0.5*samples*thin)
nChains = 2
a=Sys.time()
m1 = sampleMcmc(m1, samples = samples, thin = thin,
                adaptNf = rep(adaptNf, 2),
                transient = transient,
                nChains = nChains, nParallel = 2, updater=list(GammaEta=FALSE))
Sys.time()-a
filename = paste0("model_thin_", thin, ".RData")
save(m1, file=filename)
load("model_thin_10.RData")
mpost = convertToCodaObject(m1)
esBeta = effectiveSize(mpost$Beta)
summary(esBeta)
psrf = gelman.diag(mpost$Beta, multivariate=FALSE)$psrf
summary(psrf)
# plot the posterior trace plots
plot(mpost$Beta)
predY = computePredictedValues(m1, expected=T)
MF = evaluateModelFit(hM=m1, predY = predY)
mean(MF$TjurR2, na.rm=T)
mean(MF$AUC, na.rm=T)
range(MF$TjurR2)
range(MF$AUC)
predYm = apply(predY, 1:2, mean)
par(mfrow=c(1,2))
plot(colSums(predYm,na.rm=T), colSums(m1$Y))
lines(-1000:1000, -1000:1000)
#cor(colSums(predYm), colSums(m1$Y))ˆ2
plot(rowSums(predYm), rowSums(m1$Y))
lines(-1000:1000, -1000:1000)
group = c(rep(1,3), 2, rep(3,1) ,rep(4,4), rep(5,1), rep(6,1))
cbind(m1$covNames, group)
groupnames = c("Baiting method", "Effort", "Altitude", "Climate", "Forest cover", "Landuse heterogeneity
VP = computeVariancePartitioning(hM = m1, group = group, groupnames = groupnames)
#VP$vals = VP$vals[,rev(order(colSums(VP$vals[1:6,])))]
leg = c(VP$groupnames, m1$rLNames)
par(mar=c(8,4,2,10), xpd=T)
bp = barplot(VP$vals, xlab = "", ylab = "Variance proportion",
las = 1, col=grey.colors(8), xaxt="n")
axis(1, at=bp, labels=F)
legend(75, 1, leg, pch=15, col=grey.colors(8))
text(bp, -0.06, srt = 45, adj = 1,cex=.7,
labels = gsub("_", " ", m1$spNames), xpd = TRUE)