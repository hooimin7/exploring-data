rm(list = ls())
# install.packages("Hmsc")
# install.packages(c("Matrix", "MatrixModels", "mvtnorm"), type = "binary")
# install.packages("coda")
# install.packages("spam")
# install.packages("glmmTMB")
library(tidyr)
library(Hmsc)
vignette("vignette_1_univariate", package="Hmsc")
x = rnorm(200, 10, 3)
y = -2 + 0.4*x + rnorm(200, 0, 2)
m1 = lm(y~x)
m2 = Hmsc(Y = as.matrix(y), XData = data.frame(x), XFormula = ~x,
          distr="normal")
m2 = sampleMcmc(m2, samples=1000, transient=1000, thin=1,
                nChains=2, verbose=F)
summary(m1)$coef
# Converts the Hmsc posterior into a named list of mcmc.list objects
mpost = convertToCodaObject(m2)
summary(mpost$Beta)
# posterior trace plot 
plot(mpost$Beta)
plot(mpost$Gamma)
plot(mpost$Sigma)
effectiveSize(mpost$Beta)
# Because we ran two independent MCMC chains, we can also assess whether they yielded similar results
# Values close to 1 means that the chains yielded similar results
gelman.diag(mpost$Beta, multivariate=F)$psrf
library(glmmTMB)
set.seed(145)
x1 = rnorm(200, 10, 2)
groupmeans = rep(rnorm(10, 20, 4), each=20)
groupID = as.factor(rep(paste0("Group", 1:10), each=20))
y = 2 + 1.5*x1 + groupmeans + rnorm(200, 0, 2)
m1 = glmmTMB(y~x1 + (1|groupID))
# we first create a data frame containing the group IDs
# (studyDesign)
studyDesign = data.frame(group = as.factor(groupID))
rL1 = HmscRandomLevel(units = groupID)
m2 = Hmsc(Y = as.matrix(y), XData = data.frame(x1), XFormula = ~x1,
          studyDesign = studyDesign, ranLevels = list(group = rL1),
          distr="normal")
m2 = sampleMcmc(m2, samples=1000, transient=1000, thin=1,
                nChains=2, nParallel=2, verbose=F)
summary(m1)
mpost = convertToCodaObject(m2)
summary(mpost$Beta)
getPostEstimate(m2, "Omega")$mean


# Path: Bayesiab.R