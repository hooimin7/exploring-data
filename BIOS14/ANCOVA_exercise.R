# setwd("/Users/med-snt/Documents/Rworkshop")
 getwd()
# rm(list = ls())
set.seed(187)
x1 = rnorm(200, 10, 2)
x2 = 0.5*x1 + rnorm(200, 0, 4)
y = 0.7*x1 + 2.2*x2 + rnorm(200, 0, 4)

n = lm(y~x1+x2)
print(n)
plot(n)
coefs = summary(n)$coef
print(coefs)
#x <- 1:5; coef(lm(c(1:3, 7, 6) ~ x))
summary(n)
y_hat = coefs[1,1] + coefs[2,1]*x1 + coefs[3,1]*x2
var(y_hat)
var(y_hat)/var(y)
y_hat1 = coefs[1,1] + coefs[2,1]*x1 + coefs[3,1]*mean(x2) # To compute the predicted 
#values associated only with $x_1$, we keep $x_2$ constant at its mean
var(y_hat1)
var(y_hat1)/var(y)
#*vice versa* for the variance associated with $x_2$
y_hat2 = coefs[1,1] + coefs[2,1]*mean(x1) + coefs[3,1]*x2
var(y_hat2)
var(y_hat2)/var(y)
var(y_hat)
var(y_hat1) + var(y_hat2)
# last few percent of the variance
var(y_hat1) + var(y_hat2) + 2*cov(y_hat1, y_hat2)
#computing $V(x) = \beta_x^2\sigma_x^2$
coefs[2,1]^2*var(x1)
#include the covariance between the predictors, matrix notation 
#$V(\hat{y}) = \mathbf{\hat{\beta^T}S\hat{\beta}}$, where $\hat{\beta}$ 
#is a vector of parameter estimates (slopes), $\mathbf{S}$ is the covariance
#matrix for the predictors, and $^T$ means transposition.  Recall the `R` matrix 
#multiplication operator `%*%`
t(coefs[2:3,1]) %*% cov(cbind(x1,x2)) %*% coefs[2:3,1]
#$z = \frac{x-\bar{x}}{\sigma(x)}$
#mean of zero and a standard deviation (and variance) of one
x1_z = (x1 - mean(x1))/sd(x1)
x2_z = (x2 - mean(x2))/sd(x2)

m = lm(y ~ x1_z + x2_z)
print(m)
summary(m)
plot(m)
####
x1_m = (x1 - mean(x1))/mean(x1)
x2_m = (x2 - mean(x2))/mean(x2)

summary(lm(y ~ x1_m + x2_m))
o = lm(y ~ x1_m + x2_m)
plot(o)

### Multicollinearity
m1 = lm(x1~x2)
r2 = summary(m1)$r.squared
1/(1-r2)
###
plants = read.csv(file="~/git/exploring-data/BIOS14/alpineplants.csv")
set.seed(12)
x = rnorm(200, 50, 5)
gr = factor(c(rep("Male", 100), rep("Female", 100)))
y = -2 + 1.5*x + rnorm(200, 0, 5)
y[101:200] = 2 + 0.95*x[101:200] + rnorm(100, 0, 6)
plot(x, y, pch=c(1,16)[as.numeric(gr)], las=1)
m = lm(y~x*gr)
anova(m)
summary(m)
#extract the male and female slopes and intercepts with their standard errors
m2 = lm(y ~ -1 + gr + x:gr)
summary(m2)
logLik(m)
logLik(m2)
####

