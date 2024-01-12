# setwd("/Users/med-snt/Documents/Rworkshop")
# getwd()
# rm(list = ls())
##########################
# birds = read.csv("~/git/exploring-data/BIOS14/bird_allometry.csv")
# head(birds)
# males = birds[birds$Sex=='m',]
# females = birds[birds$Sex=='f',]
# fm = lm(log(brain_mass)~log(body_mass), data = females)
# mm = lm(log(brain_mass)~log(body_mass), data = males)
# print(summary(fm))
# print(summary(mm))
# cfm = fm$coef
# cmm = mm$coef
# print(cfm)
# print(cmm)
# xx = seq(min(log(males$body_mass)), max(log(males$body_mass)), 
#          length.out=100)
# yy = mm$coef[1] + mm$coef[2]*xx
# x = seq(min(log(females$body_mass)), max(log(females$body_mass)), 
#          length.out=100)
# y = fm$coef[1] + fm$coef[2]*x
# 
# plot(log(males$body_mass), log(males$brain_mass),
#      xlab="Body mass (log g)",
#      ylab="Brain mass (log g)",
#      las=1, pch=21, col="black", bg="lightblue")
# lines(xx, yy, lwd=2)

# points(log(females$body_mass), log(females$brain_mass), 
#      xlab="Body mass (log g)",
#      ylab="Brain mass (log g)",
#      las=1, pch=21, col="black", bg="blue")
# lines(x, y, lwd=2)
######################################
# x = rnorm(500, 10, 2)
# y = 1.5*x + rnorm(500, 0, 1)
# 
# slope_est = NULL
# errors = seq(0.01, 0.5, length.out=10)
# 
# relerrors = (errors^2)/var(x)
# 
# for(i in 1:10){
#   x_obs = x + rnorm(500, 0, errors[i])
# 
#   m1 = lm(y~x_obs)
#   slope_est[i] = summary(m1)$coef[2,1]
# }
# 
# plot(errors, slope_est,
#      las=1,
#      xlab="Error standard deviation in x",
#      ylab="Estimated slope")
# corrslope = slope_est/(1-relerrors)
# 
# plot(errors, slope_est,
#      ylim= c(1.4, 1.55),
#      las=1,
#      xlab="Error standard deviation in x",
#      ylab="Estimated slope")
# points(errors, corrslope, pch=16)
# segments(errors, slope_est, errors, corrslope)
#########################################
x = rnorm(500, 10, 2)
y = 1.5*x + rnorm(500, 0, 1)
slope_est = NULL
errors = seq(0.01, 0.5, length.out=10)

relerrors = (errors^2)/var(x)
print(relerrors)
for(i in 1:10){
  x_obs = x + rnorm(500, 0, errors[i])
  
  m1 = lm(y~x_obs)
  slope_est[i] = summary(m1)$coef[2,1]
}
plot(errors, slope_est,
     las=1,
     xlab="Error standard deviation in x",
     ylab="Estimated slope")
corrslope = slope_est/(1-relerrors)

plot(errors, slope_est,
     ylim= c(1.4, 1.55),
     las=1,
     xlab="Error standard deviation in x",
     ylab="Estimated slope")
points(errors, corrslope, pch=16)
segments(errors, slope_est, errors, corrslope)

