# setwd("/Users/med-snt/Documents/Rworkshop")
# getwd()
# rm(list = ls())
set.seed(1)
x = rnorm(50, 10, 2) #non-parametric bootstrap, where we resample the data many times
se_x = sqrt(var(x)/length(x)) #SE = √Var/n
out = NULL
for(i in 1:1000){
  sample = sample(x, replace=TRUE)
  out[i] = mean(sample)
  }
hist(out, las=1, main="")
sd(out)
# The variable out now contains what we can call the sampling distribution of the mean of x. The standard
# deviation of the sampling distribution gives an approximation of the standard error
quantile(out, c(0.025, 0.975))
# 2.5% 97.5%
# 9.760249 10.624404
# Recall that we could also have derived the 95% confidence interval analytically as ±1.96SE.
mean(x) - 1.96*se_x
# [1] 9.739995
########################
# set.seed(1)
# x = rnorm(50, 10, 2)
# se_x = sqrt(var(x)/length(x))
# out = NULL
# for(i in 1:1000){
#   sample = sample(x, replace=TRUE)
#   out[i] = mean(sample)
# }
# hist(out, las=1, main="")
# 
# out1 = NULL
# for(i in 1:1000){
#   sample = sample(x, replace=TRUE)
#   out1[i] = sd(sample)/mean(sample)
# }
# cat(out1)


#################
set.seed(1)
out = matrix(NA, nrow=200, ncol=2)
sdvals = runif(200, 2, 5)

for(i in 1:200){
  x = rnorm(200, 20, sdvals[i])
  cv = sd(x)/mean(x)
  sd_log = sd(log(x))
  out[i,1] = cv
  out[i,2] = sd_log
}

plot(out[,1], out[,2], xlab="CV(x)", ylab="SD(log[x])", las=1)
lines(0:1, 0:1)
