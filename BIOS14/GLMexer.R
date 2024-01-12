rm(list = ls())
# x = seq(from=0, to=1, by=0.01)
# v_b = x*(1-x) #Binomial variance
# plot(x, v_b, type="l", xlab="Probability", ylab="Theoretical variance", las=1)

#####################
# set.seed(1)
# x = rnorm(200, 10, 3)
# eta = -2 + 0.2*x
# y = floor(exp(eta + rnbinom(200, 1, mu=.8)))
# 
# par(mfrow=c(1,2))
# plot(x, eta, las=1)
# plot(x, y, las=1)

####################
logit = function(x) log(x/(1-x)) #logit function
invlogit = function(x) 1/(1+exp(-x)) #inverse logit function

x = runif(200)
logit_x = logit(x) #logit transform

par(mfrow=c(2,2))
hist(x, las=1) 
hist(logit_x, las=1) #logit transform

xx = seq(-5, 5, 0.01)
plot(xx, invlogit(xx), type="l", las=1, # inverse logit transform
     xlab="Logit (x)",
     ylab="P")
plot(x, invlogit(logit_x), las=1) # inverse logit transform

########################
plot(xx, invlogit(xx), type="l", las=1, # inverse logit transform
     xlab="Logit/Probit (x)",
     ylab="P")
lines(xx, pnorm(xx), lty=2) #Probit transform
legend("topleft", legend=c("Logit", "Probit"), 
       lty=c(1,2), bty="n")
########################
rm(list = ls())
logit = function(x) log(x/(1-x)) #logit function
invlogit = function(x) 1/(1+exp(-x)) #inverse logit function
x = rnorm(200, 10, 3)
eta = -2 + 0.4*x + rnorm(200, 0, 2) #Linear predictor
p = invlogit(eta) #Probability
y = rbinom(200, 1, p)
par(mfrow=c(1,3))
plot(x, eta, las=1) #Linear predictor
plot(x, p, las=1) #Probability
plot(x, y, las=1) #Response
m = glm(y~x, family=binomial(link="logit")) #Fit model
summary(m) 
coefs = summary(m)$coef #Extract coefficients
x_pred = seq(from=min(x), to=max(x), by=0.01) #Predictor values
y_hat = coefs[1,1] + coefs[2,1]*x_pred #Linear predictor
p_hat = invlogit(y_hat) #Probability
plot(x, y, las=1)
lines(x, p_hat, las=1)
y_hat = coefs[1,1] + coefs[2,1]*x #Linear predictor
p_hat = invlogit(y_hat) #Probability
mean(p_hat[which(y==1)]) - mean(p_hat[which(y==0)]) #Difference in means
##########################
rm(list = ls())
dat = read.csv("~/git/exploring-data/BIOS14/dormancy.csv")
names(dat)
subdat = dat[dat$pop=="CC",]

germ = subdat$germ2 * subdat$nseed #Successes
notgerm = subdat$nseed - germ #Failures

mod1 = glm(cbind(germ, notgerm) ~ timetosowing, "binomial", data=subdat)
mod2 = glm(germ2 ~ timetosowing, "binomial", weights=nseed, data=subdat)
logLik(mod1) == logLik(mod2) #Check that the models are equivalent
mod3 = glm(germ2 ~ timetosowing + MCseed, "binomial", weights=nseed, data=subdat)
summary(mod3)
plot(subdat$timetosowing, subdat$germ2, 
     xlab="Duration of after-ripening (days)",
     ylab="Germination rate", las=1)
xvals = seq(min(subdat$timetosowing, na.rm=T),
            max(subdat$timetosowing, na.rm=T), 0.01)

coefs = summary(mod3)$coef
y_hat = coefs[1,1] + coefs[2,1]*xvals #Linear predictor
logit = function(x) log(x/(1-x)) #logit function
# logit(0.5)
# invlogit(0)
invlogit = function(x) 1/(1+exp(-x)) #inverse logit function
lines(xvals, invlogit(y_hat)) #Probability

y_hat2 = coefs[1,1] + coefs[2,1]*xvals + coefs[3,1]*sd(subdat$MCseed) #Linear predictor
lines(xvals, invlogit(y_hat2), lty=2) #Probability

y_hat3 = coefs[1,1] + coefs[2,1]*xvals - coefs[3,1]*sd(subdat$MCseed) #Linear predictor
lines(xvals, invlogit(y_hat3), lty=2) #Probability
legend(x=170, y=0.8, lty=c(1,2,3),
       bty="n",
       legend=c("Seed size = Mean",
                "Seed size = Mean + SD",
                "Seed size = Mean - SD"))
-coefs[1,1]/coefs[2,1] #Duration of after-ripening for 50% germination
#To quantify the seed size effect, we can ask how this changes for a seed that is one standard deviation larger
#or smaller than the mean.
-(coefs[1,1] + coefs[3,1]*sd(subdat$MCseed))/coefs[2,1] # one standard deviation larger
-(coefs[1,1] - coefs[3,1]*sd(subdat$MCseed))/coefs[2,1] # one standard deviation smaller
