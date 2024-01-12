rm(list = ls())
dat = read.csv("~/git/exploring-data/BIOS14/Eulaema.csv")
library(MASS)
dat$mcMAP = dat$MAP - mean(dat$MAP, na.rm=T)
m = glm.nb(Eulaema_nigrita ~ mcMAP + forest., data = dat)
summary(m)
coefs = summary(m)$coef #Extract coefficients
exp(coefs[1,1]) #Intercept
exp(coefs[1,1] + coefs[3,1]) #Intercept + forest
plot(dat$forest., dat$Eulaema_nigrita, col="grey", las=1,
     xlab="Forest cover",
     ylab="El. nigrita abundance")
newforest = seq(min(dat$forest.), max(dat$forest.), length.out=200) #Predictor values
newMAP = rep(mean(dat$mcMAP), length(newforest)) #Predictor values
y_hat = predict(m, newdata=list(mcMAP=newMAP,
                                forest.=newforest),type="response") #Predicted values
lines(newforest, y_hat, lwd=2) #Plot predicted values
newMAP2 = rep(mean(dat$mcMAP)+sd(dat$mcMAP), length(newforest)) #Predictor values
y_hat2 = predict(m, newdata=list(mcMAP=newMAP2,
                                 forest.=newforest),
                 type="response") #Predicted values
newMAP3 = rep(mean(dat$mcMAP)-sd(dat$mcMAP), length(newforest)) #Predictor values
y_hat3 = predict(m, newdata=list(mcMAP=newMAP3,
                                 forest.=newforest),
                 type="response") #Predicted values
lines(newforest, y_hat2, lwd=2, col="blue3")
lines(newforest, y_hat3, lwd=2, col="firebrick")
legend("topleft", lty=1, lwd=2, col=c(1, "blue3", "firebrick"), bty="n",
       legend=c("MAP = Mean",
                "MAP = Mean + SD",
                "MAP = Mean - SD"))
abline(h=exp(coefs[1,1]), lty=2) #Add horizontal line at intercept
abline(h=exp(coefs[1,1] + coefs[3,1]), lty=2) #Add horizontal line at intercept + forest
                
                