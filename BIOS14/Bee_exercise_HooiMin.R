 rm(list = ls())
 par(mfrow=c(1,1))
 dat = read.csv("~/git/exploring-data/BIOS14/Eulaema.csv")
 library(MASS)
 head(dat)
 subdat <- dat[, !colnames(dat) %in% 'method']
 pairs(subdat)
 # poisson.model <- glm(dat$Eulaema_nigrita ~ dat$MAT + dat$MAP, data = dat, family = poisson(link = "log"))
 # summary(poisson.model)
 # m = glm.nb(dat$Eulaema_nigrita~dat$MAT + dat$MAP)
 # summary(m)
 # poisson.model_1 <- glm(dat$Eulaema_nigrita ~ dat$Tseason + dat$Pseason, data = dat, family = poisson(link = "log"))
 # summary(poisson.model_1)
 # seasons = glm.nb(dat$Eulaema_nigrita~dat$Tseason + dat$Pseason)
 # summary(seasons)
 # poisson.model_2 <- glm(dat$Eulaema_nigrita ~ dat$Tseason, data = dat, family = poisson(link = "log"))
 # summary(poisson.model_2)
 Tse = glm.nb(dat$Eulaema_nigrita~dat$Tseason)
 summary(Tse)
 plot(dat$Tseason, dat$Eulaema_nigrita, las=1, col="orange", pch=16,  xlab="Tseason (sd)",
      ylab="Abundance of Bees", las=1)
 y_hat_Tse = predict(Tse, newdata=list(x=dat$Tseason), type="response", se.fit=T)
 lines(dat$Tseason, y_hat_Tse$fit)
 polygon(c(dat$Tseason, rev(dat$Tseason)),
         c(y_hat_Tse$fit+1.96*y_hat_Tse$se.fit,
           rev(y_hat_Tse$fit-1.96*y_hat_Tse$se.fit)),
         col = rgb(0,1,0,.5), border = FALSE)
 # poisson.model_3 <- glm(dat$Eulaema_nigrita ~ dat$Pseason, data = dat, family = poisson(link = "log"))
 # summary(poisson.model_3)
 Pse = glm.nb(dat$Eulaema_nigrita~dat$Pseason)
 summary(Pse)
 plot(dat$Pseason, dat$Eulaema_nigrita, las=1, col="pink", pch=16,  xlab="Pseason (CV)",
      ylab="Abundance of Bees", las=1)
 y_hat_Pse = predict(Pse, newdata=list(x=dat$Pseason), type="response", se.fit=T)
 lines(dat$Pseason, y_hat_Pse$fit)
 polygon(c(dat$Pseason, rev(dat$Pseason)),
         c(y_hat_Pse$fit+1.96*y_hat_Pse$se.fit,
           rev(y_hat_Pse$fit-1.96*y_hat_Pse$se.fit)),
         col = rgb(0,1,0,.5), border = FALSE)
 cor(dat$altitude,dat$MAT)
 # cor(dat$Tseason,dat$Pseason)
 al = glm.nb(dat$Eulaema_nigrita~dat$altitude)
 summary(al)
 plot(dat$altitude, dat$Eulaema_nigrita, las=1, col="darkgrey", pch=16,  xlab="Altitute (m)",
      ylab="Abundance of Bees", las=1)
 y_hat_al = predict(al, newdata=list(x=dat$altitude), type="response", se.fit=T)
 lines(dat$altitude, y_hat_al$fit)
 polygon(c(dat$altitude, rev(dat$altitude)),
         c(y_hat_al$fit+1.96*y_hat_al$se.fit,
           rev(y_hat_al$fit-1.96*y_hat_al$se.fit)),
         col = rgb(0,1,0,.5), border = FALSE)
 m = glm.nb(dat$Eulaema_nigrita~dat$MAT)
 summary(m)
 plot(dat$MAT, dat$Eulaema_nigrita, las=1, col="lightblue", pch=16, xlab="Mean Annual Temperature (MAT)",
      ylab="Abundance of Bees", las=1)
 y_hat_m = predict(m, newdata=list(x=dat$MAT), type="response", se.fit=T)
 lines(dat$MAT, y_hat_m$fit)
 polygon(c(dat$MAT, rev(dat$MAT)),
         c(y_hat_m$fit+1.96*y_hat_m$se.fit,
           rev(y_hat_m$fit-1.96*y_hat_m$se.fit)),
         col = rgb(0,1,0,.5), border = FALSE)
