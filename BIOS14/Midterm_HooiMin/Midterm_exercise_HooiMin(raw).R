rm(list = ls())
par(mfrow=c(2,2))
dat = read.table("~/git/exploring-data/BIOS14/tephritis.txt", header=T)
dat$Patry = as.factor(dat$Patry)
dat$Hostplant = as.factor(dat$Hostplant)
dat$Sex = as.factor(dat$Sex)
dat$Baltic = as.factor(dat$Baltic)
# group = as.factor(c(dat$Patry,dat$Hostplant))
# subdat = dat[, !colnames(dat) %in% 'Patry']
# pairs(dat[c(4:10),])
# library(dplyr)
# dat %>% dplyr::select(BL:Melanization_ratio)
# library(psych)
# pairs.panels(dat)

Oleraceum = dat[dat$Hostplant=="Oleraceum",]

model_1 = lm(OL ~ BL + Wing_length, data = Oleraceum)
model_2 = lm(OL ~ BL * Wing_length, data = Oleraceum)
model_3 = lm(OL ~ BL, data = Oleraceum)
model_4 = lm(OL ~ Wing_length, data = Oleraceum)

mlist = list(model_1, model_2, model_3, model_4) 
# print(mlist)
AICTab = AIC(model_1, model_2, model_3, model_4)
# print(AICTab)
AICTab$logLik = unlist(lapply(mlist, logLik))
# print(AICTab$logLik)
AICTab = AICTab[order(AICTab$AIC, decreasing=F),]
AICTab$delta = round(AICTab$AIC - min(AICTab$AIC), 2)
# print(AICTab$delta)
lh = exp(-0.5*AICTab$delta)
AICTab$w = round(lh/sum(lh), 2)
AICTab
Omul = lm(OL ~ BL + Wing_length, data = Oleraceum)
hist(residuals(Omul))
summary(Omul)
# coefs = summary(Omul)$coef
# Oleraceum$OL_hat = coefs[1,1] + coefs[2,1]*Oleraceum$BL + coefs[3,1]*Oleraceum$Wing_length
# var(Oleraceum$OL_hat)
# Oleraceum$OL_hat1 = coefs[1,1] + coefs[2,1]*Oleraceum$BL + coefs[3,1]*mean(Oleraceum$Wing_length)
# var(Oleraceum$OL_hat1)
# Oleraceum$OL_hat2 = coefs[1,1] + coefs[2,1]*mean(Oleraceum$BL) + coefs[3,1]*Oleraceum$Wing_length
# var(Oleraceum$OL_hat2)
# var(Oleraceum$OL_hat1) + var(Oleraceum$OL_hat2) + 2*cov(Oleraceum$OL_hat1, Oleraceum$OL_hat2)
# var(Oleraceum$OL_hat1)/var(Oleraceum$OL_hat)
# var(Oleraceum$OL_hat2)/var(Oleraceum$OL_hat)
Oleraceum$BL_z = (Oleraceum$BL - mean(Oleraceum$BL))/sd(Oleraceum$BL)
Oleraceum$Wing_length_z = (Oleraceum$Wing_length - mean(Oleraceum$Wing_length))/sd(Oleraceum$Wing_length)
Omul_m = lm(Oleraceum$OL ~ Oleraceum$BL_z + Oleraceum$Wing_length_z)
summary(Omul_m)
  
Heterophyllum = dat[dat$Hostplant=="Heterophyllum",]
model_1 = lm(OL ~ BL + Wing_length, data = Heterophyllum)
model_2 = lm(OL ~ BL * Wing_length, data = Heterophyllum)
model_3 = lm(OL ~ BL, data = Heterophyllum)
model_4 = lm(OL ~ Wing_length, data = Heterophyllum)

mlist = list(model_1, model_2, model_3, model_4) 
# print(mlist)
AICTab = AIC(model_1, model_2, model_3, model_4)
# print(AICTab)
AICTab$logLik = unlist(lapply(mlist, logLik))
# print(AICTab$logLik)
AICTab = AICTab[order(AICTab$AIC, decreasing=F),]
AICTab$delta = round(AICTab$AIC - min(AICTab$AIC), 2)
# print(AICTab$delta)
lh = exp(-0.5*AICTab$delta)
AICTab$w = round(lh/sum(lh), 2)
AICTab

Hmul = lm(OL ~ BL + Wing_length, data = Heterophyllum)
hist(residuals(Hmul))
summary(Hmul)
Heterophyllum$BL_z = (Heterophyllum$BL - mean(Heterophyllum$BL))/sd(Heterophyllum$BL)
Heterophyllum$Wing_length_z = (Heterophyllum$Wing_length - mean(Heterophyllum$Wing_length))/sd(Heterophyllum$Wing_length)
Hmul_m = lm(Heterophyllum$OL ~ Heterophyllum$BL_z + Heterophyllum$Wing_length_z)
summary(Hmul_m)


Om = lm(OL ~ BL, data = Oleraceum)
summary(Om)
Om_x1 = seq(min(Oleraceum$BL), max(Oleraceum$BL),
            length.out=200)
Om_y1 = Om$coef[1] + Om$coef[2]*Om_x1
plot(Oleraceum$BL, Oleraceum$OL, 
          xlab="BL (mm)", 
          ylab="OL (mm)",
          las=1, pch=21, col="black", bg="lightblue")
lines(Om_x1, Om_y1, lwd = 2)

Hm = lm(OL ~ BL, data = Heterophyllum)
summary(Hm)
Hm_x1 = seq(min(Heterophyllum$BL), max(Heterophyllum$BL),
            length.out=200)
Hm_y1 = Hm$coef[1] + Hm$coef[2]*Hm_x1
points(Heterophyllum$BL, Heterophyllum$OL,
          xlab="BL (mm)",
          ylab="OL (mm)",
          las=1, pch=21, col="black", bg="blue")
lines(Hm_x1, Hm_y1, lwd = 2, lty = 5)  
legend("bottomright", c("Hostplant", "Oleraceum", "Heterophyllum"),
       bty="n", pch=c(NA,21,21), pt.bg=c("NA", "lightblue", "blue"))

OmWing = lm(OL ~ Wing_length, data = Oleraceum)
summary(OmWing)
Om_x2 = seq(min(Oleraceum$Wing_length), max(Oleraceum$Wing_length),
            length.out=200)
Om_y2 = OmWing$coef[1] + OmWing$coef[2]*Om_x2

plot(Oleraceum$Wing_length, Oleraceum$OL, 
     xlab="Wing_length (mm)", 
     ylab="OL (mm)",
     las=1, pch=21, col="black", bg="orchid1")
lines(Om_x2, Om_y2, lwd=2)  

HmWing = lm(OL ~ Wing_length, data = Heterophyllum)
summary(HmWing)
Hm_x2 = seq(min(Heterophyllum$Wing_length), max(Heterophyllum$Wing_length),
            length.out=200)
Hm_y2 = HmWing$coef[1] + HmWing$coef[2]*Hm_x2
points(Heterophyllum$Wing_length, Heterophyllum$OL,
       xlab="Wing_length (mm)",
       ylab="OL (mm)",
       las=1, pch=21, col="black", bg="slateblue")
lines(Hm_x2, Hm_y2, lwd = 2, lty = 5)  
legend("bottomright", c("Hostplant", "Oleraceum", "Heterophyllum"),
       bty="n", pch=c(NA,21,21), pt.bg=c("NA", "orchid1", "slateblue"))

# https://onlinelibrary.wiley.com/doi/10.1111/j.1570-7458.2006.00501.x