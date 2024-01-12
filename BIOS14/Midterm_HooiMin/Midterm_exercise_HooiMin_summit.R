rm(list = ls())
# par(mfrow=c(2,2))
dat = read.table("~/git/exploring-data/BIOS14/tephritis.txt", header=T)
dat$Patry = as.factor(dat$Patry)
dat$Hostplant = as.factor(dat$Hostplant)
dat$Sex = as.factor(dat$Sex)
dat$Baltic = as.factor(dat$Baltic)

# library(psych) 
# pairs.panels(dat) # to check how data interact with each other

Oleraceum = dat[dat$Hostplant=="Oleraceum",]
model_1 = lm(OL ~ BL + Wing_length, data = Oleraceum)
model_2 = lm(OL ~ BL * Wing_length, data = Oleraceum)
model_3 = lm(OL ~ BL, data = Oleraceum)
model_4 = lm(OL ~ Wing_length, data = Oleraceum)
# Codes below to investigate the best model based on AIC, loglik, delta, weight
mlist = list(model_1, model_2, model_3, model_4) 
AICTab = AIC(model_1, model_2, model_3, model_4)
AICTab$logLik = unlist(lapply(mlist, logLik))
AICTab = AICTab[order(AICTab$AIC, decreasing=F),]
AICTab$delta = round(AICTab$AIC - min(AICTab$AIC), 2)
lh = exp(-0.5*AICTab$delta)
AICTab$w = round(lh/sum(lh), 2)
AICTab
# model_1 chosen for analysis
Omul = lm(OL ~ BL + Wing_length, data = Oleraceum)
hist(residuals(Omul))
summary(Omul)
# To calculate the variance explained by each of the predictors Oleraceum$BL and Oleraceum$Wing
Oleraceum$BL_z = (Oleraceum$BL - mean(Oleraceum$BL))/sd(Oleraceum$BL)
Oleraceum$Wing_length_z = (Oleraceum$Wing_length - mean(Oleraceum$Wing_length))/sd(Oleraceum$Wing_length)
Omul_m = lm(Oleraceum$OL ~ Oleraceum$BL_z + Oleraceum$Wing_length_z)
summary(Omul_m)
  
Heterophyllum = dat[dat$Hostplant=="Heterophyllum",]
model_1 = lm(OL ~ BL + Wing_length, data = Heterophyllum)
model_2 = lm(OL ~ BL * Wing_length, data = Heterophyllum)
model_3 = lm(OL ~ BL, data = Heterophyllum)
model_4 = lm(OL ~ Wing_length, data = Heterophyllum)
# Codes below to investigate the best model based on AIC, loglik, delta, weight
mlist = list(model_1, model_2, model_3, model_4) 
AICTab = AIC(model_1, model_2, model_3, model_4)
AICTab$logLik = unlist(lapply(mlist, logLik))
AICTab = AICTab[order(AICTab$AIC, decreasing=F),]
AICTab$delta = round(AICTab$AIC - min(AICTab$AIC), 2)
lh = exp(-0.5*AICTab$delta)
AICTab$w = round(lh/sum(lh), 2)
AICTab
# model_1 chosen for analysis
Hmul = lm(OL ~ BL + Wing_length, data = Heterophyllum)
hist(residuals(Hmul))
summary(Hmul)
# To calculate the variance explained by each of the predictors Oleraceum$BL and Oleraceum$Wing
Heterophyllum$BL_z = (Heterophyllum$BL - mean(Heterophyllum$BL))/sd(Heterophyllum$BL)
Heterophyllum$Wing_length_z = (Heterophyllum$Wing_length - mean(Heterophyllum$Wing_length))/sd(Heterophyllum$Wing_length)
Hmul_m = lm(Heterophyllum$OL ~ Heterophyllum$BL_z + Heterophyllum$Wing_length_z)
summary(Hmul_m)

# plotting the graph OL vs BL
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
# plotting the graph OL vs Wing_length
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
# calculate the mean of each variables
aveOL_om <- mean(Oleraceum$OL, na.rm = TRUE)
print(aveOL_om) #1.653429
aveOL_hm <- mean(Heterophyllum$OL, na.rm = TRUE)
print(aveOL_hm) #1.764345
aveBL_om <- mean(Oleraceum$BL, na.rm = TRUE)
print(aveBL_om) #4.432657
aveBL_hm <- mean(Heterophyllum$BL, na.rm = TRUE)
print(aveBL_hm) #4.499933
aveWL_om <- mean(Oleraceum$Wing_length, na.rm = TRUE)
print(aveWL_om) #4.738099
aveWL_hm <- mean(Heterophyllum$Wing_length, na.rm = TRUE)
print(aveWL_hm) #4.802041
#calculate ratio OL/BL
ratioOL_om <- aveOL_om/aveBL_om
print(ratioOL_om) #0.3730107
ratioOL_hm <- aveOL_hm/aveBL_hm
print(ratioOL_hm) # 0.3920825
ratioOL_om_W <- aveOL_om/aveWL_om
print(ratioOL_om_W) #0.3489645
ratioOL_hm_W <- aveOL_hm/aveWL_hm
print(ratioOL_hm_W) #0.3674156