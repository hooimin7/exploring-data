# setwd("/Users/med-snt/Documents/Rworkshop")
# getwd()
# rm(list = ls())
dat = read.csv("~/git/exploring-data/BIOS14/butterflies.csv")
# head(dat)
names(dat)
dat$MaternalHost = paste0(dat$MaternalHost, "M")
dat$LarvalHost = paste0(dat$LarvalHost, "L")
means = tapply(dat$DevelopmentTime, list(dat$MaternalHost, dat$LarvalHost), mean)
means
m = lm(DevelopmentTime~MaternalHost*LarvalHost, data = dat)
# print(list(dat$MaternalHost, dat$LarvalHost))
# print(summary(m))
anova(m)
res.anova = aov(DevelopmentTime~MaternalHost*LarvalHost, data = dat)
# plot(res.anova)
summary(anova(m))
# print(summary(anova(m)))
se = tapply(dat$DevelopmentTime, 
             list(dat$MaternalHost, dat$LarvalHost), 
             function(x) sd(x)/sqrt(sum(!is.na(x))))
se

plot(c(0.97, 1.03), means[,1], ylim=c(18, 40), xlim=c(0.8, 2.2),
     xlab="Larval host",
     ylab="Developmental time (days)",
     xaxt="n", las=1, pch=c(21,16), col="white")
axis(1, 1:2, labels=c("Barbarea", "Berteroa"))
arrows(c(0.97,1.03), means[,1]-se[,1], c(0.97,1.03),
       means[,1]+se[,1], length=0.05, angle=90, code=3)
arrows(c(1.97,2.03), means[,2]-se[,2], c(1.97,2.03),
       means[,2]+se[,2], length=0.05, angle=90, code=3)

segments(0.97, means[1,1], 1.97, means[1,2], lty=2)
segments(1.03, means[2,1], 2.03, means[2,2], lty=6)
points(c(0.97, 1.03), means[,1], pch=c(23,18), bg="red")
points(c(1.97, 2.03), means[,2], pch=c(23, 18), bg="red")

legend("topleft", c("Maternal host", "Barbarea", "Berteroa"),
       bty="n", pch=c(NA,23,18), pt.bg=c("NA", "red", "NA"))
cM = colMeans(means)
rM = rowMeans(means)
print(cM)
print(rM)

# library(Rmisc)
# library(plyr)
# library(lattice)
# #library(ggpubr)
# summary_stat = summarySE(dat,
#                          Devtime = "DevelopmentTime",
#                          plantvars = c("MaternalHost", "LarvalHost")
#                          )
# ggplot(
#   subset(summary_stat, !is.na(MaternalHost)),
#   aes(x = LarvalHost, y = DevelopmentTime, colour = MaternalHost)
# )
