rm(list = ls())
library(MASS)
library(dplyr)
library(psych)
dat = read.csv("~/git/exploring-data/BIOS14/exam2023_data.csv")
meta = read.csv("~/git/exploring-data/BIOS14/exam2023_metadata.csv")
# Convert Date object to factor
dat$Date = as.Date(dat$Date, format = "%d/%m/%Y") 
dat$Date = as.factor(dat$Date) # Convert Date object to factor
dat$Season = as.factor(dat$Season) # Convert Season object to factor
dat$Property = as.factor(dat$Property) # Convert Property object to factor
dat$Aspect = as.factor(dat$Aspect) # Convert Aspect object to factor
dat$Landscape.position = as.factor(dat$Landscape.position) # Convert Landscape.
 # position object to factor

# Assuming the column numbers 10 to 14 represent group_Exotic and 15 to 19
# represent group_Native
# Grouping for pairwise scatterplots
group_Exotic <- dat[, 10:14]
group_Native <- dat[, 15:19]
group_seedling <- dat[, 26:28]

# Use pairs.panels
pairs.panels(dat[, 24:28], lm = TRUE) # seedlings vs distance

# Combine the selected columns
combined_groups_euc0_50 <- cbind(group_Exotic, group_Native, dat$euc_sdlgs0_50cm)
# Use pairs.panels with the combined groups
pairs.panels(combined_groups_euc0_50, lm = TRUE)

# Combine the selected columns
combined_groups_euc50_2 <- cbind(group_Exotic, group_Native, dat$euc_sdlgs50cm.2m)
# Use pairs.panels with the combined groups
pairs.panels(combined_groups_euc50_2, lm = TRUE)

# Combine the selected columns
combined_groups_euc_2m <- cbind(group_Exotic, group_Native, dat$euc_sdlgs.2m)
# Use pairs.panels with the combined groups
pairs.panels(combined_groups_euc_2m, lm = TRUE)

# Summing columns 26 through 28 to create 'group_seedling'
# Grouping for data exploration
dat$group_seedling <- rowSums(dat[, 26:28])
dat$group_Exotic <- rowSums(dat[, 10:14])
dat$group_Native <- rowSums(dat[, 15:19])
ncol(dat) # Number of columns in 'dat'
nrow(dat) # Number of rows in 'dat'
colnames(dat) # Column names in 'dat'
# Removing rows with missing values
dat <- na.omit(dat)

# Check if 'group_seedling' exists in 'dat' and its class
if ("group_seedling" %in% colnames(dat)) {
  print(class(dat$group_seedling))
} else {
  print("group_seedling column not found in 'dat'.")
}

# Running the Poisson regression
model_P0 <- glm(group_seedling ~ group_Exotic + group_Native, data = dat, family = poisson)
model_P1 <- glm(group_seedling ~ group_Exotic * group_Native, data = dat,
               family = poisson)
model_P2 <- glm(group_seedling ~ group_Exotic + group_Native + Euc_canopy_cover,
               data = dat, family = poisson)
model_P3 <- glm(group_seedling ~ group_Exotic + group_Native + Euc_canopy_cover
                 + Distance_to_Eucalypt_canopy.m., data = dat, family = poisson)
model_P4 <- glm(group_seedling ~ group_Native * Euc_canopy_cover,
               data = dat, family = poisson)
model_P5 <- glm(group_seedling ~ group_Native * Distance_to_Eucalypt_canopy.m.,
               data = dat, family = poisson)
model_P6 <- glm(group_seedling ~ group_Exotic * Euc_canopy_cover,
               data = dat, family = poisson)

mlist = list(model_P0, model_P1, model_P2, model_P3, model_P4, model_P5, model_P6)
AICTab = AIC(model_P0, model_P1, model_P2, model_P3, model_P4, model_P5, model_P6)
AICTab$logLik = unlist(lapply(mlist, logLik))
AICTab = AICTab[order(AICTab$AIC, decreasing=F),]
AICTab$delta = round(AICTab$AIC - min(AICTab$AIC), 2)
lh = exp(-0.5*AICTab$delta)
AICTab$w = round(lh/sum(lh), 2)
AICTab

# # Number of Observations Used in Each Model
# nobs(model_P0)
# nobs(model_P2)
# nobs(model_P3)
# nobs(model_P4)
# nobs(model_P5)
# nobs(model_P6)

#
# Compare the Summary of Each Model
# summary(model_P0)
# summary(model_P1)
# summary(model_P2)
summary(model_P3)
# summary(model_P4)
# summary(model_P5)
# summary(model_P6)

# # Check for Missing Values in the Variables Used in the Models
# sum(is.na(dat$group_seedling))
# sum(is.na(dat$group_Exotic))
# sum(is.na(dat$group_Native))
# sum(is.na(dat$Euc_canopy_cover))
# sum(is.na(dat$Distance_to_Eucalypt_canopy.m.))

# Negative Binomial Regression
model_0 <- glm.nb(group_seedling ~ group_Exotic + Distance_to_Eucalypt_canopy.m., 
                  data = dat)
model_1 <- glm.nb(group_seedling ~ group_Exotic + group_Native, data = dat)
model_2 <- glm.nb(group_seedling ~ group_Exotic * group_Native, data = dat)
model_3 <- glm.nb(group_seedling ~ group_Exotic + group_Native + Euc_canopy_cover, 
               data = dat)
model_4 <- glm.nb(group_seedling ~ group_Exotic * Euc_canopy_cover, 
                  data = dat)
model_5 <- glm.nb(group_seedling ~ group_Exotic + group_Native + Euc_canopy_cover
               + Distance_to_Eucalypt_canopy.m., data = dat)
model_6 <- glm.nb(group_seedling ~ group_Native * Distance_to_Eucalypt_canopy.m.,
               data = dat)


mlist = list(model_0, model_1, model_2, model_3, model_4, model_5, model_6)
AICTab = AIC(model_0, model_1, model_2, model_3, model_4, model_5, model_6)
AICTab$logLik = unlist(lapply(mlist, logLik))
AICTab = AICTab[order(AICTab$AIC, decreasing=F),]
AICTab$delta = round(AICTab$AIC - min(AICTab$AIC), 2)
lh = exp(-0.5*AICTab$delta)
AICTab$w = round(lh/sum(lh), 2)
AICTab

# Number of Observations Used in Each Model
# nobs(model_0)
# nobs(model_1)
# nobs(model_2)
# nobs(model_3)
# nobs(model_4)
# nobs(model_5)
# nobs(model_6)

# Compare the Summary of Each Model
# summary(model_0)
# summary(model_1)
# summary(model_2)
summary(model_0)
# summary(model_4)
# summary(model_5)
# summary(model_6)

coefs = summary(model_0)$coef 
exp(coefs[1,1]) #Intercept
exp(coefs[1,1] + coefs[2,1]) # Intercept + Exotic
exp(coefs[1,1] + coefs[3,1]) # Intercept + Distance_to_Eucalypt_canopy.m.

plot(dat$Distance_to_Eucalypt_canopy.m., dat$group_seedling, xlab="Distance to Eucalypt canopy (m)", 
     ylab = "Number of seedlings", main = "Number of Seedlings vs Distance", 
     col = 'grey', las = 1)

newDistance = seq(min(dat$Distance_to_Eucalypt_canopy.m.), 
                  max(dat$Distance_to_Eucalypt_canopy.m.), length.out=200)
newExotic = rep(mean(dat$group_Exotic), length(newDistance)) # Predictor values
y_hat = predict(model_0, newdata = list(group_Exotic = newExotic, 
                                        Distance_to_Eucalypt_canopy.m. = newDistance),
                                        type = "response") 
# Plot predicted values
lines(newDistance, y_hat, lwd=2) 

newExotic2 = rep(mean(dat$group_Exotic)+sd(dat$group_Exotic), length(newDistance))
y_hat2 = predict(model_0, newdata=list(group_Exotic = newExotic2, 
                                       Distance_to_Eucalypt_canopy.m. = newDistance), 
                                       type = "response") 

newExotic3 = rep(mean(dat$group_Exotic)-sd(dat$group_Exotic), length(newDistance))
y_hat3 = predict(model_0, newdata=list(group_Exotic = newExotic3, 
                                       Distance_to_Eucalypt_canopy.m. = newDistance), 
                 type = "response") 
                 
lines(newDistance, y_hat2, lwd=2, col="blueviolet")
lines(newDistance, y_hat3, lwd=2, col="darkgoldenrod")
legend("topleft", lty=1, lwd=2, col=c(1, "blueviolet", "darkgoldenrod"), bty="n",
       legend=c("Exotic = Mean",
                "Exotic = Mean + SD",
                "Exotic = Mean - SD"))
abline(h=exp(coefs[1,1]), lty=2) # Intercept
abline(h=exp(coefs[1,1] + coefs[3,1]), lty=2, col="red") # Intercept + 
                                                        # Distance_to_Eucalypt_canopy.m.


