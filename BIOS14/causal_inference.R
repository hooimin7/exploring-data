rm(list = ls())
plants = read.csv("~/git/exploring-data/BIOS14/alpineplants.csv")
plants = na.omit(plants)
plants = as.data.frame(scale(plants))
round(colMeans(plants), 2)
round(apply(plants, 2, sd), 2)
m1 = lm(Carex.bigelowii ~ snow + min_T_winter + soil_moist, data=plants)
m2a = lm(min_T_winter ~ snow, data=plants)
m2b = lm(soil_moist ~ snow, data=plants)
m2c = lm(Carex.bigelowii ~ min_T_winter + soil_moist, data=plants)
summary(m1)
summary(m1)$coef
summary(m1)$coef[2,1] +
  summary(m1)$coef[3,1]*cor(plants$snow, plants$min_T_winter, "pairwise") +
  summary(m1)$coef[4,1]*cor(plants$snow, plants$soil_moist, "pairwise")
cor(plants$snow, plants$Carex.bigelowii, "pairwise")
summary(m2a)$coef
summary(m2b)$coef
summary(m2c)$coef
library(piecewiseSEM)
m2 = psem(lm(soil_moist~snow, data=plants),
          lm(min_T_winter~snow, data=plants),
          lm(Carex.bigelowii~min_T_winter+soil_moist, data=plants),
          data=plants)
summary(m2)
plot(m2)
#directed separation (“d-separation”), an untested correlation between these variables,
#%~~% operator.
m2b = psem(lm(soil_moist~snow, data=plants),
           lm(min_T_winter~snow, data=plants),
           lm(Carex.bigelowii~min_T_winter+soil_moist, data=plants),
           min_T_winter %~~% soil_moist,
           data=plants)
summary(m2b)
plot(m2b)
m3 = psem(soil_moist~1,
          lm(min_T_winter~snow, data=plants),
          lm(Carex.bigelowii~min_T_winter, data=plants),
          min_T_winter %~~% soil_moist,
          data=plants)
summary(m3)
plot(m3)
