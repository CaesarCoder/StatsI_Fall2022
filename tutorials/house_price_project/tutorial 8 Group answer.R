dat <- readRDS("data/train.rds")

library(stargazer)
install.packages("broom")
library(broom)

LM_BATHR <- lm(dat$AdjSalePrice ~ dat$Bathrooms)
summary(LM_BATHR)
dat_add <- augment(LM_BATHR)
stargazer(LM_BATHR, type = "text", title = "Regression Modle Bathroom")

LM_LANDV <- lm(dat$AdjSalePrice ~ dat$LandVal)
dat_add <- augment(LM_LANDV)
summary(LM_LANDV)


getwd()
