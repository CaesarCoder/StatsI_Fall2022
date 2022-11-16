DAT <- read.csv("C:/Users/Caesar/Documents/GitHub/StatsI_Fall2022/datasets/incumbents_subset.csv")

library(stargazer)

## Q1: 
LR_VS_DL <- lm(voteshare ~ difflog, data = DAT)
summary(LR_VS_DL)

## p-value 2.2e-16, smaller than 0.001, reject the null 

stargazer(LR_VS_DL, title="Regression Results: Vote Share ~ Difflog",
          dep.var.labels=c("Vote Share"), ci=TRUE, ci.level=0.95, single.row=TRUE))







output_stargazer <- function(outputFile, ...) {
  output <- capture.output(stargazer(...))
  cat(paste(output, collapse = "\n"), "\n", file=outputFile, append=TRUE)
}
output_stargazer("regression_output_LR_VS_DL.tex", LR_VS_DL)


RS_LR_VS_DL <- residuals(LR_VS_DL)
print(RS_LR_VS_DL)


plot(voteshare ~ difflog, data = DAT)
abline(LR_VS_DL)


## prediction equation 
# y = 0.579 + 0.042x 



## Q2:
LR_PV_DL <- lm(presvote ~ difflog, data = DAT)
summary(LR_PV_DL)

## p-value 2.2e-16, smaller than 0.001, reject the null 

# prediction equation: 
# y = 0.508 + 0.024x

RS_LR_PV_DL <- residuals(LR_PV_DL)

## Q3: 
LR_VS_PV <- lm(voteshare ~ presvote, data = DAT)
summary(LR_VS_PV)

## p-value of coefficient for presvote = 2e-16, smaller than 0.001, reject the null 

## prediction equation: 

# y = 0.441 + 0.388x  

## Q4:

LR_RS_PV_DL_VS_DL <- lm(RS_LR_VS_DL ~ RS_LR_PV_DL, data = DAT)
summary(LR_RS_PV_DL_VS_DL)

# p-value for resediual = 2e-16 < 0.001, reject null

plot(RS_LR_VS_DL ~ RS_LR_PV_DL, data = DAT)
abline(LR_RS_PV_DL_VS_DL)

## prediction equation:
# Y = -9.985e-19 + 5.062e-01x

## Q5: 
LR_VS_DL_PV <- lm(voteshare ~ difflog + presvote, data = DAT)
summary(LR_VS_DL_PV)
# p-value of the coefficient for difflog = 2e-16 < 0.001, reject null
# p-value of the coefficient for presvote = 2e-16 < 0.001, reject null 
# prediction equation:
# Y = 0.449 + 0.036x1 + 0.257x2


