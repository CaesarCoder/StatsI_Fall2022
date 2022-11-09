DAT <- read.csv("C:/Users/Caesar/Documents/GitHub/StatsI_Fall2022/datasets/incumbents_subset.csv")


## Q1: 
LR_VS_DL <- lm(voteshare ~ difflog, data = DAT)
summary(LR_VS_DL)

## p-value 2.2e-16, smaller than 0.001, reject the null 


library(stargazer)
output_stargazer <- function(outputFile, ...) {
  output <- capture.output(stargazer(...))
  cat(paste(output, collapse = "\n"), "\n", file=outputFile, append=TRUE)
}
output_stargazer("regression_output_LR_VS_DL.tex", LR_VS_DL)


RS_LR_VS_DL <- residuals(LR_VS_DL)
print(RS_LR_VS_DL)


plot(voteshare ~ difflog, data = DAT)
abline(LR_VS_DL )


## prediction equation 
# y = 0.579 + 0.042x 



## Q2:
LR_PV_DL <- lm(presvote ~ difflog, data = DAT)
summary(LR_PV_DL)

## p-value 2.2e-16, smaller than 0.001, reject the null 



## Q3: 
LR_VS_PV <- lm(voteshare ~ presvote, data = DAT)
summary(LR_VS_PV)

## p-value 2.2e-16, smaller than 0.001, reject the null 



