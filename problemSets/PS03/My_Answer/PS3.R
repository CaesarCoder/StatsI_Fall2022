DAT <- read.csv("C:/Users/Caesar/Documents/GitHub/StatsI_Fall2022/datasets/incumbents_subset.csv")

install.packages("stargazer")
library(stargazer)


## Q1: 
LR_VS_DL <- lm(voteshare ~ difflog, data = DAT)
summary(LR_VS_DL)

## p-value for the coeeficient of difflog is 2.2e-16, smaller than 0.001, we can
# reject the null hypothesis that there is no association between voteshare and 
# difflog statistically significant at the 99.9% level. 

stargazer(LR_VS_DL, title="Regression Results: Vote Share ~ Difflog")



RS_LR_VS_DL <- residuals(LR_VS_DL)
print(RS_LR_VS_DL)

png("voteshare ~ difflog.png")
plot(voteshare ~ difflog, data = DAT)
abline(LR_VS_DL)
dev.off()

## prediction equation 
# y = 0.579 + 0.042x 



## Q2:
LR_PV_DL <- lm(presvote ~ difflog, data = DAT)
stargazer(LR_PV_DL, title="Regression Results: Presvote ~ Difflog")

## p-value for the coefficient of difflog is 2.2e-16, smaller than 0.001, we can
# reject the null hypothesis that there is no association between presvote and 
# difflog statistically significant at the 99.9% level. 

# prediction equation: 
# y = 0.508 + 0.024x

png("presvote ~ difflog.png")
plot(presvote ~ difflog, data = DAT)
abline(LR_PV_DL)
dev.off()


RS_LR_PV_DL <- residuals(LR_PV_DL)
print(RS_LR_PV_DL)

## Q3: 
LR_VS_PV <- lm(voteshare ~ presvote, data = DAT)
stargazer(LR_VS_PV, title="Regression Results: voteshare ~ Presvote")

## p-value of coefficient for presvote is 2e-16, smaller than 0.001, we can 
# reject the null hypothesis that there is no association statistically
# significant between voteshare and presvote at the 99.9% level. 


png("voteshare ~ presvote.png")
plot(voteshare ~ presvote, data = DAT)
abline(LR_PV_DL)
dev.off()


## prediction equation: 

# y = 0.441 + 0.388x  

## Q4:

LR_RS_PV_DL_VS_DL <- lm(RS_LR_VS_DL ~ RS_LR_PV_DL, data = DAT)
stargazer(LR_RS_PV_DL_VS_DL, title="Regression Results: voteshare ~ Presvote")

# p-value for the coefficient of residuals from regression model of Q2 is 
# 2e-16 < 0.001, we can reject the null hypothesis that there is no association
# statistically significant between residuals from regression model of Q2 and 
# regression model of Q1 at the 99.9% level.

png("Residual (voteshare ~ difflog) ~ Redisual (presvote ~ difflog).png")
plot(RS_LR_VS_DL ~ RS_LR_PV_DL, data = DAT)
abline(LR_RS_PV_DL_VS_DL)
dev.off()



## prediction equation:
# Y = (2.569e-01)x - 4.860e-18

## Q5: 
LR_VS_DL_PV <- lm(voteshare ~ difflog + presvote, data = DAT)
stargazer(LR_VS_DL_PV, title = "Regression Results: voteshare ~ difflog + presvote")

# p-value of the coefficient for difflog is 2e-16, smaller than 0.001, we can 
# reject the null hypothesis that there is no association statistically 
# significant between vote share and difflog at the 99.9% level.

# p-value of the coefficient for presvote is 2e-16, smaller than 0.001, we can 
# reject the null  hypothesis that there is no association statistically 
# significant between vote share and presvote at the 99.9% level.



# prediction equation:
# Y = 0.449 + 0.036x1 + 0.257x2


## residuals of models from Q4 and Q5 are the same, which equals to 0.073.  
# In the Regression Model of Q4, the residuals from the regression model (Vote
# Share~difflog), is statistically associated with the residuals from the regression
# model (presvote ~ difflog), which means RSS of Q4 refers to the unexplained 
# variations by variables voteshare, difflog and presvote. The residuals of Q5 
# also refers to the unexplained variations by variables voteshare, difflog and 
# presvote. So the residuals of Q4 and Q5 have the same value. 







## stargazer, define function from PS1 


output_stargazer <- function(outputFile, ...) {
  output <- capture.output(stargazer(...))
  cat(paste(output, collapse = "\n"), "\n", file=outputFile, append=TRUE)
}
output_stargazer("regression_output_LR_VS_DL.tex", LR_VS_DL)

