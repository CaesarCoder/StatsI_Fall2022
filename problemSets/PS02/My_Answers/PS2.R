# Q1:
# Part a:
Bribe <- read.csv("C:/Users/Caesar/Documents/GitHub/StatsI_Fall2022/problemSets/PS02/My_Answers/Bribe.csv")

names(Bribe_num) <- Bribe[, 1]
Bribe_num <- Bribe[, -1]
head(Bribe_num)
# First Column: 1 for Upper Class, 2 for Lower Class

prop.table(Bribe_num)*100
# Upper Class Not stopped: 33.33%
# Upper Class bribe requested: 14.29%
# Upper Class Stopped/Given warning: 16.67% 
# Lower Class Not Stopped: 16.67%
# Lower Class bribe Requested: 16.67% 
# Lower Class Stopped/Given warning: 2.38%

exp_Frq_Upper_NS <- sum(Bribe_num[1, ])*sum(Bribe_num[, 1])/sum(Bribe_num)
exp_Frq_Upper_NS

exp_Frq_Upper_BR <- sum(Bribe_num[1, ])*sum(Bribe_num[, 2])/sum(Bribe_num)
exp_Frq_Upper_BR

exp_Frq_Upper_SGW <- sum(Bribe_num[1, ])*sum(Bribe_num[, 3])/sum(Bribe_num)
exp_Frq_Upper_SGW

exp_Frq_Lower_NS <- sum(Bribe_num[2, ])*sum(Bribe_num[, 1])/sum(Bribe_num)
exp_Frq_Lower_NS

exp_Frq_Lower_BR <- sum(Bribe_num[2, ])*sum(Bribe_num[, 2])/sum(Bribe_num)
exp_Frq_Lower_BR

exp_Frq_Lower_SGW <- sum(Bribe_num[2, ])*sum(Bribe_num[, 3])/sum(Bribe_num)
exp_Frq_Lower_SGW

# Expect Frequency Upper Class is not stopped 13.5
# Expect Frequency Upper Class is bribe requested: 8.36
# Expect Frequency Upper Class Stopped/Given warning: 5.14
# Expect Frequency Lower Class Not Stopped: 7.5
# Expect Frequency Lower Class bribe Requested: 4.64 
# Expect Frequency Lower Class Stopped/Given warning: 2.86


TS <-(13.5-14)^2/13.5+(8.36-6)^2/8.36+(5.14-7)^2/5.14+(7.5-7)^2/7.5+(4.64-7)^2/4.64+(2.86-1)^2/2.86
TS
# X^2 sum / Test Statistics is 3.80

df <- (3-1)*(2-1)
df
# Degree of Freedom 2



# Part b:

# Chi square value with degree of freedom 2, α = 0.1 is 4.61
# 3.8 < 4.61 
# Therefore, we cannot reject the null hypothesis that soliciting a bribe by 
# police or not is independent from the class of driver.


## p-value????? 


# Part c: 


Res_Upper_NS <- (13.5-14)^2/sqrt(13.5)
Res_Upper_NS

Res_Upper_BR <- (8.36-6)^2/sqrt(8.36)
Res_Upper_BR

Res_Upper_SGW <- (5.14-7)^2/sqrt(5.14)
Res_Upper_SGW

Res_Lower_NS <- (7.5-7)^2/sqrt(7.5)
Res_Lower_NS

Res_Lower_BR <- (4.64-7)^2/sqrt(4.64)
Res_Lower_BR

Res_Lower_SGW <- (2.86-1)^2/sqrt(2.86)
Res_Lower_SGW


# standardised residuals for Upper Class Not stopped: 0.068
# standardised residuals for Upper Class bribe requested: 1.926
# standardised residuals for Upper Class Stopped/Given warning: 1.526 
# standardised residuals for Lower Class Not Stopped: 0.091
# standardised residuals for Lower Class bribe Requested: 2.586
# standardised residuals for Lower Class Stopped/Given warning: 2.046

# Part d: 



# Q2:

WESTB <- read.csv("https://raw.githubusercontent.com/kosukeimai/qss/master/PREDICTION/women.csv")

# Part a: 
# Null Hypothesis: There is no correlation between Gender of GP leaders and 
# the likelihood of the number of new or repaired drinking water facilities
# in the villages. 

# Part b:

Reg_Gen_Wat <- lm(water ~ female, data = WESTB)
summary(Reg_Gen_Wat)

## how to print the report???


# Part c: 
# The p-value of the coefficient of female is 0.0413, which is lower than 0.05.
# Therefore, we can reject the null hypothesis that there is no correlation 
# between the gender of GP leaders and the likelihood of the number of new or 
# repaired drinking water facilities in the villages at the 95% level. 
# The coefficient of female is 7.864, which indicates that when there is a 
# difference in gender of GP leaders, we expect to see an average difference
# by 7.864 in the number of new or repaired drinking water facilities
# in the villages.





## increase or decrease? 

