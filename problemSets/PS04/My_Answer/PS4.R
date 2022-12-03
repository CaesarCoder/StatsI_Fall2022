nstall.packages(car)
library(car)
data(Prestige)
help(Prestige)
library(stargazer)

#Q1:
  
# a) 

Prestige$professional_dummy <- ifelse(Prestige$type == "prof", 1, 0)  


# b)

# multiple regression with interaction
interact_reg_pres_inc_pro <- lm(prestige ~ income + professional_dummy + income:professional_dummy, data = Prestige)
summary((interact_reg_pres_inc_pro))
stargazer(interact_reg_pres_inc_pro, title="Regression Results with interaction: prestige ~ income + professional_dummy")





# c) prediction equation: 


# d) intepretation: 


