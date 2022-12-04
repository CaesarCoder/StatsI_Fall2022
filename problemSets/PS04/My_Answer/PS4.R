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


# d) interpretation: 


# e) 


# f) 

y1 = 0.003 * 1000 - 0.002*1000*1
# y1 = 1


# g)

y2 = 37.781 * 1 - 0.002 * 6000 *1 
# y2 = 25.781



# Q2
# a)


# Hypothesis: Ho: 
sd1 = 0.016
ts1 <- (0.042-0) /0.016
ts1
# test_statistics = 2.625
p_value1 <- 2*pt(abs(ts1), 128, lower.tail = FALSE)
p_value1
# p_value1 =  0.00972002




# b)
sd2 = 0.013
ts2 = (0.042-0) /0.013
ts2
# test_statistics = 3.231
p_value2 <- 2*pt(abs(ts2), 128, lower.tail = FALSE)
p_value2
# p_value2 =  0.00156946