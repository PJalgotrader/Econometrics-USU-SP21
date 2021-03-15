library(wooldridge)
library(stargazer)



MRM1 <- lm(wage ~ educ+ exper + IQ + sibs , wage2)
stargazer(MRM1, type = "text")


# let's test if education is relevant i.e H0: b_educ = 0 vs H1: b_educ != 0 

# steps: 1) set alpha    2) look at hypothesis: it is two tailed test  3) find the t_values 4) compare t and c 


alpha <-  0.05

t_educ <-  (56.814 - 0 ) / 7.106 
t_educ


# let's find the critical value

c <- qt(0.975,930)
c

# what is your conclusion: because t > c then we reject the null at 5% significane level


# alternatively you could use summary() function. 
summary(MRM1)

#----------------------------------------------------------------------------------


# let's test if sibs is relevant and has negative effect on wage i.e H0: b_sibs = 0 vs H1: b_sibs < 0  

alpha <- 0.10

t_sibs <- ( -8.148 - 0)  / 5.526
t_sibs


c <- qt(0.1 , 930)
c


# let's test sibs with two tailed test

qt(0.05, 930)


# what do you conclude? at 5% sig level we fail to reject the null i.e. sibs is irrelevant. 



# --------------------------------------------------------------------------------------


MRM2 <-  lm(wage ~ educ+ exper + IQ  , wage2)
stargazer(MRM1, MRM2, type = "text")



cor(wage2$wage, wage2$sibs)











