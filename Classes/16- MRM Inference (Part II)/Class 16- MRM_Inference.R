# Pedram Jahangiry

# chapter 4: MRM, Inference

library(wooldridge)
library(stargazer)
library(dplyr)


# Example 4-8
MRM <- lm(log(rd)~ log(sales)+ profmarg, rdchem)
summary(MRM)
stargazer(MRM, type = "text")

# finding critical values 
df    <- nobs(MRM) - 2-1 
alpha <- 0.05

# let's do a two-tailed test
qt(1- alpha/2 , df) 


# Look at t_stat
summary(MRM)$coefficients[ ,'t value' ]

# Confidence Interval  
confint(MRM, level =  1-alpha)


# let's check if H1: b2>0 . say alpha =0.1

# from R out put the t= 1.694 
t <- 1.694

qt(0.90, 29)

# because t > c , we reject the null at 10% significance level. 

##############################################################################
# Single linear combination (LC) of the parameters: 

reg <- lm(lwage~jc+univ+exper, twoyear)
summary(reg)



# H0: b1=b2   H1: b1 != b2
df          <- mutate(totcoll= jc+univ, twoyear)
reg_new     <- lm(lwage~jc+totcoll+exper, df)

summary(reg_new)

stargazer(reg, reg_new, type="text")

# what if we want to test H1: b1 < b2? 

t <- -1.468
qt(0.05, 6759 )


# what is the p-value for one tailed test
pt(-1.468, 6759)

# so the p-value is 7% 

# Constructing 95% confidence interval
confint(reg_new, level =  0.95)

#-------------------------------------------------------

# H0: b2=2*b1   H1: b2 != 2*b1
df          <- mutate(collcomb = 2*univ + jc, twoyear)
reg_new1    <- lm(lwage~collcomb+univ+exper, df)

# another way of doing this:
reg_new2   <- lm(lwage~ I(2*univ+jc)+univ+exper, df)

summary(reg_new1)
summary(reg_new2)

stargazer(reg, reg_new, reg_new1, reg_new2, type="text")


#------------------------------------------------------------------------------------
# later in chapter 6 we will see that we can use the following function as well.
library(car)
linearHypothesis(reg, c("jc-univ=0"))


