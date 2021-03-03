# Class 12 F2F:

library(wooldridge)
library(stargazer)
library(dplyr)

#--------------------------------------------------
df<- saving

head(df)
str(df)


# Simple regression model

SRM <- lm(cons~ inc, df)
stargazer(SRM, type = 'text')


# Multiple Regression Model

MRM <- lm(cons~ inc   + age, df)
stargazer(SRM,MRM, type = 'text')



# -------------------------- Now let's try to answer your questions

# 1. How do I know when to use quadratic and when log transformation (bending the data points)?
# 2. I would like to do some examples in class of finding the OLS equation and interpreting each intercept and coefficient.
 

# let's do some plots first:

plot(df$inc, df$cons) # do you see the non-linear relationship between inc and cons? what can we do?
plot(df$age, df$cons) # do you see the heteroskedasticity, what can we do? try log(cons)


# quadratic or log? 

# starting with quad
# 3. Why do we need to pass both ???? and ????^2 to the quadratic model?
# 8. when do we use "I" in R?
MRM_quad <-  lm(cons~ inc + I(inc^2)+ age , df)
stargazer(SRM,MRM,MRM_quad ,type = 'text')

# let's try log now?
MRM_log <-  lm(log(cons)~ log(inc) + age , df)
stargazer(SRM,MRM,MRM_quad, MRM_log ,type = 'text')
# so which one is the winner? how do you interpret the coef of log(inc).

#------------------------------------------------------------

# 4. I was confused about the derivative part. The derivative is only measuring the slope at one instantaneous point, right? It is not giving us a new model, is it?
MRM1 <-  lm(cons~ I(inc/1000) + I((inc/1000)^2)+ age , df)
stargazer(MRM1 ,type = 'text') 




# 5. I understand the definition of multicollinearity but I think an example would help to wrap my mind around it.

cor(df$inc, df$educ)

MRM2 <- lm(cons~ inc   + age, df)
MRM3 <- lm(cons~ inc   + age + educ, df)
stargazer(MRM2, MRM3 ,type = 'text') 

# 8. could you explain endogeneity and exogeneity more. 
resid2 <- resid(MRM2)
cor(resid2, df$educ) # this means that educ is endogenous! 



# ----------------------------------------------

# 6. I could use an example of how exactly to do step 1 and 2 in the partialing out concept in R

MRM4 <- lm(cons~ inc   + age + educ, df)
stargazer(MRM4, type = 'text')

# for example we want to calculate the partial effect of inc on cons in two steps (we know the answer is 0.892)
# step 1) Regress the explanatory variable on all other explanatory variables

step1 <- lm(inc~ age+educ , df)
stargazer(step1, type = 'text')
resid1 <- resid(step1)

# step 2) Regress y on the residuals from the regression in step 1

step2 <- lm(cons ~ resid1, df)
stargazer(step2, type = 'text')  # did you get the 0.892? yay!! 


# ---------------------------------------------------

# 7. I don't understand why total variance increases with sample size, wouldn't the opposite be true?
# 9. What is the difference between ????^2 and adjusted ????^2?























