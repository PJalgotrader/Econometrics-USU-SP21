## Pedram Jahangiry
# Chapter 2 - The Simple Regression Model




#######################################################################################################
#######################################################################################################
#                     DO NOT PANIC: ALL YOU NEED IS ONE SINGLE LINE OF CODE IN R                      #

summary ( lm  ( y  ~ x , data)) # that's it. All! 

#######################################################################################################
#######################################################################################################


# Now let's have some fun! 



library(wooldridge)
library(dplyr)


# Example 2-3 : CEO salary and ROE

#------------------------------------------------------
# Mannual calculations of OLS coefficient

# ingredients to the OLS formulas 
salary <- ceosal1$salary
roe    <- ceosal1$roe

( n           <- length(salary))
( b1hat       <- cov(roe,salary)/var(roe) )
( b0hat       <- mean(salary) - b1hat*mean(roe) )

salary
( salaryhat   <- b0hat + b1hat*roe  )
( uhat        <- salary - salaryhat)
( SST         <- (n-1) * var(salary))
( SSE         <- (n-1) * var(salaryhat))
( SSR         <- (n-1) * var(uhat))

# Four different ways of calculating R-square
( R2_mehtod1    <- SSE / SST) 
( R2_mehtod2    <- 1- (SSR / SST)  ) 
( R2_mehtod3    <- var(salaryhat) / var(salary)) 
( R2_mehtod4    <- cor(salary,salaryhat)^2)
( R2_method5    <- cor(salary, roe)^2)


#######################################################################################################
#######################################################################################################

# Calculating the OLS coefficients using lm() and summary() 

reg_sal_roe <- lm( salary ~ roe, ceosal1)
summary(reg_sal_roe)


# You can access different parameters in your regression results
(n            <- nobs(reg_sal_roe))
(coefficients <- coef(reg_sal_roe))


# We can use stargazer package in R to generate a better looking output:
# install.packages("stargazer")
library(stargazer)
stargazer(reg_sal_roe, type="text")


# replicating table 2.1
salaryhat <- fitted(reg_sal_roe)
uhat      <- resid(reg_sal_roe) # check if  (resid = y - yhat ) you can try it out by using: y_yhat <- ceosal1$salary - yhat

cbind(roe, salary, salaryhat,uhat)[1:15,  ]




#-----------------------------------------------------------------------------------
## Regression through the origin (forcing the intercept to be equal to 0)
reg_sal_roe_origin <- lm ( salary ~ 0 + roe , ceosal1   )
stargazer(reg_sal_roe, reg_sal_roe_origin, type="text")




#-----------------------------------------------------
# Plotting the data and regression line (SRF)

plot(ceosal1$roe, ceosal1$salary, xlab="ROE" , ylab = "Salary in thousand $", main=" Regressing salary on roe",  col="blue")
abline(reg_sal_roe, col="red" , lwd=3)
abline(reg_sal_roe_origin, col="black" , lwd=3)





## Incorporating non-linearities 

# example: regressing log(wage) on education
reg_wage_educ <- lm(wage~educ, wage1)
reg_logwage_educ <- lm( log(wage) ~ educ, wage1  )
stargazer(reg_wage_educ, reg_logwage_educ, type = "text")
# which one is better?

# you want to see the plots?

par(mfrow=c(1,2)) # seeing both plots in one graph!


# wage vs education plot
plot(wage1$educ, wage1$wage, xlab="Education" , ylab = "Wage", col="blue")
abline(reg_wage_educ, col="red" , lwd=3)

# log(wage) vs education plot
plot(wage1$educ, log(wage1$wage), xlab="Education" , ylab = "Log(wage)", col="blue")
abline(reg_logwage_educ, col="red" , lwd=3)


# Now let's look at the residual histograms

hist(resid(reg_wage_educ))
hist(resid(reg_logwage_educ))


# Do you see the heteroskedasticity when we use wage instead of log(wage)!  
plot(resid(reg_wage_educ), col="blue")
abline(h=0,col="red", lwd=3)

plot(resid(reg_logwage_educ),col="blue")
abline(h=0,col="red",lwd=3)

mean(resid(reg_logwage_educ))



##### correlation and R2

SRM <- lm(wage~educ,wage1)
stargazer(SRM, type = "text")

# r2 = corr(x,y)^2 = corr(y, yhat)^2
cor(wage1$wage, wage1$educ)^2
cor(wage1$wage, predict(SRM))^2


# check for regression through origin
SRM_orig <- lm(wage~0+educ,wage1)
stargazer(SRM_orig, type = "text")

# r2 != corr(x,y)^2 or corr(y, yhat)^2
cor(wage1$wage, wage1$educ)^2
cor(wage1$wage, fitted(SRM_orig))^2


# let's check E(u)=0 or (E(u_hat))=0 in either case! what do you think?

mean(resid(SRM))
mean(resid(SRM_orig)) # why?

par(mfrow=c(1,1))
plot(wage1$educ, wage1$wage, xlab="Education" , ylab = "Hourly wage in $", main=" Regressing wage on educ",  col="blue")
abline(SRM, col="red" , lwd=3)
abline(SRM_orig, col="black" , lwd=3)












