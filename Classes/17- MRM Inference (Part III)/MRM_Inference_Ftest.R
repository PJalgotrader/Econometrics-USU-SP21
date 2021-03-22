# Prof. Pedram Jahangiry
# chapter 4: MRM, Inference, F-test

library(wooldridge)
library(stargazer)
library(car)




##############################################################################
##### Testing multiple linear restrictions: The F-test

reg_UR <- lm(log(salary)~years+gamesyr+bavg+hrunsyr+rbisyr, mlb1)
stargazer(reg_UR, type = "text")
summary(reg_UR)


# b3=b4=b5=0
reg_R <- lm(log(salary)~years+gamesyr, mlb1)
stargazer(reg_R, type = "text")
summary(reg_R)

stargazer(reg_UR, reg_R, type = "text")

# automatic calculation of F using linearHypothesis() function

H0 <- c("bavg=0","hrunsyr=0","rbisyr=0")
linearHypothesis(reg_UR, H0)

vif(reg_UR)

### optional : constructing F statistic mannually 

SSR_R  <- sum(resid(reg_R)^2)
SSR_UR <- sum(resid(reg_UR)^2)
q   <- 3
df2 <- nobs(reg_UR) - 5-1

F <- ((SSR_R - SSR_UR)/q)  / (SSR_UR/df2)

##############################################################################
##### Testing overall significance of a regression b1=b2=b3=.....bk=0

reg_UR <- lm(log(salary)~years+gamesyr+bavg+hrunsyr+rbisyr, mlb1)
stargazer(reg_UR, type = "text")


H0 <- c("years=0","gamesyr=0","bavg=0","hrunsyr=0","rbisyr=0")
linearHypothesis(reg_UR, H0)

# extracting F
summary(reg_UR)



##############################################################################
##### Testing general linear restrictions

reg_UR <- lm(log(price)~log(assess)+log(lotsize)+log(sqrft)+bdrms, hprice1)
stargazer(reg_UR, type = "text")
summary(reg_UR)

## you want to see if analysts at Zillow are doing a good job or not?
H0 <- c("log(assess)=1","log(lotsize)=0","log(sqrft)=0","bdrms=0")
linearHypothesis(reg_UR, H0)
# based on the high pvalue = 0.61, it seems that we fail to reject the fact that 
# analysts at Zillow are doing a good job because everything is reflected in assess= Zestimate.

vif(reg_UR)



# let's test the joint significance of everything but assess! 
# are the house properties relevant or not? 
H0 <- c("log(lotsize)=0","log(sqrft)=0","bdrms=0")
linearHypothesis(reg_UR, H0)








