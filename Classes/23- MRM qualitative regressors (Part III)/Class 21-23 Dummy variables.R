# Prof. Pedram Jahangiry 

# Dummy variable 




library(stargazer)
library(car)
library(effects)
library(wooldridge)
library(dplyr)



###############################################################################

# Part 1: # Dummy independent variables 
 
##### using interaction terms vs using multiple categories


head(wage1)

#model 1
reg_categories <- lm(lwage~I(married*(1-female))+ I(married*female) + I((1-married)*female) + educ + exper+I(exper^2) + tenure + I(tenure^2), wage1)
stargazer(reg_categories, type="text")

plot(effect("exper", reg_categories))

#model 2
reg_interaction <- lm(lwage~ female + married + married:female + educ + exper+I(exper^2) + tenure + I(tenure^2), wage1)
stargazer(reg_categories,reg_interaction, type="text")


# model 1 is better if you are interested for testing for wage differentials between any group and the base group
# model 2 allows us to easily test the null hypothesis that the gender differential does not depend on marital status or not. 

reg_final_model <- lm(lwage~ female + married + married:female + educ + exper + tenure + I(tenure^2), wage1)
stargazer(reg_interaction, reg_final_model, type="text")

###############################################################################

# Example 7.11

MRM_dummy_UR <- lm(cumgpa ~ female + sat + female:sat + hsperc + female:hsperc + tothrs + female:tothrs, gpa3, subset= (spring==1)) # the example in the book only applies to the subset of Spring semester.
stargazer(MRM_dummy_UR, type = "text", digits = 4)




# Joint test with F statistic:

#case1: If both the intercept difference and the slope differences are zero. 
linearHypothesis(MRM_dummy_UR, c("female=0", "female:sat=0" , "female:hsperc=0", "female:tothrs=0")) # reject the null

#alternatively you can use matchCoefs() function from "car" package
linearHypothesis(MRM_dummy_UR, matchCoefs(MRM_dummy_UR, "female")) # reject the null



#case2: If the slope differences are zero. 
linearHypothesis(MRM_dummy_UR, c("female:sat=0" , "female:hsperc=0", "female:tothrs=0"))             # fail to reject the null, so a better model is the one that allows for intercept difference only. 


MRM_dummy_R <- lm(cumgpa ~ female + sat +  hsperc +  tothrs , gpa3, subset= (spring==1)) # the example in the book only applies to the subset of Spring semester.
stargazer(MRM_dummy_R, type = "text", digits = 4)




stargazer(MRM_dummy_UR, MRM_dummy_R, type = "text", digits = 4)




############################################################################

# Part 2: # Dummy dependent variables 

MRM_dummy_dep <- lm(inlf ~ nwifeinc + educ + exper + I(exper^2)+age+kidslt6 + kidsge6 , mroz) 
stargazer(MRM_dummy_dep, type = "text", digits = 4)
plot(effect("exper",MRM_dummy_dep))

# let's look at the predicted inlf for the first 10 observations
predict(MRM_dummy_dep)[1:6]


(Percent_correctly_predicted_1       <- sum(predict(MRM_dummy_dep) > 0.5 & mroz$inlf==1) / sum(mroz$inlf==1))
(Percent_correctly_predicted_0       <- sum(predict(MRM_dummy_dep) <= 0.5 & mroz$inlf==0) / sum(mroz$inlf==0))
(Percent_correctly_predicted_overall <- (sum(predict(MRM_dummy_dep) > 0.5 & mroz$inlf==1) + sum(predict(MRM_dummy_dep) <= 0.5 & mroz$inlf==0)) / length(mroz$inlf)) 





# or alternatively
Confusion_Matrix <- table(mroz$inlf, predict(MRM_dummy_dep) >= 0.5)
prop.table(Confusion_Matrix,margin=1)




# Should I be worried using Linear Probability model? no 
dim(mroz)
sum(predict(MRM_dummy_dep)<0) 
sum(predict(MRM_dummy_dep)>1) 
summary(predict(MRM_dummy_dep))

y_pred <- predict(MRM_dummy_dep)
hist(y_pred)


y_pred <- ifelse(y_pred<0, 0 , y_pred)
y_pred <- ifelse(y_pred>1, 1 , y_pred)


hist(y_pred)

# does this change the confusion matrix?
Confusion_Matrix <- table(mroz$inlf, y_pred >= 0.5)
prop.table(Confusion_Matrix,margin=1)

accuracy <- (203+350)/(203+122+78+350) # this is the overall significance
accuracy
########################

# for HW10 question 5: How to define a binary variable ecobuy
head(apple)
df <- mutate(apple, ecobuy = ifelse(ecolbs>0,1,0))

head(df)


LPM <- lm(ecobuy ~ ecoprc + regprc + faminc + hhsize + educ + age, df)
stargazer(LPM, type = "text")



############################# Optional: Logistic Regression (Chapter 17) ##########################
Logistic <- glm(inlf ~ nwifeinc + educ + exper + I(exper^2)+age+kidslt6 + kidsge6, family = binomial, mroz)
stargazer(Logistic, type = "text", digits = 4)
predict(Logistic, type = "response")

Confusion_Matrix_logistic <- table(mroz$inlf, predict(Logistic, type = "response") >= 0.5)
prop.table(Confusion_Matrix_logistic,1)

