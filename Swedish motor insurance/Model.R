# These data were compiled by the Swedish Committee on the Analysis of Risk Premium in Motor Insurance, 
# summarized in Hallin and Ingenbleek (1983) and Andrews and Herzberg (1985). The data are cross-sectional, 
# describing third party automobile insurance claims for the year 1977.
# The outcomes of interest are the number of claims (the frequency) and sum of payments (the severity), 
# in Swedish kroners. Outcomes are based on 5 categories of distance driven by a vehicle, 
# broken down by 7 geographic zones, 7 categories of recent driver claims experience and 9 types of automobile. 
# Even though there are 2,205 potential distance, zone, experience and type combinations (5 x 7 x 7 x 9 = 2,205), 
# only n = 2,182 were realized in the 1977 data set.

library(broom)
library(tidyverse)
library(Metrics)

swedish <- read.csv('SwedishMotor.csv')
swedish

# split into training and test sets

# create an ID
swedish <- swedish %>% mutate(id = row_number())
#Check IDs
head(swedish$id)
#Create training set
train <- swedish %>% 
  sample_frac(.70)
#Create test set
test  <- anti_join(swedish, train, by = 'id')

head(train)

# drop id column
train <- select(train, Kilometres:Payment)
head(train)

# Claims vs Insured
ggplot(data = train, aes(x = Insured,y = Claims)) + 
  geom_point() +
  geom_smooth(method = lm) # positively correlated

# Claims vs Kilometres
ggplot(data = train, aes(x = as.factor(Kilometres),y = Claims)) + 
  geom_boxplot() # Claims might be reducing as you go along the levels but not conclusive visually

# Claims vs Zone
ggplot(data = train, aes(x = as.factor(Zone),y = Claims)) + 
  geom_boxplot() # Some correlation. Higher number of claims in this order: 4, 1, 2, 3, 6, 5, 7

# Claims vs Bonus
ggplot(data = train, aes(x = as.factor(Bonus),y = Claims)) + 
  geom_boxplot() # Some correlation. higher number of claims in this order: 7, 1, 2, 6, ..

# Claims vs Make
ggplot(data = train, aes(x = as.factor(Make),y = Claims)) + 
  geom_boxplot() # Some correlation. higher number of claims in this order: 7, 1, 2, 6, ..

# now let's model. linear model with ols should be fine. We're not dealint with count or binary data.
model1 <- lm(data = train, Claims ~ Insured) # + factor(Kilometres) + factor(Zone) + factor(Bonus) + factor(Make))
tidy(model1, conf.int = TRUE)
summary(model1)
# model with just insured. the insured variable is highly significant


# check whether the outliers belong to a particular variable group
ggplot(data  = train) +
  geom_point(mapping = aes(x = Insured, y = Claims, color = as.factor(Kilometres))) # not noticeable pattern

ggplot(data  = train) +
  geom_point(mapping = aes(x = Insured, y = Claims, color = as.factor(Zone))) # no noticeable pattern

ggplot(data  = train) +
  geom_point(mapping = aes(x = Insured, y = Claims, color = as.factor(Bonus))) # most level sevens have higher insured values and also the highest no of claims

ggplot(data  = train) +
  geom_point(mapping = aes(x = Insured, y = Claims, color = as.factor(Make))) # most level 9s have the highest insured values and also highest no of claims

# how about Kilometres. we want to see if there are any significant differences between the levels
res_aov1 <- aov(Claims ~ factor(Kilometres), data = train)
summary(res_aov1)

# p-value less than 5%, so we can conclude that there are significant differences between the groups

# Same for the other categorical variables

res_aov2 <- aov(Claims ~ factor(Zone), data = train)
summary(res_aov2) # significant differences between levels

res_aov3 <- aov(Claims ~ factor(Bonus), data = train)
summary(res_aov3) # significant differences between levels

res_aov4 <- aov(Claims ~ factor(Make), data = train)
summary(res_aov4) # significant differences between levels


# let's add them to the linear model
model <- lm(data = train, Claims ~ Insured + factor(Kilometres) + factor(Zone) + factor(Bonus) + factor(Make))
summary(model) # some levels are not significant, but overall, they warrant inclusion. The Adjusted R squared increased

# Let's add the most significant ones. 
model3 <- lm(data = train, Claims ~ Insured + factor(Bonus) + factor(Make))
summary(model3)

# prediction time?
# let's make our variables factors.
transmute(data = test, Zone = factor(Zone), Kilometres = factor(Kilometres), Bonus = factor(Bonus), Make = factor(Make))

test$Pred_Claims <- predict(model,test, type = "response")

# rmse?
rmse(test$Claims,test$Pred_Claims)

# compare that with a model with just insured
test$Pred_Claims_Ins <- predict(model1,test, type = "response")
rmse(test$Claims,test$Pred_Claims_Ins)

# compare with model with only significant categorical variables
test$Pred_Claims_Ins_2 <- predict(model3,test, type = "response")
rmse(test$Claims,test$Pred_Claims_Ins_2)

# still higher than the model with all variables


# plot of out predicted values against out actual test values
head(test,50)

ggplot(data  = test,aes(x = Insured, y = Claims)) +
  geom_point(color='blue') +
  geom_line(data = test, aes(x = Insured, y = Pred_Claims),  color = "red")

# there might be some overfitting in the above model

# let's try model with only Insured
ggplot(data  = test,aes(x = Insured, y = Claims)) +
  geom_point(color='blue') +
  geom_line(data = test, aes(x = Insured, y = Pred_Claims_Ins),  color = "red")

# let's try model with just significant categorical variables
ggplot(data  = test,aes(x = Insured, y = Claims)) +
  geom_point(color='blue') +
  geom_line(data = test, aes(x = Insured, y = Pred_Claims_Ins_2),  color = "red") # still seems to be some overfitting

# go with model with all variables, but will probably not do well on other datasets













































