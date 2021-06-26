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

# Explore the data 
summary(train)

# both response variables are continuous so we can potentially just use linear regression for both
clm_lm <- lm(formula = Claims ~ Kilometres + Zone + Bonus + Make + Insured, data = train)
tidy(clm_lm, conf.int = TRUE)

# from output, seems the distance driven is not significant. p value > 5%. Confidence interval includes zero
# refit without Kilometres
clm_lm <- lm(formula = Claims ~ Zone + Bonus + Make + Insured, data = train)
tidy(clm_lm, conf.int = TRUE)

# let's find out if some of these variables are correlated.
correlation <- cor(train)
round(correlation, 2)

# insured amount highly correlated with Claims and Payment
# variables not really correlated with each other


