#### 37301 Data Driven Marketing ######
#### Homework 4 #######

library(psych)
library(lattice)
library(plyr)

setwd("/Users/ben/Dropbox/Chicago Booth/37103 Data Driven Marketing/Homework 4/37103-HW4")
load("Hellmans.RData")

########################################################
################## Question 1 ##########################

# Look at a summary of the data
summary(hellmans_DF)

# Create a variable for price
hellmans_DF$price = hellmans_DF$sales_dollars/hellmans_DF$sales_units

# Divide the percentage variables by 100
hellmans_DF$feature_pctacv = hellmans_DF$feature_pctacv/100
hellmans_DF$display_pctacv = hellmans_DF$display_pctacv/10

# Create a historgam by account
histogram(~feature_pctacv | account, data = hellmans_DF, type = "count", main = "Histogram of Feature Percent")
histogram(~display_pctacv | account, data = hellmans_DF, type = "count", main = "Histogram of Feature Percent")

# These promotional instruments differ in that feature is typically either 100% or zero.  
# Display is more likely to be in 8%, and is evenly distrubuted above that level

# Calculate correlations between variables
cor(hellmans_DF$feature_pctacv, hellmans_DF$display_pctacv)
cor(hellmans_DF$feature_pctacv, hellmans_DF$price)
cor(hellmans_DF$display_pctacv, hellmans_DF$price)

# Feature and display are highly correlated. The are both negatively correclated with price.
# This presents a potential problem because we may not be able to tell if it is price decreases or promotions that are driving sales.

########################################################
################## Question 2 ##########################

# Run a regression with only price
reg1.1 <- lm(log(sales_units) ~ log(price), data = hellmans_DF[which(hellmans_DF$account == "Dominicks"),])
summary(reg1.1)
reg1.2 <- lm(log(sales_units) ~ log(price), data = hellmans_DF[which(hellmans_DF$account == "Jewel"),])
summary(reg1.2)


# Run a regression using price, feature and display
reg2.1 <- lm(log(sales_units) ~ log(price) + display_pctacv + feature_pctacv, data = hellmans_DF[which(hellmans_DF$account == "Dominicks"),])
summary(reg2.1)
reg2.2 <- lm(log(sales_units) ~ log(price) + display_pctacv + feature_pctacv, data = hellmans_DF[which(hellmans_DF$account == "Jewel"),])
summary(reg2.2)

########################################################
################## Question 3 ##########################

# Create a matrix of the baseline assumption
price <- c(1)
display_pctacv <- c(0)
feature_pctacv <- c(0)
base <- data.frame(price, display_pctacv, feature_pctacv)

# Create scenarios
price <- c(0.85, 0.85, 0.85)
display_pctacv <- c(0, 0.70, 0.70)
feature_pctacv <- c(0, 0, 1)
scenarios <- data.frame(price, display_pctacv, feature_pctacv)

# Run calculation for Dominicks
scenarios$price^(reg2.1$coefficients[2])*exp(scenarios$display_pctacv*reg2.1$coefficients[3])*exp(scenarios$feature_pctacv*reg2.1$coefficients[4])

# Run calculation for Jewel.  Coeffieient on Feature is not statistically significant, so it is set to 0
scenarios$price^(reg2.2$coefficients[2])*exp(scenarios$display_pctacv*reg2.2$coefficients[3])*exp(scenarios$feature_pctacv*0)

# Export regression coefficients
write.csv(summary(reg2.1)$coefficients,"Dominicks Regression.csv")
write.csv(summary(reg2.2)$coefficients,"Jewel Regression.csv")
