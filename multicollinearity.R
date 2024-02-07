#### BUAN 348/448 Spring 2023
#### Week 7

#-------------------------------------------------------------------------------------
#                          Multicollinearity
#-------------------------------------------------------------------------------------
# Multicollinearity: two or more predictors in a regression model are highly correlated. 

# Example: an extreme example of perfectly collinear data.

my.data <- data.frame(y =  c(12, 13, 10, 5, 7, 12, 15),
                      x1 = c(6, 6.5, 5, 2.5, 3.5, 6, 7.5),
                      x2 = c(6, 6.5, 5, 2.5, 3.5, 6, 7.5))
my.data

# The outcome y is perfectly fitted as y = x1+x2
# But there might be problems because the following models are also correct:

# y=2 * x1
# y=3 * x1 - x2
# y= -400 * x1 + 402 * x2

# If we drop one of the variables from the model, 
# we know exactly what the coefficient of another should be.


#### How to detect multicollinearity?

# Method 1:  Create pair-wise correlation plots among different variables. 

## In most of the cases, variables will have some bit of correlation among each other,
## But high correlation (higher than 0.9) may indicate multicollinearity.

# Method 2:  Use variance inflation factors (VIF) to detect multicollinearity.

# VIF provides an index that measures how much the variance of an estimated regression coefficient is increased because of collinearity. 
## VIF of over 4 indicates that the variables have high correlation among each other. 
## Usually, VIF value of less than 4 is considered good for a model.


# Example:
blood_pressure <- read.csv("bloodpressure.csv")
str(blood_pressure)

# Pt: patient ID
# blood pressure (y = BP, in mm Hg)
# age (x1 = Age, in years)
# weight (x2 = Weight, in kg)
# body surface area (x3 = BSA, in sq m)
# duration of hypertension (x4 = Dur, in years)
# basal pulse (x5 = Pulse, in beats per minute)
# stress index (x6 = Stress)

library(corrplot) 
cmat2 <- cor(blood_pressure)
corrplot.mixed(cmat2)

# `Blood pressure` is related fairly strongly to Weight (r = 0.950) and BSA (r = 0.866), 
#  and hardly related at all to Stress level (r = 0.164).

# Weight and BSA appear to be strongly related (r = 0.875), 

# The high correlation among some of the predictors suggests that data-based multicollinearity exists.

# Let's take a look at the impact of the multicollinearity on regression analysis. 

lm1 <- lm(BP ~ Weight, data = blood_pressure)
lm2 <- lm(BP ~ BSA, data = blood_pressure)
lm3 <- lm(BP ~ Weight + BSA, data = blood_pressure)

lm1
lm1$coefficients["Weight"]
# lm1: BP = 2.205 + 1.201 * Weight 

lm2
lm2$coefficients["BSA"]
# lm2: BP = 45.18 + 34.44 * BSA 

lm3
lm3$coefficients["Weight"]
lm3$coefficients["BSA"]
# lm3: BP = 5.653 + 1.039 * Weight + 5.831 * BSA

#Note that, depending on which predictors we include in the model, 
#we obtain wildly different estimates of the slope parameter for "BSA"

# If x = BSA is the only predictor included in our model (lm2):
# for every additional one square meter increase in body surface area (BSA), 
# blood pressure (BP) increases by 34.4 mm Hg.

# If x1 = Weight and x2 = BSA are both included in our model (lm3):
# for every additional one square meter increase in body surface area (BSA), 
# holding weight constant, blood pressure (BP) increases by only 5.83 mm Hg.


# Method 2: VIF (Variance Inflation Factors)

# inclide all the variables as predictors in lm:
lm4 <- lm(BP~.,data=blood_pressure)
lm4

#calculate the VIF for each predictor
library(car) 
vif(lm4)


# Three of the variance inflation factors are greater than 4. 
# EX: The VIF for the predictor `Weight`:
# the variance of the estimated coefficient of Weight is inflated by a factor of 8.42 (high)
# because Weight is highly correlated with at least one of the other predictors in the model.

# we see that the predictors Weight and BSA are highly correlated (r = 0.875). 
# We can choose to remove either predictor from the model. 

lm5 <- lm(BP ~ . -BSA, data = blood_pressure)
lm5

vif(lm5)
library(forecast)