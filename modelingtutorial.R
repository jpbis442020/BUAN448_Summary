###### BUAN 348/448 Multiple Linear Regression
#            Fall 2023

# ====================Predictive Modeling =============================
#1. defining goal - Price prediction
#2. getting data

car.df <- read.csv(file.choose())
str(car.df)

#3. preprocessing - Skip

#4. select variable

#### select variables Price, Age_08_04, KM, Fuel_type for regression

selected.df <- car.df[,c("Price","Age_08_04","KM","Fuel_Type")]
  
  head(selected.df)

#5. Select methods - Skip
#6. Run Algorithm

# partition data
set.seed(12345) #set seed for reproducing the partition
# Setting a seed in R means to initialize a pseudo random number generator

# Randomly set train index
1436 * 0.6 # train size

#### make train and validation datasets
train.index <- sample(1:1436, 862)
  
  
  train.index

train.df <- selected.df[train.index, ]
  valid.df <-selected.df[-train.index,]
  
  #### use lm() to run a linear regression of Price on all the predictors in the training set.
  car.lm <- lm(Price~., data=train.df)
  
  car.lm

# use options() to ensure numbers are not displayed in scientific notation
options(scipen = 999)
car.lm

summary(car.lm)

# 7. compare performance - cross validation
## predict in the validation dataset
install.packages("forecast")
library(forecast)

#### use predict() to make predictions on validation data set
car.lm.pred <- predict(car.lm,valid.df)
  car.lm.pred
options(scipen = 999,digits = 5)
car.lm.pred

#### save the residuals for the first 20 observations
some.residuales <- valid.df$Price[1:20]-car.lm.pred[1:20]
  some.residuales

#### create a new data frame with three columns: "Predicted", "Actual", and "residual" (validation dataset)
#### this data frame should have 574 observations
model.result <- data.frame(predicted=car.lm.pred, actual= valid.df$Price,residual=valid.df$Price-car.lm.pred)

  model.result

#### use accuracy() to compute common accuracy measures
### accuracy function gets the predicted and actual price variables
performance <- accuracy(model.result$predicted, model.result$actual)
  performance

#### calculate the following metrics manually
#### Mean Error (ME): Gives an idea of systematic over- or under-prediction
ME<-mean(model.result$actual- model.result$predicted)
#### RMSE: Root-mean-squared-error (validation set)
RSME<-sqrt(mean((model.result$actual- model.result$predicted)**2))
#### MAE: Mean absolute error (deviation)
MAE<-mean(abs((model.result$actual- model.result$predicted)))
#### MPE: Mean percentage error
MPE<-mean(((model.result$actual- model.result$predicted)/model.result$actual))*100
#### MAPE: Mean absolute percent error
MAPE<-mean(abs(model.result$actual- model.result$predicted/model.result$actual*100))

## histogram of residuals

library(ggplot2)
p <- ggplot(data = model.result, aes(x = residual)) 
p + geom_histogram(bins = 20)

#####################
####
# A frequent problem in data mining is that of using a regression equation to
# predict the value of a dependent variable when we have many variables available
# to choose as predictors in our model.
# Given the high speed of modern algorithms for multiple linear regression calculations, ust use all the variables
# in the model. There are several reasons for exercising caution before throwing all possible variables into a model.

# 1 - It may be expensive or not feasible to collect a full complement of predictors for future predictions.
# 2 - We may be able to measure fewer predictors more accurately (e.g., in surveys).
# 3 - The more predictors, the higher the chance of missing values in the data.
# 4 - We obtain more insight into the influence of predictors in models with few parameters.
# 5 - Estimates of regression coefficients are likely to be unstable, due to multicollinearity in models
# with many variables. (Multicollinearity is the presence of two or more predictors
# sharing the same linear relationship with the outcome variable the outcome variable.)
# 6. it might reduce the model performance due to overfitting
# 7. irrelevant 

# 
# The first step in trying to reduce the number of predictors should always be to use
# domain knowledge. It is important to understand what the various predictors are
# measuring and why they are relevant for predicting the outcome variable.
# 
# Some practical reasons for predictor elimination are the expense of collecting this information in the future;
# inaccuracy; high correlation with another predictor; many missing values; or simply irrelevance.

# 
# The next step makes use of computational power and statistical performance
# metrics. In general, there are two types of methods for reducing the number
# of predictors in a model. The first is an exhaustive search for the “best” subset
# of predictors by fitting regression models with all the possible combinations of
# predictors. The exhaustive search approach is not practical in many applications,
# and implementation in R can be tedious and unstable.

# exhaustive search: The idea here is to evaluate all subsets of predictors.
# Since the number of subsets for even moderate values of p is very large, after
# the algorithm creates the subsets and runs all the models, we need some way
# to examine the most promising subsets and to select from them. The challenge
# is to select a model that is not too simplistic in terms of excluding important
# parameters (the model is under-fit), nor overly complex thereby modeling random
# noise (the model is over-fit). 
# 
# Several criteria for evaluating and comparing models are based on metrics computed from the training data:
# One popular criterion is the adjusted R2. Unlike R2, which does not account for the number
# of predictors used, R2 adj uses a penalty on the number of predictors. This avoids
# the artificial increase in R2 that can result from simply increasing the number of
# predictors but not the amount of information.
# 
# A second popular set of criteria for balancing under-fitting and over-fitting
# are the Akaike Information Criterion (AIC) and Schwartz’s Bayesian Information
# Criterion (BIC). AIC and BIC measure the goodness of fit of a model, but also
# include a penalty that is a function of the number of parameters in the model.
# As such, they can be used to compare various models for the same data set. AIC
# and BIC are estimates of prediction error based in information theory. Their
# derivation is beyond the scope of this class, but suffice it to say that models with
# smaller AIC and BIC values are considered better.


# The second approach is to search through a partial set of models.
# The end product is one best subset of predictors . This approach is
# computationally cheaper, but it has the potential of missing “good” combinations of predictors.
# None of the methods guarantee that they yield the best
# subset for any criterion, such as R2 adj. They are reasonable methods for situations
# with a large number of predictors, but for a moderate number of predictors, the
# exhaustive search is preferable.


# 4. variable selection 
#============================ Step-wise Regression=============================
# use step() to run stepwise
# set directions = to backward, forward, or both

car.df <- read.csv("ToyotaCorolla.csv")
#selected variables for regression
selected.df <- car.df[c(3, 4, 7, 8, 9, 10, 12, 13, 14, 17, 18)]

#partition data
set.seed(12345) #set seed for reproducing the partition
train.index <- sample(c(1:1436), 862)
train.df <- selected.df[train.index, ]
valid.df <- selected.df[-train.index, ]

#use lm() to run a linear regression of Price on all 11 predictors in the training set.
#use . after ~ to include all the remaining columns in train.df as predictors
car.lm <- lm(Price ~ ., data = train.df)
#use options() to ensure numbers are not displayed in scientific notation
options(scipen = 999)
summary(car.lm)

#The R function step() can be used to perform variable selection. 

car.lm.back <- step(car.lm, direction="backward")
summary(car.lm.back) #which variable did it drop?

car.lm.back.pred <- predict(car.lm.back, valid.df)
accuracy(car.lm.back.pred, valid.df$Price)

#To perform forward selection we need to begin by specifying a starting model and the range
#of models which we want to examine in the search. 
null <- lm(Price ~ 1, data = train.df)
car.lm.forw <- step(null, scope=list(lower=null, upper=car.lm), direction="forward")

summary(car.lm.forw) #which variable did it drop?
car.lm.forw.pred <- predict(car.lm.forw, valid.df)
accuracy(car.lm.forw.pred, valid.df$Price)

car.lm.step <- step(null, scope=list(upper=car.lm), data = train.df, direction="both")
summary(car.lm.step) #which variable did it drop?
car.lm.step.pred <- predict(car.lm.step, valid.df)
accuracy(car.lm.step.pred, valid.df$Price)

# ==============comparing models using cross validation================================================

car.lm.1 <- lm(Price ~ Age_08_04, data = train.df)
car.lm.2 <- lm(Price ~ Age_08_04 + Weight, data = train.df)
car.lm.3 <- lm(Price ~ Age_08_04 + Weight + Fuel_Type, data = train.df)
car.lm.4 <- lm(Price ~ Age_08_04 + Weight + Fuel_Type +KM, data = train.df)
car.lm.5 <- lm(Price ~ Age_08_04 + Weight + Fuel_Type +KM + Quarterly_Tax, data = train.df)
car.lm.6 <- lm(Price ~ Age_08_04 + Weight + Fuel_Type +KM + Quarterly_Tax + Doors, data = train.df)
car.lm.7 <- lm(Price ~ Age_08_04 + Weight + Fuel_Type +KM + Quarterly_Tax + Doors + HP, data = train.df)

car.lm.1.train.pred <- predict(car.lm.1, train.df)
car.lm.2.train.pred <- predict(car.lm.2, train.df)
car.lm.3.train.pred <- predict(car.lm.3, train.df)
car.lm.4.train.pred <- predict(car.lm.4, train.df)
car.lm.5.train.pred <- predict(car.lm.5, train.df)
car.lm.6.train.pred <- predict(car.lm.6, train.df)
car.lm.7.train.pred <- predict(car.lm.7, train.df)

car.lm.1.train.accuracy <- accuracy(car.lm.1.train.pred, train.df$Price)
car.lm.2.train.accuracy <- accuracy(car.lm.2.train.pred, train.df$Price)
car.lm.3.train.accuracy <- accuracy(car.lm.3.train.pred, train.df$Price)
car.lm.4.train.accuracy <- accuracy(car.lm.4.train.pred, train.df$Price)
car.lm.5.train.accuracy <- accuracy(car.lm.5.train.pred, train.df$Price)
car.lm.6.train.accuracy <- accuracy(car.lm.6.train.pred, train.df$Price)
car.lm.7.train.accuracy <- accuracy(car.lm.7.train.pred, train.df$Price)

car.lm.1.valid.pred <- predict(car.lm.1, valid.df)
car.lm.2.valid.pred <- predict(car.lm.2, valid.df)
car.lm.3.valid.pred <- predict(car.lm.3, valid.df)
car.lm.4.valid.pred <- predict(car.lm.4, valid.df)
car.lm.5.valid.pred <- predict(car.lm.5, valid.df)
car.lm.6.valid.pred <- predict(car.lm.6, valid.df)
car.lm.7.valid.pred <- predict(car.lm.7, valid.df)

car.lm.1.valid.accuracy <- accuracy(car.lm.1.valid.pred, valid.df$Price)
car.lm.2.valid.accuracy <- accuracy(car.lm.2.valid.pred, valid.df$Price)
car.lm.3.valid.accuracy <- accuracy(car.lm.3.valid.pred, valid.df$Price)
car.lm.4.valid.accuracy <- accuracy(car.lm.4.valid.pred, valid.df$Price)
car.lm.5.valid.accuracy <- accuracy(car.lm.5.valid.pred, valid.df$Price)
car.lm.6.valid.accuracy <- accuracy(car.lm.6.valid.pred, valid.df$Price)
car.lm.7.valid.accuracy <- accuracy(car.lm.7.valid.pred, valid.df$Price)

train.valid.rmse.df <- data.frame("Model#" = c(1,2,3,4,5,6,7), "Valid_RMSE" = c(car.lm.1.valid.accuracy[1,2],
                                                                                car.lm.2.valid.accuracy[1,2],
                                                                                car.lm.3.valid.accuracy[1,2],
                                                                                car.lm.4.valid.accuracy[1,2],
                                                                                car.lm.5.valid.accuracy[1,2],
                                                                                car.lm.6.valid.accuracy[1,2],
                                                                                car.lm.7.valid.accuracy[1,2]),
                                  "Train_RMSE" = c(car.lm.1.train.accuracy[1,2],
                                                   car.lm.2.train.accuracy[1,2],
                                                   car.lm.3.train.accuracy[1,2],
                                                   car.lm.4.train.accuracy[1,2],
                                                   car.lm.5.train.accuracy[1,2],
                                                   car.lm.6.train.accuracy[1,2],
                                                   car.lm.7.train.accuracy[1,2]))

# RMSE: Root Mean Square Error
plot(train.valid.rmse.df$Train_RMSE, type = "o",col = "red", xlab = "Model#", ylab = "RMSE", 
     main = "Train vs. Valid RSME")
lines(train.valid.rmse.df$Valid_RMSE, type = "o", col = "blue")



car.lm.1.rsqr <- summary(car.lm.1)$r.squared
car.lm.2.rsqr <- summary(car.lm.2)$r.squared
car.lm.3.rsqr <- summary(car.lm.3)$r.squared
car.lm.4.rsqr <- summary(car.lm.4)$r.squared
car.lm.5.rsqr <- summary(car.lm.5)$r.squared
car.lm.6.rsqr <- summary(car.lm.6)$r.squared
car.lm.7.rsqr <- summary(car.lm.7)$r.squared

train.valid.rmse.df$rsqr <- c(car.lm.1.rsqr,car.lm.2.rsqr,car.lm.3.rsqr,car.lm.4.rsqr,car.lm.5.rsqr,car.lm.6.rsqr,car.lm.7.rsqr)

car.lm.1.adjrsqr <- summary(car.lm.1)$adj.r.squared
car.lm.2.adjrsqr <- summary(car.lm.2)$adj.r.squared
car.lm.3.adjrsqr <- summary(car.lm.3)$adj.r.squared
car.lm.4.adjrsqr <- summary(car.lm.4)$adj.r.squared
car.lm.5.adjrsqr <- summary(car.lm.5)$adj.r.squared
car.lm.6.adjrsqr <- summary(car.lm.6)$adj.r.squared
car.lm.7.adjrsqr <- summary(car.lm.7)$adj.r.squared

train.valid.rmse.df$adjrsqr <- c(car.lm.1.adjrsqr,car.lm.2.adjrsqr,car.lm.3.adjrsqr,car.lm.4.adjrsqr,car.lm.5.adjrsqr,car.lm.6.adjrsqr,car.lm.7.adjrsqr)


plot(train.valid.rmse.df$rsqr, type = "o",col = "red", xlab = "Model#", ylab = "RMSE", 
     main = "Train vs. Valid RSME")

lines(train.valid.rmse.df$adjrsqr, type = "o", col = "blue")









