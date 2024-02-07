#a
# the data should be partitioned to determine if the model works correctly 
#the training set is to be used to train the model
#the validation set is to be used to validate if the model is working correctly 
bostondf <- read.csv(file.choose())
set.seed(12345)
train.index <- sample(1:506, 304)
train.df <- bostondf[train.index, ]
m1 <- lm(MEDV ~ CRIM + CHAS + RM, data = train.df)
summary(m1)
#b
# equation
#MEDV = -27.6003 + ( -0.2589 * CRIM) + (2.8099 * CHAS) + (8.1283 * RM)
#c
# predicted MEDV $21.14
new_observation <- data.frame(
  CRIM = 0.1,  # Crime rate
  CHAS = 0,    # Not bound by Charles River
  RM = 6       # Average number of rooms per house
)

predicted_MEDV <- predict(m1, newdata = new_observation)
cat("Predicted Median House Price (MEDV): $", round(predicted_MEDV, 2), "\n")

#d-1
#INDUS and NOX: These two variables could be correlated because areas with a higher proportion of non-retail business land (INDUS) may also have more industrial activity, 
#which could lead to higher emissions of nitrogen oxides (NOX). 
#In areas with extensive commercial and industrial development, both INDUS and NOX might increase together.
#TAX and NOX: There might be an indirect relationship between property tax rate (TAX) and nitrogen oxide concentration (NOX). 
#Higher property tax rates could lead to more funding for environmental regulations and enforcement, which could result in lower levels of pollution such as NOX. 
#However, this relationship might not be as direct as the one between INDUS and NOX.

cor_matrix <- cor(bostondf)
print(cor_matrix)
# Tax and RAD seem to measure the same thing since both of their correlations are above .9
#d-2
# Load the 'car' package for VIF calculation
#install.packages("car")
library(car)

# Calculate VIF for the predictors
vif_values <- vif(m1)
install.packages("corrplot")

library(corrplot)


selectedd_columns <- train.df[, c('CRIM', 'ZN', 'INDUS', 'NOX', 'RM', 'AGE', 'DIS', 'RAD', 'TAX', 'PTRATIO', 'LSTAT', 'MEDV')]
correlation_matrix <- cor(selectedd_columns)
corrplot.mixed(correlation_matrix)


# Print the VIF values
print(vif_values)
print(cor_matrix)
# tax or RAD either should be removed preferably the one with the higher correlation to the other variables
# RAD is the one to remove
#bostondf <- bostondf[, !(names(bostondf) == "RAD")]
#d-3

train.df <- bostondf[train.index, ]
train.index <- sample(1:506, 304)
valid.df <-bostondf[-train.index,]

#step wise forward, back ward and both on training set

n<-lm(MEDV ~ 1, data=train.df)

#step wise forward, back ward and both on validation set
m3 <- lm(MEDV ~., data=valid.df)
summary(m3)
backm3<- step(m3, direction="backward")
summary(backm3)
fowm3<- step(m3, arg="foward")
summary(fowm3)
bom3<- step(m3, direction="both")
summary(bom3)


#step wise forward, back ward and both on validation set
fo9 <- step(n, scope=list(lower=n, upper=m3), arg="foward")
ba9 <- step(m3, arg = 'backward')
bo9 <- step(n, scope=list(upper=m3), data=train.df, arg="both")


summary(ba9)
summary(bo9)
summary(fo9)

predd_backward <- predict(ba9, valid.df)
predd_forward <- predict(fo9, valid.df)
predd_both <- predict(bo9, valid.df)



library(forecast)


accuracyy_backward <- accuracy(predd_backward, valid.df$MEDV)
accuracyy_forward <- accuracy(predd_forward, valid.df$MEDV)
accuracyy_both <- accuracy(predd_both, valid.df$MEDV)


# Print & compare accuracy values
summary(accuracyy_backward)
summary(accuracyy_forward)
summary(accuracyy_both)

#backward RSME=3.51 ME=-0.0000000000000154. MAPE=13.5
#forward RSME=3.66 ME=-0.277. MAPE=14.4
#both RSME=3.66 ME=-0.277. MAPE=14.4
#both and forward are the same while backward is different
#based on the adjusted rsquared step backward model is the best
