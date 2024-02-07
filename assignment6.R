library(neuralnet)
library(caret)
library(forcast)

car.df <- read.csv(file.choose())


predictors <- car.df[, c("Age_08_04", "KM", "Fuel_Type", "HP", "Automatic", "Doors", "Quarterly_Tax", "Mfr_Guarantee", "Guarantee_Period", "Airco", "Automatic_airco", "CD_Player", "Powered_Windows", "Sport_Model", "Tow_Bar")]


target <- car.df$Price
table(car.df$Fuel_Type)


dummy_variables <- model.matrix(~Fuel_Type - 1, data = car.df)

# Rename the columns to be more descriptive
colnames(dummy_variables) <- c("Fuel_Type_CNG", "Fuel_Type_Diesel", "Fuel_Type_Petrol")
car.df <- cbind(car.df, dummy_variables)
car.df <- car.df[, c("Price","Age_08_04", "KM", "Fuel_Type_CNG", "Fuel_Type_Diesel", "Fuel_Type_Petrol", "HP", "Automatic", "Doors", "Quarterly_Tax", "Mfr_Guarantee", "Guarantee_Period", "Airco", "Automatic_airco", "CD_Player", "Powered_Windows", "Sport_Model", "Tow_Bar")]

# Remove the original Fuel_Type column
car.df$Fuel_Type <- NULL


set.seed(123)
train.index <- sample(c(1:nrow(car.df)), nrow(car.df)*0.75)
train <- car.df[train.index, ]
valid <- car.df[-train.index, ]

preprocessed_data <- preProcess(predictors, range = c(0, 1))



model <- neuralnet(Price ~ ., data = train, hidden = c(2))
model2 <- neuralnet(Price ~ ., data = train, hidden = c(5))
model3 <- neuralnet(Price ~ ., data = train, hidden = c(5,5))



train_predictions <- predict(model, train)
valid_predictions <- predict(model, valid)


train_predictions2 <- predict(model2, train)
valid_predictions2 <- predict(model2, valid)

train_predictions3 <- predict(model3, train)
valid_predictions3 <- predict(model3, valid)

h2<-sqrt(mean((train_predictions - train$Price)^2))
h5<-sqrt(mean((train_predictions2 - train$Price)^2))
h55<-sqrt(mean((train_predictions3 - train$Price)^2))

print(h2)
print(h5)
print(h55)

#i
#the RSME is the same for hidden layer 2 and 5 and RSME increases for hidden layer 5 ,5
hl2<-sqrt(mean((valid_predictions - valid$Price)^2))
hl5<-sqrt(mean((valid_predictions2 - valid$Price)^2))
hl55<-sqrt(mean((valid_predictions3 - valid$Price)^2))

print(hl2)
print(hl5)
print(hl55)


#ii
##the RSME is the same for hidden layer 2 and 5 and RSME increases for hidden layer 5 ,5

#iii
# to reduce over fitting and to keep RMSE as low as possible hidden layer 5 whould be the best model
# number of layers is 1, number of nodes are 5
