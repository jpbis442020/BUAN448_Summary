#install.packages("ISLR")
library(ISLR)
View(Smarket)
colnames(Smarket)
unique(Smarket$Year)
Smarket_train <- subset(Smarket, Year < 2005)
Smarket_test <- subset(Smarket, Year == 2005)
Smarket_train_x <- Smarket_train[, c("Lag1", "Lag2", "Lag3", "Lag4", "Lag5", "Volume")]
z_score_standardize <- function(data) {
  standardized_data <- scale(data)
  standardized_data_df <- as.data.frame(standardized_data)
  return(standardized_data_df)
}

Smarket_train_z <- z_score_standardize(Smarket_train_x)
Smarket_test_x <- Smarket_test[, c("Lag1", "Lag2", "Lag3", "Lag4", "Lag5", "Volume")]
Smarket_test_z <- z_score_standardize(Smarket_test_x)
Smarket_train_label <- Smarket_train$Direction
Smarket_test_label <- Smarket_test$Direction
library(class)

k_values <- c(1, 3, 5)

for (k in k_values) {
  predictions <- knn(Smarket_train_z, Smarket_test_z, Smarket_train_label, k = k)
  cross_table <- table(Actual = Smarket_test_label, Predicted = predictions)
  accuracy <- sum(diag(cross_table)) / sum(cross_table)
  print(paste("Accuracy for k =", k, ":", accuracy))
}

#k=5 has highest accuracy



