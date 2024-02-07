# BUAN 348/448 Spring 2023
# KNN Lab
rm(list=ls())

library(caret)
library(FNN)

bank.df <- read.csv(file.choose())

# remove columns: ID, Zip Code
bank.df <- bank.df[,-c(1,5)]
bank.df$Education <- factor(bank.df$Education, levels = c(1, 2, 3), 
                            labels = c("Education_1", "Education_2", "Education_3"))
for(level in unique(bank.df$Education)) {
  bank.df[level] <- ifelse(bank.df$Education == level, 1, 0)
}

#remove the "old" column: Education
bank.df <- bank.df[,-c(6)]

#Partition data
set.seed(12345)
train.index <- sample(c(1:dim(bank.df)[1]), dim(bank.df)[1]*0.6)
train.df <- bank.df[train.index, ]
valid.df <- bank.df[-train.index, ]

# Normalize Data
train.norm.df <- train.df
valid.norm.df <- valid.df
bank.norm.df <- bank.df

norm.values <- preProcess(bank.df[ ,1:6], method=c("center", "scale"))
train.norm.df[,1:6] <- predict(norm.values, train.df[, 1:6])
valid.norm.df[,1:6] <- predict(norm.values, valid.df[, 1:6])
bank.norm.df[,1:6] <- predict(norm.values, bank.df[, 1:6])

# target value: Personal loan

# Q1: Build a KNN model using k = 1

p1<-knn(
  train=train.norm.df,
  test=valid.norm.df,
  cl=train.norm.df$Personal.Loan,
  k=k
)


# Q2: Find the best k: k = 1, 2, 3, ..., 14
k_values <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14)
for (k in k_values) {
  predictions <- p1<-knn(
    train=train.norm.df,
    test=valid.norm.df,
    cl=train.norm.df$Personal.Loan,
    k=k
  )
  cross_table <- table(Actual = valid.norm.df$Personal.Loan, Predicted = predictions)
  accuracy <- sum(diag(cross_table)) / sum(cross_table)
  print(paste("Accuracy for k =", k, ":", accuracy))
}

# Q3: What's your best k?

#k=1




# Q4: Use the best k you found, predict the following new data record:
new_set <- data.frame(Age = 40, Experience = 10, 
                      Income = 84, Family = 2, 
                      CCAvg = 2, Mortgage = 0, Education_1 = 0,
                      Education_2 = 1, Education_3 = 0,
                       Securities.Account = 0,
                      CD.Account = 0, Online = 1, CreditCard = 1)

new_set[,1:6] <- predict(norm.values, new_set[, 1:6])


predicted_class <- knn(train = train.norm.df[-7],
                       test = new_set,
                       cl = train.norm.df$Personal.Loan,
                       k = 1)

# Display the predicted class
cat("Predicted class for the new record:", predicted_class)




# Q5: What will be the predicted class? Class 1 or Class 0?
#class will be 1
