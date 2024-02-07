# BUAN 348/448 Spring 2023
# KNN Lab
rm(list=ls())

library(caret)
library(FNN)

bank.df <- read.csv("UniversalBank.csv")
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





# Q2: Find the best k: k = 1, 2, 3, ..., 14






# Q3: What's your best k?






# Q4: Use the best k you found, predict the following new data record:
new_set <- data.frame(Age = 40, Experience = 10, 
                      Income = 84, Family = 2, 
                      CCAVG = 2, Education_1 = 0,
                      Education_2 = 1, Education_3 = 0,
                      Mortgage = 0, Securities.Account = 0,
                      CD.Account = 0, Online = 1, CreditCard = 1)

# Q5: What will be the predicted class? Class 1 or Class 0?

