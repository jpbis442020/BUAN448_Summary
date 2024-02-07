# Fall 2023
# Homework 4 Logistic Regression

rm(list=ls())
Auction.df <- read.csv(file.choose())
str(Auction.df)

#create dummy variables
for(level in unique(Auction.df$Category)){
  Auction.df[level] <- ifelse(Auction.df$Category == level, 1, 0)
}

for(level in unique(Auction.df$currency)){
  Auction.df[level] <- ifelse(Auction.df$currency == level, 1, 0)
}

for(level in unique(Auction.df$endDay)){
  Auction.df[level] <- ifelse(Auction.df$endDay == level, 1, 0)
}



## part a
pivot.Category <- aggregate(Auction.df$Competitive., by = list(Auction.df$Category), FUN = mean)
names(pivot.Category) <- c("Category", "MeanCompetitive")
pivot.currency <- aggregate(Auction.df$Competitive., by = list(Auction.df$currency), FUN = mean)
names(pivot.currency) <- c("currency", "MeanCompetitive")
pivot.endDay <- aggregate(Auction.df$Competitive., by = list(Auction.df$endDay), FUN = mean)
names(pivot.endDay) <- c("endDay", "MeanCompetitive")
pivot.Duration <- aggregate(Auction.df$Competitive., by = list(Auction.df$Duration), FUN = mean)
names(pivot.Duration) <- c("Duration", "MeanCompetitive")

# from the pivot.currency Table, 'EUR' and 'US' are similar.
# Create dummy variables for Currency;
Auction.df$USEUR <- Auction.df$US + Auction.df$EUR


# Please answer part (b) to part (d) 
#b and c
#initalize train and valid dfs from the Aunction.df
set.seed(12345)
train.index <- sample(1:1972, 1183)
train.df <- Auction.df[train.index, ]
valid.df <- Auction.df[-train.index, ]

# logistic models
expricemodel <-glm(Competitive.~ . - ClosePrice, data = train.df)
summary(expricemodel)
wholemodel <- glm(Competitive. ~ ., data = train.df)
summary(wholemodel)

# predicted values from each model
predicted_acc_exprice <- predict(expricemodel, valid.df, type="response")
summary(predicted_acc_exprice)
predicted_acc_whole <- predict(wholemodel, valid.df, type="response")
summary(predicted_acc_whole)



#accuracy using accuracy function
accuracy1 <- accuracy(predicted_acc_exprice, valid.df$Competitive.)
accuracy2 <- accuracy(predicted_acc_whole, valid.df$Competitive.)
summary(accuracy1)
summary(accuracy2)

#setting cutoff of .5
valid.df$prediction <- ifelse(predicted_acc_exprice >= .5, 1, 0)
valid.df$prediction2 <- ifelse(predicted_acc_whole >= .5, 1, 0)
summary(valid.df$prediction)
summary(valid.df$prediction2)


#creating confusion matrix for both models
#counting all correctly predicted values and finding the % of the values that were correctly predicted for both models
table(valid.df$prediction, valid.df$Competitive.)
sum(valid.df$prediction == valid.df$Competitive.)
mean(valid.df$prediction == valid.df$Competitive.)

table(valid.df$prediction2, valid.df$Competitive.)
sum(valid.df$prediction2 == valid.df$Competitive.)
mean(valid.df$prediction2 == valid.df$Competitive.)


#model comparisons
# comparing the predicted values excluding price(valid.df$prediction) vs whole model (valid.df$prediction2)
# the MIN(0.0), 1st Quartile(0.0), median(1.0), 3rd Quartile(1.0)  and max(1.0) are all the same value
# the only value that is different is the mean. .601 when excluding price and .597 when not
# when using the accuracy function on the model that excludes price the ME:.00476, RSME:.473, and MAE:.441. 
# when using the accuracy function on the model that doesn't excludes price the ME:.00291, RSME:.462, and MAE:.441. 
# MPE and MAPE are NA and Inf respectively for both models
# when using the sum function for the model that excludes the price the sum of correctly predicted values is 506
#using the mean function for the model that excludes the price the percentage of values that were predicted correctly is %64.132
# when using the sum function for the model that excludes the price the sum of correctly predicted values is 529
#using the mean function for the model that excludes the price the percentage of values that were predicted correctly is %64.047    22


#d
# Display the confusion matrices
print("Confusion Matrix for 'model excluding price':")
table(valid.df$prediction, valid.df$Competitive.)

print("Confusion Matrix for 'the whole model':")
table(valid.df$prediction2, valid.df$Competitive.)

