bank<- read.csv(file.choose())
set.seed(12345)
bank <- bank[, !(names(bank) %in% c("ZIP.Code", "ID"))]
train.index <- sample(1:5000, 3000)
train.df <- bank[train.index, ]
valid.df <- bank[-train.index, ]
m1 <- glm(Personal.Loan ~., data = train.df, family = "binomial")
summary(m1)
predicted_probabilities <- predict(m1, newdata=valid.df, type = "response")
predicted_labels2 <- ifelse(predicted_probabilities >= 0.5, 1, 0)
accuracy5 <- mean(predicted_labels2 == valid.df$Personal.Loan)
print(accuracy5)


library(class)
k <- knn(train = train.df, test = valid.df, cl = train.df$Personal.Loan, k = 3)

summary(k)
accuracy <- mean(k == valid.df$Personal.Loan) * 100
cat(accuracy, "knn")

k2 <- knn(train = train.df, test = valid.df, cl = train.df$Personal.Loan, k = 4)

summary(k2)
accuracy <- mean(k2 == valid.df$Personal.Loan) * 100
cat(accuracy, "knn")

k3 <- knn(train = train.df, test = valid.df, cl = train.df$Personal.Loan, k = 5)

summary(k3)
accuracy <- mean(k3 == valid.df$Personal.Loan) * 100
cat(accuracy, "knn")

k4 <- knn(train = train.df, test = valid.df, cl = train.df$Personal.Loan, k = 2)

summary(k4)
accuracy <- mean(k4 == valid.df$Personal.Loan) * 100
cat(accuracy, "knn")

k5 <- knn(train = train.df, test = valid.df, cl = train.df$Personal.Loan, k = 1)

summary(k5)
accuracy <- mean(k45 == valid.df$Personal.Loan) * 100
cat(accuracy, "knn")
#best k is 3 with accuracy of 91.55%
# i did this by manually trying k values 1 thru 5

custom_cutoff <- .6
predicted_labels <- ifelse(predicted_probabilities >= custom_cutoff, 1, 0)
accuracy4 <- mean(predicted_labels == valid.df$Personal.Loan)
print(accuracy4)
# best cutoff is .6 i did this by manually testing cutoffs 
#accuracy is 95.65%

custom_cutoff <- .6
predicted_labels <- ifelse(predicted_probabilities >= custom_cutoff, 1, 0)
accuracy4 <- mean(predicted_labels == valid.df$Personal.Loan)
print(accuracy4)

custom_cutoff <- .4
predicted_labels <- ifelse(predicted_probabilities >= custom_cutoff, 1, 0)
accuracy4 <- mean(predicted_labels == valid.df$Personal.Loan)
print(accuracy4)


custom_cutoff <- .7
predicted_labels <- ifelse(predicted_probabilities >= custom_cutoff, 1, 0)
accuracy4 <- mean(predicted_labels == valid.df$Personal.Loan)
print(accuracy4)


custom_cutoff <- .8
predicted_labels <- ifelse(predicted_probabilities >= custom_cutoff, 1, 0)
accuracy4 <- mean(predicted_labels == valid.df$Personal.Loan)
print(accuracy4)


custom_cutoff <- .9
predicted_labels <- ifelse(predicted_probabilities >= custom_cutoff, 1, 0)
accuracy4 <- mean(predicted_labels == valid.df$Personal.Loan)
print(accuracy4)

# best cutoff is .6 i did this by manually testing cutoffs 
#accuracy is 95.65%



#confusion matrix of log regression(m1)
conf_matrix <- table(predicted_labels, valid.df$Personal.Loan)
print("Confusion Matrix:")
print(conf_matrix)

#confusion matrix of knn regression(k)
conf_matrix_knn <- table(k, valid.df$Personal.Loan)
print("Confusion Matrix for KNN Model:")
print(conf_matrix_knn)


new_observation <- data.frame(Age = 40, Experience = 10, Income = 84, Family = 2, CCAvg = 2,
                              Education=2, Mortgage = 0,
                              Securities.Account = 0, CD.Account = 0, Online = 1, CreditCard = 1)

predicted_PersonalLoan <- predict(m1, newdata = new_observation, type = "response")
print(predicted_PersonalLoan)
#.4% which is less than .6 so this person ont get the loan




