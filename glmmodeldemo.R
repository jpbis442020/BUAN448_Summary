bank<- read.csv(file.choose())
str(bank)
set.seed(12345)
train.index <- sample(1:5000, 3000)
train.df <- bank[train.index, ]
valid.df <- bank[-train.index, ]
bank <- bank[, !(names(bank) %in% c("ZIP.Code", "ID"))]
table(bank$Education)
bank$Education<-factor(bank$Education, levels=c(1,2,3)
                       labels=c("Undergraduate","Graduate","Advanced"))

m1 <- glm(Personal.Loan ~., data = train.df, family = "binomial")
summary(m1)
summary(m1)
predicted_PL <- predict(m1, valid.df, type="response")
summary(predicted_PL)

# 50% cut off point
valid.df$prediction <- ifelse(predicted_PL >= .5, 1, 0)

#evaluate
table(valid.df$prediction, valid.df$Personal.Loan)
sum(valid.df$prediction == valid.df$Personal.Loan)
mean(valid.df$prediction == valid.df$Personal.Loan)

