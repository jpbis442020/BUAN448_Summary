setwd("~/Desktop/Rclass")
data <- read.csv(file.choose())
park <- read.csv("insurance.php", stringsAsFactors = TRUE)
data$sex<- factor(data$sex)
str(data)
table(data$region)
summary(data$expenses)
hist(data$expenses)
cor(data[c(1,3,4,7)])
pairs(data[c(1,3,4,7)])
model1<-lm(expenses~., data)
summary(model1)

data$sex<- relevel(data$sex, ref="male")
summary(model1)
data$age2<- data$age^2
model2<-lm(expenses~., data)
summary(model2)

model3<-lm(expenses~bmi, data)
summary(model3)


data$bmi30 <- ifelse(data$bmi >=30, 1, 0)
model4 <- lm(expenses~., data)
summary(model4)



model5<- lm(expenses~smoker+bmi30, data)
summary(model5)