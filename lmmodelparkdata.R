setwd("~/Desktop/Rclass")

park <- read.csv("customer satisfaction.php", stringsAsFactors = TRUE)
str(park)
park$weekend <- factor(park$weekend)

install.packages("car")
library(car)

scatterplotMatrix(park[,-1])


#some commonly used transformations 
#unit sales, revenue ,income, price:log scale 
#distance : an inverse or log scale 
#right tailed sidtributions: a square root or log 
#left tailed distributions: a quadratic(x^2)


park$logDistance <- log(park$distance)
scatterplotMatrix(park[,-1])


cor(park[,c(2,4:9)])


install.packages("corrplot")
library(corrplot)

cmat<- cor(park[,c(2,4:9)])
corrplot.mixed(cmat)

#let's create a simple linear regression model 
#dependent varieble = overall 
#independent variable = rides 

model1<- lm(overall~rides,data=park)
model1

temp<-summary(model1)


model2 <- lm(overall~rides+clean, data=park)
park$pred <- predict(model1,park)

park$residuals <- park$overall - park$pred
min(park$residuals)
max(park$residuals)
median(park$residuals)

colnames(park2)[1]<-"rides"
predict(model1, data.frame(rides=90))



