concrete.df <- read.csv(file.choose())
str(concrete.df)
set.seed(12345)

normalize<- function(x){
  return ((x-min(x))/(max(x)-min(x)))
}

concrete_norm<-as.data.frame(lapply(concrete.df, normalize))
summary(concrete_norm$cement)

RNGversion("3.5.2")
train.index <- sample(c(1:nrow(concrete_norm)), nrow(concrete_norm)*0.75)
concrete_train <- concrete_norm[train.index, ]
concrete_valid <- concrete_norm[-train.index, ]
  

library(neuralnet)
concrete_model<- neuralnet(strength~., data=concrete_train)
plot(concrete_model)
prediction<- compute(concrete_model, concrete_valid)

concrete_valid$prediction<-prediction$net.result
cor(concrete_valid$prediction, concrete_valid$strength)


concrete_model2<- neuralnet(strength~., data=concrete_train, hidden=c(5))
plot(concrete_model2)
prediction2<- compute(concrete_model2, concrete_valid)

concrete_valid$prediction2<-prediction2$net.result
cor(concrete_valid$prediction2, concrete_valid$strength)
 