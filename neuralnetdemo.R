cheese.df <- read.csv(file.choose())
str(cheese.df)
set.seed(12345)
library(neuralnet)
model1<- neuralnet(Acceptance~Fat+Salt, data=cheese.df, act.fct="logistic", hidden=c(3,2))
model1
plot(model1)
prediction<- compute(model1, cheese.df)
cheese.df$predict<-prediction$net.result
model1$weights
input1<-.2
input2<-.9
w13<-5.224972
w23<-2.630911
bias3<-4.52066
input3<- bias3+input1*w13+input2*w23
output3<-1/(1+exp(-1*input3))
bias4<- -.2803869
w34<- 4.6829010
input4<-bias4+output3*w34
output4<-1/(1+exp(-1*input4))

