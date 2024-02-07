tc<- read.csv(file.choose())
pairs(tc[,-c(4,6,7)])
model<-lm(Price~., data=tc)
summary(model)
options(scipen = 999,digits = 5)

tc$Age2 <- tc$Age^2
tc$KM2 <- tc$KM^2

model2<-lm(Price~., data=tc)
summary(model2)

plot(tc$KM, tc$Price,
     xlab = "KM",  # Label for the x-axis
     ylab = "Price",  # Label for the y-axis
     main = "Scatter Plot of Price vs. KM",  # Main title for the plot
     
     pch =0       # Type of point (filled circles)
)









#d(iii)
install.packages("forecast")
library(forecast)
train.df <- BHouse[train.index, ]
valid.df <- BHouse[-train.index, ]


#backward
model1 <-lm(MEDV ~ ., data = train.df) 
lm.backward <- step(model1, direction = "backward")
#Step:  AIC=768.09
#MEDV ~ CRIM + INDUS + CHAS + NOX + RM + AGE + DIS + RAD + TAX + PTRATIO + LSTAT + CAT..MEDV
summary(lm.backward)

lm.backward.pred <- predict(lm.backward, valid.df)
accuracy(lm.backward.pred,valid.df$MEDV)
#.               ME     RMSE      MAE       MPE     MAPE
#Test set -0.2271225 4.339509 3.113757 -6.169425 16.60883

#forward
null <- lm(MEDV ~ 1, data = train.df)
lm.forward<- step(null, scope=list(lower=null, upper=model1), direction="forward")
#Step:  AIC=768.83
#MEDV ~ CAT..MEDV + LSTAT + PTRATIO + RM + CRIM + DIS + NOX + AGE + CHAS
summary(lm.forward)

lm.forward.pred <- predict(lm.forward,valid.df)
accuracy(lm.forward.pred, valid.df$MEDV)

#                 ME     RMSE      MAE       MPE     MAPE
#Test set -0.321012 4.406788 3.211678 -6.727664 17.24002

#both direction
lm.step <- step(null, scope=list(upper=model1), data = train.df, direction="both")
summary(lm.step) 
#Step:  AIC=768.83
#MEDV ~ CAT..MEDV + LSTAT + PTRATIO + RM + CRIM + DIS + NOX + AGE + CHAS
lm.step.pred <- predict(lm.step, valid.df)
accuracy(lm.step.pred, valid.df$MEDV)















