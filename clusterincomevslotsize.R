mower.df <- read.csv(file.choose())
RNGversion("3.5.2")
set.seed(12345)

train.index <- sample(c(1:nrow(mower.df)), nrow(mower.df)*0.6)
train.df <- mower.df[train.index, ]
valid.df <- mower.df[-train.index, ]

plot(Lot_Size~Income, data=train.df,pch=c(1,3))
text(train.df$Income, train.df$Lot_Size+.3,pch=c(1,3,4))
text(60, 20,"X")
legend("topright", c("owner","non-owner","new"),pch=c(1,3,4))
summary(train.df[c(1,2)])

#normalizing income and lotsize
#Income
# mean and sd of train$Income
income_mean <- mean(train.df$Income)
income_std <- sd(train.df$Income)

train.df$Income_n <-(train.df$Income-income_mean)/income_std
summary(train.df$Income_n)

#lotsize
Lot_Size_mean <- mean(train.df$Lot_Size)
Lot_Size_std <- sd(train.df$Lot_Size)

train.df$Lot_Size_n <-(train.df$Lot_Size-Lot_Size_mean)/Lot_Size_std
summary(train.df$Lot_Size_n)

valid.df$Income_n <-(valid.df$Income-income_mean)/income_std
valid.df$Lot_Size_n <-(valid.df$Income-income_mean)/Lot_Size_std

install.packages("FNN")
library(FNN)
mower.pred <- knn(train=train.df[c("Income_n", "Lot_Size_n")], 
                  test=valid.df[c("Income_n", "Lot_Size_n")], 
                  cl=train.df$Ownership,
                  k=3)

table(mower.pred, valid.df$Ownership)
mean(mower.pred == valid.df$Ownership)

