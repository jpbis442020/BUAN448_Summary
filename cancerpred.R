bc.df <- read.csv(file.choose())
RNGversion("3.5.2")
set.seed(12345)
wbcd.df<- bc.df[-which(names(bc.df) == "id")]
table(wbcd.df$diagnosis)
#b-benign m-maligant(cancer)
#prop.table()
round(prop.table(table(wbcd.df$diagnosis))*100, digits=1)

#summary radius _mean, area_mean, smoothness_mean
summary(wbcd.df[c(2,5,6)])

standardize <- function(x){
  return ((x-min(x))/(max(x)-min(x)))
}
standardize (c(1,2,3))
wbcd_n <- as.data.frame(lapply(wbcd.df[-1], standardize))
summary(wbcd_n[c(2,5,6)])

wbcd_n$diagnosis <- wbcd.df$diagnosis


#training and valid df (.75)
train.index <- sample(c(1:nrow(wbcd_n)), nrow(wbcd_n)*0.75)
train.df <- wbcd_n[train.index, ]
valid.df <- wbcd_n[-train.index, ]

round(prop.table(table(train.df$diagnosis))*100, digits=1)
round(prop.table(table(valid.df$diagnosis))*100, digits=1)



library(FNN)
train.pred3 <- knn(train= train.df[1:30], valid.df[1:30],
                  cl=train.df$diagnosis,
                  k=3)


table(train.pred3, valid.df$diagnosis)
mean(train.pred3 == valid.df$diagnosis)

#5
train.pred5 <- knn(train= train.df[1:30], valid.df[1:30],
                   cl=train.df$diagnosis,
                   k=5)


table(train.pred5, valid.df$diagnosis)
mean(train.pred5 == valid.df$diagnosis)
#7
train.pred7 <- knn(train= train.df[1:30], valid.df[1:30],
                   cl=train.df$diagnosis,
                   k=7)


table(train.pred7, valid.df$diagnosis)
mean(train.pred7 == valid.df$diagnosis)
#21
train.pred21 <- knn(train= train.df[1:30], valid.df[1:30],
                   cl=train.df$diagnosis,
                   k=21)


table(train.pred21, valid.df$diagnosis)
mean(train.pred21 == valid.df$diagnosis)

