teens<-read.csv(file.choose())
str(teens)
table(teens$gender)
table(teens$gender, useNA='ifany')

teens$age<- ifelse(teens$age >=13 & teens$age <20, teens$age, NA)
summary(teens$age)
hist(teens$age)
boxplot(teens$age)

#dummy variable
#female
#no_gender

teens$female <- ifelse(teens$gender =="F" & !is.na(teens$gender),1,0)
teens$no_gender <- ifelse(is.na(teens$gender),1,0)
table(teens$gender, useNA = "ifany")
table(teens$female, useNA = "ifany")
table(teens$no_gender, useNA = "ifany")


table(teens$gradyear)
mean(teens$age, na.rm=TRUE)
aggregate(data=teens, age~gradyear, mean, na.rm=TRUE)
ave_age<-ave(teens$age, teens$gradyear, FUN=function(x) mean(x, na.rm=TRUE))
teens$age<- ifelse(is.na(teens$age), ave_age, teens$age)
interests <- teens[5:40]
interests_z <- as.data.frame(lapply(interests, scale))
summary(interests_z$basketball)
RNGversion("3.5.2")
set.seed(12345)
teen_clusters<-kmeans(interests_z, 5)
teens$cluster<-teen_clusters$cluster

teen_clusters$size
teen_clusters$centers
plot(teens$basketball, teens$football)
aggregate(data=teens, age~cluster, mean, na.rm=TRUE)
aggregate(data=teens, female~cluster, mean, na.rm=TRUE)
aggregate(data=teens, friends~cluster, mean, na.rm=TRUE)


#solutio  of mssing values in age
# average of age

#> dealing with missing vales
#> numeric
#> 1- take the average
#> 2- remove those missing values( if the numer of missing values are not that many)
#> 3- do prediction
#> 
#>categorical

