# load in data
car.df <- read.csv(file.choose())
str(car.df)
summary (car.df)
#pairs function observations
pairs(car.df[c(3,4,5,7,9,10,12,13,14,15)])
#> all numeric varaibles
pairs(car.df[c(3,4,7)])
#> all variables that had some correlation
#> price and age have negative correlation
#> age and km have slight positive correlation
#> as age of car goes up price goes down
#> as milage increases price decreases
# first model
selected.df <- car.df[,c("Price","Age_08_04","KM","Fuel_Type","HP","Met_Color","CC","Doors","Weight")]
train.index <- sample(1:1436, 1436)
train.df <- selected.df[train.index, ]
m1<- lm(Price~., data=train.df)
summary(m1)
# prediction of price based on 1st model
car.lm.pred <- predict(m1,train.df)
car.lm.pred
options(scipen = 999,digits = 5)
car.lm.pred
some.residuales <- train.df$Price-car.lm.pred
some.residuales
model.result <- data.frame(predicted=car.lm.pred, actual= train.df$Price,residual=train.df$Price-car.lm.pred)
# prediction of price based on 1st model result
summary(model.result)
#input varlables that statistically significant effect on Price are Age_08_04, KM, HP and Weight
#> each one of Age_08_04 after the car is made on average the car becomes $123.28 cheaper according to the model
#> each KM that the car is driven the price decreases by $0.0173
#> for each addititonal HP the car has it is on average $24.70 more expensive 
#> for each addititonal weight the car has it is on average $21.37 more expensive


#prediction
selected.df <- car.df[,c("Price","Age_08_04","KM","Fuel_Type","HP","Met_Color","CC","Doors","Weight")]

newcar <- data.frame(
  Age_08_04 = 60,
  KM = 80000,
  Fuel_Type = "Petrol",
  Met_Color = 0,
  Automatic = 0,
  CC = 1300,
  Doors = 2,
  Weight = 1000,
  HP=90
)



newpredprice<-predict(m1,newdata=newcar)
options(scipen = 999,digits = 6)
newpredprice

# new price prediction = $8404.87
# price vs km plot

plot(car.df$KM, car.df$Price,
     xlab = "KM",  # Label for the x-axis
     ylab = "Price",  # Label for the y-axis
     main = "Scatter Plot of Price vs. KM",  # Main title for the plot
     
     pch =0       # Type of point (filled circles)
)

# new columns
car.df$AgeSq <- car.df$Age_08_04 ^ 2
car.df$KMSq <- car.df$KM ^ 2
#m3
selected2.df <- car.df[,c("Price","Age_08_04","KM","Fuel_Type","HP","Met_Color","CC","Doors","Weight","AgeSq","KMSq")]
train.index <- sample(1:1436, 1436)
train2.df <- selected2.df[train.index, ]
m3 <- lm(Price~., data=train2.df)
summary(m3)
#> Weight, HP, age, KM, Ageaq, kmsq, and the fuel type variables were signifigant 
#> m3 is less accurate than m1

#age increase from 10 to 11 and 20 to 21
selected.df <- car.df[,c("Price","Age_08_04","KM","Fuel_Type","HP","Met_Color","CC","Doors","Weight")]

newcar2 <- data.frame(
  Age_08_04 = 10,
  KM = 80000,
  Fuel_Type = "Petrol",
  Met_Color = 0,
  Automatic = 0,
  CC = 1300,
  Doors = 2,
  Weight = 1000,
  HP=90
)



newpredpricet1<-predict(m1,newdata=newcar2)
options(scipen = 999,digits = 7)
newpredpricet1

# new price prediction at age 10 = $14568.64
selected.df <- car.df[,c("Price","Age_08_04","KM","Fuel_Type","HP","Met_Color","CC","Doors","Weight")]

newcar3 <- data.frame(
  Age_08_04 = 11,
  KM = 80000,
  Fuel_Type = "Petrol",
  Met_Color = 0,
  Automatic = 0,
  CC = 1300,
  Doors = 2,
  Weight = 1000,
  HP=90
)



newpredpricet2<-predict(m1,newdata=newcar3)
options(scipen = 999,digits = 7)
newpredpricet2
#new price prediction at age 11 = $14445.37 
#with all else being equal increasing the age from 10 to 11 decreased the price by $123.27

selected.df <- car.df[,c("Price","Age_08_04","KM","Fuel_Type","HP","Met_Color","CC","Doors","Weight")]

newcar4 <- data.frame(
  Age_08_04 = 20,
  KM = 80000,
  Fuel_Type = "Petrol",
  Met_Color = 0,
  Automatic = 0,
  CC = 1300,
  Doors = 2,
  Weight = 1000,
  HP=90
)



newpredpricet3<-predict(m1,newdata=newcar4)
options(scipen = 999,digits = 7)
newpredpricet3
#new price prediction at age 20 = $13335.89 
selected.df <- car.df[,c("Price","Age_08_04","KM","Fuel_Type","HP","Met_Color","CC","Doors","Weight")]

newcar5 <- data.frame(
  Age_08_04 = 21,
  KM = 80000,
  Fuel_Type = "Petrol",
  Met_Color = 0,
  Automatic = 0,
  CC = 1300,
  Doors = 2,
  Weight = 1000,
  HP=90
)



newpredpricet4<-predict(m1,newdata=newcar5)
options(scipen = 999,digits = 7)
newpredpricet4
#new price prediction at age 21 = $13212.61
#with all else being equal increasing the age from 20 to 21 decreased the price by $123.28
#yes it changes by 1 cent



library(car)
vif_values <- vif(m3)
print(vif_values)
