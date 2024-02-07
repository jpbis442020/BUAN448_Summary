getwd()
setwd("~/Desktop/fall_2023/buan 448")
COVID <- read.csv("COVID19.csv",stringsAsFactors=FALSE)
dim(COVID)
summary(COVID)
str(COVID)
COVID$Date =  as.Date(COVID$Date, format = "%m/%d/%y")
str(COVID["Date"])
head(COVID, n=5)

COVID_Filtered<- COVID[,c(1,2,5,6)]
View(COVID_Filtered)
totDeath = aggregate(Deaths~Country.Region+Date, data=COVID, FUN=sum)
deathOrders= order(totDeath$Country.Region, totDeath$Date)
totDeathSorted<- totDeath[deathOrders,]
totDeathUS<-subset(totDeathSorted,Country.Region=="US")
plot(x = totDeathUS$Date,
     y = totDeathUS$Deaths, 
     type = "o",
     xlab = "Date",
     ylab = "Deaths",
     main = "Total Deaths in the US Over Time",
     xlim = c(min(totDeathUS$Date), max(totDeathUS$Date))
)

