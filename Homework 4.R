# Fall 2023
# Homework 4 Logistic Regression

rm(list=ls())
Auction.df <- read.csv("eBayAuctions.csv")
str(Auction.df)

#create dummy variables
for(level in unique(Auction.df$Category)){
  Auction.df[level] <- ifelse(Auction.df$Category == level, 1, 0)
}

for(level in unique(Auction.df$currency)){
  Auction.df[level] <- ifelse(Auction.df$currency == level, 1, 0)
}

for(level in unique(Auction.df$endDay)){
  Auction.df[level] <- ifelse(Auction.df$endDay == level, 1, 0)
}



## part a
pivot.Category <- aggregate(Auction.df$Competitive., by = list(Auction.df$Category), FUN = mean)
names(pivot.Category) <- c("Category", "MeanCompetitive")
pivot.currency <- aggregate(Auction.df$Competitive., by = list(Auction.df$currency), FUN = mean)
names(pivot.currency) <- c("currency", "MeanCompetitive")
pivot.endDay <- aggregate(Auction.df$Competitive., by = list(Auction.df$endDay), FUN = mean)
names(pivot.endDay) <- c("endDay", "MeanCompetitive")
pivot.Duration <- aggregate(Auction.df$Competitive., by = list(Auction.df$Duration), FUN = mean)
names(pivot.Duration) <- c("Duration", "MeanCompetitive")

# from the pivot.currency Table, 'EUR' and 'US' are similar.
# Create dummy variables for Currency;
Auction.df$USEUR <- Auction.df$US + Auction.df$EUR


# Please answer part (b) to part (d) 


