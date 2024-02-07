# Intro to R - Week 2 Part 2

#### ---------------------------- Dataframe -----------------------------------

# Dataframes can contain both string vectors and numeric vectors.
# Data frame is two dimensional (A dataframe has columns and rows )

# Example: 
# "trees" dataset: contains Girth, Height and Volume for Black Cherry Trees.
?trees
str(trees)
# We can see that trees is a data frame with 31 rows and 3 columns.

# Access information from this data frame
trees[2,] 
trees[,2] 

trees[c(2, 5, 9), ]
trees[c(2, 5, 9), 1]

trees[2:3,]    

trees[10:12,2] 

trees[trees$Height > 82,]   
# or
subset(trees, trees$Height>82)

# Example 2:
housing.df <- read.csv("WestRoxbury.csv")  # load data

# Information about this data set

dim(housing.df)  # find the dimension of data frame
head(housing.df)  # show the first six rows
View(housing.df)  # show all the data in a new tab
str(housing.df) # list the structure of the data
summary(housing.df)  # find summary statistics for each column

# Indexing
# write a syntax to 

# show the first 10 rows of the first column only

# show the first 10 rows of each of the columns

# show the fifth row of the first 10 columns

# show the fifth row of some columns

# show the whole first column

########

# How to remove rows? (use negative index)
# How to remove variables? (negative index or NULL)

##
housing.df$TAX <- 5 # Replace all the data in a field with a number
#
housing.df$YR.BUILT[housing.df$YR.BUILT>=1900] <- "new house" # Replace the data in a field based on equal to some value
housing.df$YR.BUILT[housing.df$ROOMS>=6] <- "big house" # Replace the data in a field based on equal to some value
# Create a new column/variable
housing.df$GOOD.HOUSE[housing.df$FLOORS==2] <- 1
housing.df$GOOD.HOUSE[is.na(housing.df$GOOD.HOUSE)] <- 0


###  ----------  If-else statements --------------------------------------

# There are two types of if statements in R. 
###  1. The simple "if statement":

x <- 10

if (x > 20) {
  y <- 1
}

# We can use this in conjunction with an "else" statement:
x <- 18

if (x > 20) {
  y <- 1
} else {
  y <- 0
}

# We can include multiple commands:
x <- 20

if (x == 21) {
  y <- 1
  w <- TRUE
} else {
  y <- 0
  w <- FALSE
}

y
w


# Exercise: 
# if x > 10, y = 1, z = "TRUE"
# if x == 10, y = 2, z = "FALSE"
# if x < 10, y = 3, z = "NA"

x <- 4




#### 2. "ifelse" statement

# ifelse(Boolean_Vector, Outcome_If_True, Outcome_If_False)

# If the condition is satisfied, Outcome_If_True is returned.  If the
# condition is not satisfied, Outcome_If_False is returned.

ifelse(c("True", "False", "True", "False"), "Young", "Old")

ifelse(3 > 4, x <- 5, x <- 6)


##### Exploring and understanding data --------------------

## data exploration example using used car data
usedcars <- read.csv("usedcars.csv", stringsAsFactors = FALSE)

# get structure of used car data
str(usedcars)

## Exploring numeric variables -----

# summarize numeric variables
summary(usedcars$year)
summary(usedcars[c("price", "mileage")])


# the min/max of used car prices
range(usedcars$price)

# the difference of the range
diff(range(usedcars$price))

# use quantile to calculate five-number summary
quantile(usedcars$price)

# IQR for used car prices
IQR(usedcars$price)


# the 99th percentile
quantile(usedcars$price, probs = c(0.01, 0.99))

# quintiles
quantile(usedcars$price, seq(from = 0, to = 1, by = 0.20))

# boxplot of used car prices and mileage
boxplot(usedcars$price, main="Boxplot of Used Car Prices",
        ylab="Price ($)")

boxplot(usedcars$mileage, main="Boxplot of Used Car Mileage",
        ylab="Odometer (mi.)")

# histograms of used car prices and mileage
hist(usedcars$price, main = "Histogram of Used Car Prices",
     xlab = "Price ($)")

hist(usedcars$mileage, main = "Histogram of Used Car Mileage",
     xlab = "Odometer (mi.)")

# variance and standard deviation of the used car data
var(usedcars$price)
sd(usedcars$price)
var(usedcars$mileage)
sd(usedcars$mileage)

## Exploring numeric variables -----

# one-way tables for the used car data
table(usedcars$year)
table(usedcars$model)
table(usedcars$color)

# compute table proportions
model_table <- table(usedcars$model)
prop.table(model_table)

# round the data
color_table <- table(usedcars$color)
color_pct <- prop.table(color_table) * 100
round(color_pct, digits = 1)

## Exploring relationships between variables -----

# scatterplot of price vs. mileage
plot(x = usedcars$mileage, y = usedcars$price,
     main = "Scatterplot of Price vs. Mileage",
     xlab = "Used Car Odometer (mi.)",
     ylab = "Used Car Price ($)")
abline(a=0,b=1, col="red")