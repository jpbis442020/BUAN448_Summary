#### BUAN 348/448 Fall 2023
### Week 2 Part 1
### Intro to R

getwd()
setwd("the path to your folder")


# create vectors of data for three medical patients

subject_name <- c("John Doe", "Jane Doe", "Steve Graves")
temperature <- c(98.1, 98.6, 101.4)
flu_status <- c(FALSE, FALSE, TRUE)

# access the second element in body temperature vector

temperature[2]

## examples of accessing items in vector
# include items in the range 2 to 3
temperature[2:3]

# exclude item 2 using the minus sign
temperature[-2]

# use a vector to indicate whether to include item
temperature[c(TRUE, TRUE, FALSE)]

## Factors -----

# A factor is a special case of vector that is solely used for representing nominal variables.
# A nominal variable represent a charachterestic with categories of values.

# add gender factor
gender <- factor(c("MALE", "FEMALE", "MALE"))
gender <- c("MALE", "FEMALE", "MALE")
gender
# Why not use character vectors? An advantage of using factors is that they are generally more efficient
# than character vectors because the category labels are stored only once.
# Rather than storing MALE, MALE, FEMALE, the computer may store 1, 1, 2.
# This can save memory. Additionally, certain machine learning algorithms use special routines to handle categorical variables.
# Coding categorical variables as factors ensures that the model will treat this data appropriately.

# add blood type factor
# When factors are created, we can add additional levels that may not appear in the data

blood <- factor(c("O", "AB", "A"),
                levels = c("A", "B", "AB", "O"))
blood

# add ordered factor
symptoms <- factor(c("SEVERE", "MILD", "MODERATE"),
                   levels = c("MILD", "MODERATE", "SEVERE"),
                   ordered = TRUE)
symptoms

# check for symptoms greater than moderate
symptoms > "MODERATE"

## Lists -----

# display information for a patient
subject_name[1]
temperature[1]
flu_status[1]
gender[1]
blood[1]
symptoms[1]

# create list for a patient
subject1 <- list(fullname = subject_name[1], 
                 temperature = temperature[1],
                 flu_status = flu_status[1],
                 gender = gender[1],
                 blood = blood[1],
                 symptoms = symptoms[1])

subject2 <- list(fullname = subject_name[2], 
                 temperature = temperature[2],
                 flu_status = flu_status[2],
                 gender = gender[2],
                 blood = blood[2],
                 symptoms = symptoms[2])

# display the patient
subject1

## methods for accessing a list

# get a single list value by position (returns a sub-list)
subject1[2]

# get a single list value by position (returns a numeric vector)
subject1[[2]]

# get a single list value by name
subject1$temperature

# get several list items by specifying a vector of names
subject1[c("temperature", "flu_status")]

## access a list like a vector
# get values 2 and 3
subject1[2:3]

## Data frames -----

# create a data frame from medical patient data

pt_data <- data.frame(subject_name, temperature, flu_status, gender,
                      blood, symptoms, stringsAsFactors = FALSE)

# display the data frame
pt_data

## accessing a data frame

# get a single column
pt_data$subject_name

# get several columns by specifying a vector of names
pt_data[c("temperature", "flu_status")]

# this is the same as above, extracting temperature and flu_status
pt_data[2:3]

# accessing by row and column
pt_data[1, 2]

# accessing several rows and several columns using vectors
pt_data[c(1, 3), c(2, 4)]

## Leave a row or column blank to extract all rows or columns

# column 1, all rows
pt_data[, 1]
# row 1, all columns
pt_data[1, ]
# all rows and all columns
pt_data[ , ]

# the following are equivalent
pt_data[c(1, 3), c("temperature", "gender")]
pt_data[-2, c(-1, -3, -5, -6)]

# creating a Celsius temperature column
pt_data$temp_c <- (pt_data$temperature - 32) * (5 / 9)

# comparing before and after
pt_data[c("temperature", "temp_c")]
