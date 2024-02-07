riding_mowers<-read.csv(file.choose())

RNGversion("3.5.2")
set.seed(12345) 
#>a
percentage_owners <- mean(riding_mowers$Ownership == "Owner") * 100
cat("Percentage of owners:", round(percentage_owners, 2), "%\n")
#>b

riding_mowers$own<- ifelse(riding_mowers$Ownership == "Owner",1,0)
print(riding_mowers)


plot(riding_mowers$Income, riding_mowers$Lot_Size,  col = riding_mowers$own+1, pch = 16,
     xlab = "Income", ylab = "Lot Size", main = "Scatter Plot of Income vs Lot Size")

legend("topright", legend = c("Owners","Nonowners"), col = 2:1, pch = 16)
#>c
#>
logistic_model <- glm(own ~ Income + Lot_Size, data = riding_mowers, family = "binomial")

summary(logistic_model)

predicted_probabilities <- predict(logistic_model, type = "response")
predicted_ownership <- ifelse(predicted_probabilities >= 0.8, 1, 0)
percentage_nonowners_correct <- mean(predicted_ownership[riding_mowers$own == 0] == 0) * 100

# Print the result
cat("Percentage of correctly classified nonowners:", round(percentage_nonowners_correct, 2), "%\n")

#>d
#>based on the testing and changing of the .5 values in line 25 I would increase this value to correctly classified nonowners