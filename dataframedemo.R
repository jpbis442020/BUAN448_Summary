# Create a data frame with the given variables
Bike <- data.frame(
  id = c("A", "B", "C", "D", "E", "F"),
  WheelSize = c(26, 27.5, 29, 26, 27.5, 29),
  Suspension = c("Hardtail", "Hardtail", "Hardtail", "Full", "Full", "Full"),
  Material = c("Aluminum", "Aluminum", "Carbon", "Aluminum", "Carbon", "Carbon"),
  Price = c(1000, 1100, 1600, 1200, 1500, 1750),
  Rating = c(4.1, 4.7, 4.6, 4.3, 4.9, 4.8),
  AboveAve = c(0, 1, 1, 0, 1, 1)
)

# View the created data frame
print(Bike)


train = -c(1,2,5)
Bike[train,] 


Rating.pred = ifelse(Bike$Rating > 4.65, 1, 0)
table(Rating.pred, Bike$AboveAve) 


Rating.pred==Bike$AboveAve 

order(Bike$Price) 

