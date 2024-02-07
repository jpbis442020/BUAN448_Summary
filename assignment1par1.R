getwd()
setwd("~/Desktop/fall_2023/buan 448")
corolla <- read.csv("ToyotaCorolla.csv")
#View(corolla)
rows_to_remove <- c(3:200)
filtered_corolla <- corolla[-rows_to_remove, ]
#View(filtered_corolla)
filtered_corolla <- filtered_corolla[-500, ]
filtered_corolla <- filtered_corolla[-501, ]
filtered_corolla <- filtered_corolla[-502, ]
#View(filtered_corolla)
rows_to_remove2 <- c(1000:1205)
filtered_corolla2 <- filtered_corolla[-rows_to_remove2, ]
#View(filtered_corolla2)
num_records_before <- nrow(filtered_corolla2)
CorollaNOKM <- filtered_corolla2[filtered_corolla2$KM < 120000, ]
num_records_after <- nrow(CorollaNOKM)
records_removed <- num_records_before - num_records_after
print('number of rows removed is 123')
CorollaNOMETCOLOR <- CorollaNOKM[, -which(names(CorollaNOKM) == "Met_Color")]
CorollaNOMETCOLOR$HP[CorollaNOMETCOLOR$HP > 200] <- 180
CorollaNOMETCOLOR$Old_Car <- ifelse(CorollaNOMETCOLOR$Age_08_04 >= 12, "Old", "Not too old")
CorollaNOMETCOLOR$Fuel_Type_Petrol <- ifelse(CorollaNOMETCOLOR$Fuel_Type == "Petrol", 1, 0)
CorollaNOMETCOLOR$Fuel_Type_Diesel <- ifelse(CorollaNOMETCOLOR$Fuel_Type == "Diesel", 1, 0)
CorollaNOMETCOLOR$Fuel_Type_CNG <- ifelse(CorollaNOMETCOLOR$Fuel_Type == "CNG", 1, 0)
toyotacorollaedited<-CorollaNOMETCOLOR
View(toyotacorollaedited)





