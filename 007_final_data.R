#set AnimalType as factor
data$AnimalType <- factor(data$AnimalType, levels = c("Cat","Dog"), ordered = FALSE)
val_data$AnimalType <- factor(val_data$AnimalType, levels = c("Cat","Dog"), ordered = FALSE)

#set Euthanized as factor
data$Euthanized <- factor(data$Euthanized, levels = c("FALSE","TRUE"), ordered = FALSE)
val_data$Euthanized <- factor(val_data$Euthanized, levels = c("FALSE","TRUE"), ordered = FALSE)

#produce data tables with final predictors
final_data <- data %>% select(AnimalType, AgeInYears, FixedStatus, Sex, Month, Season, MonthStatus, BreedStatus, Euthanized)
final_val_data <- val_data %>% select(AnimalType, AgeInYears, FixedStatus, Sex, Month, Season, MonthStatus, BreedStatus, Euthanized)