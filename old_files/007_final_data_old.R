#set AnimalType as factor
data$AnimalType <- factor(data$AnimalType, levels = c("Cat","Dog"), ordered = FALSE)

#set Euthanized as factor
data$Euthanized <- factor(data$Euthanized, levels = c("FALSE","TRUE"), ordered = FALSE)

#produce data table with final predictors
final_data <- data %>% select(AnimalType, AgeInYears, FixedStatus, Sex, Month, Season, MonthStatus, BreedStatus, Euthanized)