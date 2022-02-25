#produce data table with final predictors
final_data <- data %>% select(AnimalType, AgeInYears, FixedStatus, Sex, Month, Season, MonthStatus, BreedStatus, Euthanized)