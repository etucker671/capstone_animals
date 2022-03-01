#create binary euthanization feature
data <- data %>% mutate(Euthanized = OutcomeType == "Euthanasia")