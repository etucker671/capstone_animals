#100 most common breeds
data %>% group_by(Breed) %>% summarize(n = n(), euth_prop = mean(OutcomeType == "Euthanasia")) %>% arrange(-n) %>% print(n=100)

#100 most commonly euthanized breeds, filtered by n >= 10
data %>% group_by(Breed) %>% summarize(n = n(), euth_prop = mean(OutcomeType == "Euthanasia")) %>% arrange(-euth_prop) %>% filter(n >= 10) %>% print(n=100)

#age distributions of each outcome
data %>% ggplot(aes(x = OutcomeType, y = AgeInYears)) + geom_boxplot()

#age distributions of each outcome with scaled y axis
data %>% ggplot(aes(x = OutcomeType, y = AgeInYears)) + geom_boxplot() + ylim(0,5)