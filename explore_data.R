##### BREED EFFECTS #####

#100 most common breeds
data %>% group_by(Breed) %>% summarize(n = n(), euth_prop = mean(Euthanized == TRUE)) %>% arrange(-n) %>% print(n=100)

#100 most commonly euthanized breeds, filtered by n >= 10
data %>% group_by(Breed) %>% summarize(n = n(), euth_prop = mean(Euthanized == TRUE)) %>% arrange(-euth_prop) %>% filter(n >= 10) %>% print(n=100)


##### AGE EFFECTS #####

#age distributions of each outcome
data %>% ggplot(aes(x = OutcomeType, y = AgeInYears)) + geom_boxplot()

#age distributions of each outcome with scaled y axis
data %>% ggplot(aes(x = OutcomeType, y = AgeInYears)) + geom_boxplot() + ylim(0,5)

#age distributions of euthanized vs. not
data %>% ggplot(aes(x = Euthanized, y = AgeInYears)) + geom_boxplot()

#age distributions of euthanized vs. not with scaled y axis
data %>% ggplot(aes(x = Euthanized, y = AgeInYears)) + geom_boxplot() + ylim(0,5)

#t test for statistical age difference between euthanized and non-euthanized groups
t.test(pull(filter(data, Euthanized == TRUE),AgeInYears),pull(filter(data, Euthanized == FALSE),AgeInYears))


##### EFFECTS OF ANIMAL TYPE, SEX, and FIXED STATUS #####

#euthanization rates by animal type
data %>% group_by(AnimalType) %>% summarize(n = n(), euth_prop = mean(Euthanized == TRUE)) %>% arrange(-euth_prop)

#euthanization rates by fixed status
data %>% group_by(FixedStatus) %>% summarize(n = n(), euth_prop = mean(Euthanized == TRUE)) %>% arrange(-euth_prop)

#euthanization rates by sex
data %>% group_by(Sex) %>% summarize(n = n(), euth_prop = mean(Euthanized == TRUE)) %>% arrange(-euth_prop)

#euthanization rates by sex and animal type
data %>% group_by(Sex,AnimalType) %>% summarize(n = n(), euth_prop = mean(Euthanized == TRUE)) %>% arrange(-euth_prop)

#euthanization rates by sex, animal type, and fixed status
data %>% group_by(Sex,AnimalType,FixedStatus) %>% summarize(n = n(), euth_prop = mean(Euthanized == TRUE)) %>% arrange(-euth_prop)

#chi squared tests for statistical significance between groups

#animal type
chisq.test(data$AnimalType,data$Euthanized)

#fixed status
chisq.test(data$FixedStatus,data$Euthanized)

#fixed status without unknowns
chisq.test(pull(filter(data,FixedStatus!="Unknown"),FixedStatus),pull(filter(data,FixedStatus!="Unknown"),Euthanized))

#sex
chisq.test(data$Sex,data$Euthanized)

#sex without unknowns
chisq.test(pull(filter(data,Sex!="Unknown"),Sex),pull(filter(data,Sex!="Unknown"),Euthanized))













