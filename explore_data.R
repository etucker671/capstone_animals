
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


##### MONTH AND SEASON EFFECTS #####

#euthanization rates by month
data %>% group_by(Month) %>% summarize(n = n(), euth_prop = mean(Euthanized == TRUE)) %>% arrange(-euth_prop)

#plotted
data %>% group_by(Month) %>% summarize(n = n(), euth_prop = mean(Euthanized == TRUE)) %>% ggplot(aes(x = Month, y = euth_prop)) + geom_point() + geom_line(aes(group=1), col = "red") + ylab("Proportion of Animals Euthanized")

#adoption and euthanization counts by month
data %>% group_by(Month) %>% summarize(Adoptions = sum(OutcomeType == "Adoption"), Euthanizations = sum(OutcomeType == "Euthanasia"))

#plotted adoptions by month
data %>% group_by(Month) %>% summarize(Adoptions = sum(OutcomeType == "Adoption"), Euthanizations = sum(OutcomeType == "Euthanasia")) %>% ggplot(aes(x = Month, y = Adoptions)) + geom_point() + geom_line(aes(group=1), col = "blue")

#plotted euthanizations by month
data %>% group_by(Month) %>% summarize(Adoptions = sum(OutcomeType == "Adoption"), Euthanizations = sum(OutcomeType == "Euthanasia")) %>% ggplot(aes(x = Month, y = Euthanizations)) + geom_point() + geom_line(aes(group=1), col = "red")

#plotted as deviations from average
AveAdoptionsPerMonth <- data %>% group_by(Month) %>% 
  summarize(Adoptions = sum(OutcomeType == "Adoption"), Euthanizations = sum(OutcomeType == "Euthanasia")) %>% 
  pull(Adoptions) %>% mean()

AveEuthanizationsPerMonth <- data %>% group_by(Month) %>% 
  summarize(Adoptions = sum(OutcomeType == "Adoption"), Euthanizations = sum(OutcomeType == "Euthanasia")) %>% 
  pull(Euthanizations) %>% mean()

data %>% group_by(Month) %>% 
  summarize(Adoptions = (sum(OutcomeType == "Adoption")-AveAdoptionsPerMonth)/AveAdoptionsPerMonth, 
            Euthanizations = (sum(OutcomeType == "Euthanasia")-AveEuthanizationsPerMonth)/AveEuthanizationsPerMonth) %>% 
  gather(.,key = "Outcome", value = Deviation, 2:3) %>% as.data.frame() %>% 
  ggplot(aes(x = Month, y = Deviation, col = Outcome)) + geom_point() + 
  geom_line(aes(group=Outcome)) + scale_color_manual(values=c("blue","red"))

rm(AveAdoptionsPerMonth,AveEuthanizationsPerMonth)

#euthanization rates by season
data %>% group_by(Season) %>% summarize(n = n(), euth_prop = mean(Euthanized == TRUE)) %>% arrange(-euth_prop)

#plotted
data %>% group_by(Season) %>% summarize(n = n(), euth_prop = mean(Euthanized == TRUE)) %>% ggplot(aes(x = Season, y = euth_prop)) + geom_point() + geom_line(aes(group=1), col = "red") + ylab("Proportion of Animals Euthanized")

#age of euthanized animals during seasons
data %>% filter(Euthanized == TRUE, is.na(AgeInYears)==FALSE) %>% group_by(Season) %>% summarize(n = n(), mean_age = mean(AgeInYears), med_age = median(AgeInYears))

#age of euthanized animals during months
data %>% filter(Euthanized == TRUE, is.na(AgeInYears)==FALSE) %>% group_by(Month) %>% summarize(n = n(), mean_age = mean(AgeInYears), med_age = median(AgeInYears))


##### BREED EFFECTS #####

#100 most common breeds
data %>% group_by(Breed) %>% summarize(n = n(), euth_prop = mean(Euthanized == TRUE)) %>% arrange(-n) %>% print(n=100)

#100 most commonly euthanized breeds, filtered by n >= 10
data %>% group_by(Breed) %>% summarize(n = n(), euth_prop = mean(Euthanized == TRUE)) %>% arrange(-euth_prop) %>% filter(n >= 10) %>% print(n=100)


##### COLOR EFFECTS #####

#100 most common colors
data %>% group_by(Color) %>% summarize(n = n(), euth_prop = mean(Euthanized == TRUE)) %>% arrange(-n) %>% print(n=100)

#100 most commonly euthanized breeds, filtered by n >= 10
data %>% group_by(Color) %>% summarize(n = n(), euth_prop = mean(Euthanized == TRUE)) %>% arrange(-euth_prop) %>% filter(n >= 10) %>% print(n=nrow(.))

