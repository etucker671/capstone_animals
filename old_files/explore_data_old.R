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
  geom_line(aes(group=Outcome)) + scale_color_manual(values=c("blue","red")) + ylab("% Diff from Mean")

rm(AveAdoptionsPerMonth,AveEuthanizationsPerMonth)

#euthanization rates by season
data %>% group_by(Season) %>% summarize(n = n(), euth_prop = mean(Euthanized == TRUE)) %>% arrange(-euth_prop)

#plotted
data %>% group_by(Season) %>% summarize(n = n(), euth_prop = mean(Euthanized == TRUE)) %>% ggplot(aes(x = Season, y = euth_prop)) + geom_point() + geom_line(aes(group=1), col = "red") + ylab("Proportion of Animals Euthanized")

#age of euthanized animals during seasons
data %>% filter(Euthanized == TRUE, is.na(AgeInYears)==FALSE) %>% group_by(Season) %>% summarize(n = n(), mean_age = mean(AgeInYears), med_age = median(AgeInYears))

#age of euthanized animals during months
data %>% filter(Euthanized == TRUE, is.na(AgeInYears)==FALSE) %>% group_by(Month) %>% summarize(n = n(), mean_age = mean(AgeInYears), med_age = median(AgeInYears))

#try binary variable "danger range" = March, April, May, June


##### BREED EFFECTS #####

#100 most common breeds
data %>% group_by(Breed) %>% summarize(n = n(), euth_prop = mean(Euthanized == TRUE)) %>% arrange(-n) %>% print(n=100)

#100 most commonly euthanized breeds, filtered by n >= 10
data %>% group_by(Breed) %>% summarize(n = n(), euth_prop = mean(Euthanized == TRUE)) %>% arrange(-euth_prop) %>% filter(n >= 10) %>% print(n=100)

#top 30 most commonly euthanized breeds, with breeds separated
data %>% mutate(Breed2 = str_remove(Breed," Mix")) %>% group_by(Breed2) %>% summarize(n = n(), euth_prop = mean(Euthanized == TRUE)) %>% arrange(-euth_prop) %>% filter(n >= 10) %>% slice_max(., order_by = euth_prop, n = 30) %>% separate(., Breed2, into = c("1","2","3"), sep = "/") %>% print(n=nrow(.))
#single breeds counted in this group
top30breeds <- data %>% mutate(Breed2 = str_remove(Breed," Mix")) %>% group_by(Breed2) %>% summarize(n = n(), euth_prop = mean(Euthanized == TRUE)) %>% arrange(-euth_prop) %>% filter(n >= 10) %>% slice_max(., order_by = euth_prop, n = 30) %>% separate(., Breed2, into = c("1","2","3"), sep = "/") %>% print(n=nrow(.))
c(as.vector(top30breeds$`1`),as.vector(top30breeds$`2`),as.vector(top30breeds$`3`)) %>% table() %>% as.data.frame() %>% arrange(-Freq)
rm(top30breeds)
#most common are Labrador Retriever (6), Pit Bull (6), Boxer (3),
#Chow Chow (3), Chihuahua Shorthair (2), German Shepherd (2), Rottweiler (2)

#60 least commonly euthanized breeds, with breeds separated
data %>% mutate(Breed2 = str_remove(Breed," Mix")) %>% group_by(Breed2) %>% summarize(n = n(), euth_prop = mean(Euthanized == TRUE)) %>% arrange(euth_prop) %>% filter(n >= 10) %>% slice_min(., order_by = euth_prop, n = 60) %>% separate(., Breed2, into = c("1","2","3"), sep = "/") %>% print(n=nrow(.))
#single breeds counted in this group
bottom60breeds <- data %>% mutate(Breed2 = str_remove(Breed," Mix")) %>% group_by(Breed2) %>% summarize(n = n(), euth_prop = mean(Euthanized == TRUE)) %>% arrange(euth_prop) %>% filter(n >= 10) %>% slice_min(., order_by = euth_prop, n = 60) %>% separate(., Breed2, into = c("1","2","3"), sep = "/") %>% print(n=nrow(.))
c(as.vector(bottom60breeds$`1`),as.vector(bottom60breeds$`2`),as.vector(bottom60breeds$`3`)) %>% table() %>% as.data.frame() %>% arrange(-Freq)
rm(bottom60breeds)
#most common are Chihuahua Shorthair (11), Labrador Retriever (11), Miniature Poodle (4),
#Australian Shepherd (2), Beagle (2), Cardigan Welsh Corgi (2), Cathoula (2),
#Jack Russell Terrier (2), Miniature Pinscher (2), Miniature Schnauzer (2),
#Plott Hound (2), Pug (2), Rat Terrier (2)
#BUT REALLY every breed on this list was never euthanized,
#some even containing 50+ observations

#try "Danger Breed" vs. "Safe Breed" - see R script
#after running, confirm significance:
data %>% group_by(BreedStatus) %>% summarize(euth_prop = mean(Euthanized == TRUE)) %>% arrange(-euth_prop)
chisq.test(data$BreedStatus,data$Euthanized)
#highly significant

#proportions of mixed breeds vs. pure breds euthanized
data %>% mutate(Mix = str_detect(Breed,"/|Mix")) %>% group_by(Mix) %>% summarize(euth_prop = mean(Euthanized == TRUE))

#test for significance
chisq.test(pull(mutate(data,Mix = str_detect(Breed,"/|Mix")),Mix),pull(data,Euthanized))
#not significant


##### COLOR EFFECTS #####

#100 most common colors
data %>% group_by(Color) %>% summarize(n = n(), euth_prop = mean(Euthanized == TRUE)) %>% arrange(-n) %>% print(n=100)

#breeds ordered by euthanization rates, filtered by n >= 10
data %>% group_by(Color) %>% summarize(n = n(), euth_prop = mean(Euthanized == TRUE)) %>% arrange(-euth_prop) %>% filter(n >= 10) %>% print(n=nrow(.))

#top 30 most commonly euthanized colors, with words separated
data %>% group_by(Color) %>% summarize(n = n(), euth_prop = mean(Euthanized == TRUE)) %>% arrange(-euth_prop) %>% filter(n >= 10) %>% slice_max(., order_by = euth_prop, n = 30) %>% separate(., Color, into = c("1","2","3","4","5","6"), sep = "/| ") %>% print(n=nrow(.))
#words counted in this group
top30colors <- data %>% group_by(Color) %>% summarize(n = n(), euth_prop = mean(Euthanized == TRUE)) %>% arrange(-euth_prop) %>% filter(n >= 10) %>% slice_max(., order_by = euth_prop, n = 30) %>% separate(., Color, into = c("1","2","3","4","5","6"), sep = "/| ")
c(as.vector(top30colors$`1`),as.vector(top30colors$`2`),as.vector(top30colors$`3`),as.vector(top30colors$`4`)) %>% table() %>% as.data.frame() %>% arrange(-Freq)
rm(top30colors)
#most common are white (13), brown (7), black (5), brindle (5), 
#blue (4), gray (4), point (4)

#30 least frequently euthanized colors, with words separated
data %>% group_by(Color) %>% summarize(n = n(), euth_prop = mean(Euthanized == TRUE)) %>% arrange(euth_prop) %>% filter(n >= 10) %>% slice_min(., order_by = euth_prop, n = 30) %>% separate(., Color, into = c("1","2","3","4","5","6"), sep = "/| ") %>% print(n=nrow(.))
#words counted in this group
bottom30colors <- data %>% group_by(Color) %>% summarize(n = n(), euth_prop = mean(Euthanized == TRUE)) %>% arrange(euth_prop) %>% filter(n >= 10) %>% slice_min(., order_by = euth_prop, n = 30) %>% separate(., Color, into = c("1","2","3","4","5","6"), sep = "/| ")
c(as.vector(bottom30colors$`1`),as.vector(bottom30colors$`2`),as.vector(bottom30colors$`3`),as.vector(bottom30colors$`4`)) %>% table() %>% as.data.frame() %>% arrange(-Freq)
rm(bottom30colors)
#most common are white (11), tan (7), black (5), buff (4), sable (4)
#probably won't get anything out of predicting by colors


###### OTHER FUNCTIONS ######

#search for NA values
sapply(final_data, FUN = function(x){
  which(is.na(x) == TRUE)
})

