###########################
###        SETUP        ###
###########################

#define project libraries
package.names <- c("tidyverse","lubridate","randomForest","caret")

#define check function
package.check <- function(package.names){
  for(i in 1:length(package.names)){
    if(package.names[i] %in% installed.packages()){
      library(package.names[i], character.only = TRUE)
      cat(package.names[i],"already installed. Library reloaded.\n")
    }
    else{
      install.packages(package.names[i], character.only = TRUE)
      library(package.names[i], character.only = TRUE)
      cat("Missing package",package.names[i],"has been installed.\n")
    }
  }
  cat("All requested libraries have been loaded.\n")
}

#run check function on libraries
package.check(package.names)

#download data
data <- read.csv(url("https://raw.githubusercontent.com/etucker671/capstone_animals/main/animaloutcomes.csv"), header = TRUE)

#check data
data %>% head(n=10)

#print original outcomes
cat("\nOriginal Outcomes:\n")
data$OutcomeType %>% table()

#binarize outcomes
data <- data %>% mutate(Euthanized = OutcomeType == "Euthanasia")

#print new outcomes
cat("\nBinary Outcomes:\n")
data$Euthanized %>% table()

#split data
if(version$year >= 2019) {
  set.seed(1, sample.kind="Rounding")
} else {
  set.seed(1)
}
index <- createDataPartition(data$Euthanized, p = 0.8, list = FALSE)
full_data <- data
data <- full_data[index,]
val_data <- full_data[-index,]

#clean up
rm(index)


#################################################
###        DATA EXPLORATION & CLEANING        ###
#################################################

#### AGE EFFECTS ####

#view ages
data$AgeuponOutcome %>% head()

#create conversion dataframe
ages <- data.frame(matrix(ncol = 4, nrow = nrow(data)))
x <- c("number", "unit_orig", "unit_new", "age_years")
colnames(ages) <- x
rm(x)

#Extract numbers
ages$number <- as.numeric(str_extract(data$AgeuponOutcome, "\\d+"))

#Extract units
ages$unit_orig <- str_extract(data$AgeuponOutcome, "[a-z]+")

#Check which units exist
unique(na.omit(ages$unit_orig)) %>% sort()

#remove "s" to consolidate units
ages$unit_new <- str_remove(ages$unit_orig,"s")

#Produce all values in years
ages <- ages %>% mutate(age_years = ifelse(unit_new == "day", number/365, ifelse(unit_new == "month", number/12, ifelse(unit_new == "week", number/52,number))))

#merge into data
data <- cbind(data,AgeInYears = ages$age_years)

#clean up
rm(ages)

#create conversion dataframe
ages <- data.frame(matrix(ncol = 4, nrow = nrow(val_data)))
x <- c("number", "unit_orig", "unit_new", "age_years")
colnames(ages) <- x
rm(x)

#Extract numbers
ages$number <- as.numeric(str_extract(val_data$AgeuponOutcome, "\\d+"))

#Extract units
ages$unit_orig <- str_extract(val_data$AgeuponOutcome, "[a-z]+")

#Check which units exist
table(ages$unit_orig)

#remove "s" to consolidate units
ages$unit_new <- str_remove(ages$unit_orig,"s")

#Check new units
table(ages$unit_new)

#Produce all values in years
ages <- ages %>% mutate(age_years = ifelse(unit_new == "day", number/365, ifelse(unit_new == "month", number/12, ifelse(unit_new == "week", number/52,number))))

#merge into data
val_data <- cbind(val_data,AgeInYears = ages$age_years)

#clean up
rm(ages)

#print distribution
hist(data$AgeInYears, xlab = "Age In Years", main = "Distribution of Ages in Training Data")

#age distributions of euthanized vs. not
data %>% ggplot(aes(x = Euthanized, y = AgeInYears)) + geom_boxplot()

#t test for statistical age difference between euthanized and non-euthanized groups
t.test(pull(filter(data, Euthanized == TRUE),AgeInYears),pull(filter(data, Euthanized == FALSE),AgeInYears))


#### ANIMAL TYPE EFFECTS ####

#euthanization rates by animal type
data %>% group_by(AnimalType) %>% summarize(n = n(), euth_prop = mean(Euthanized == TRUE)) %>% arrange(-euth_prop)

#animal type
chisq.test(data$AnimalType,data$Euthanized)


#### SEX AND FIXED STATUS EFFECTS ####

#euthanization rates by SexUponOutcome
data %>% group_by(SexuponOutcome) %>% summarize(n = n(), euth_prop = mean(Euthanized == TRUE)) %>% arrange(-euth_prop)

#extract sex and spay/neuter status
temp <- separate(data[7], col = "SexuponOutcome", into = c("FixedStatus","Sex"), sep = " ")

#reassign spayed/neutered status to general value
temp[temp$FixedStatus=="Spayed",1] <- "Spayed-Neutered"
temp[temp$FixedStatus=="Neutered",1] <- "Spayed-Neutered"

#convert blanks to Unknown
temp[temp$FixedStatus=="",1] <- "Unknown"
temp[is.na(temp$Sex),2] <- "Unknown"

#convert to factors
temp$FixedStatus <- factor(temp$FixedStatus, levels = c("Spayed-Neutered", "Intact", "Unknown"))
temp$Sex <- factor(temp$Sex)

#add to data set
data <- cbind(data,temp)

#clean up
rm(temp)

#repeat for validation set

#extract sex and spay/neuter status
temp <- separate(val_data[7], col = "SexuponOutcome", into = c("FixedStatus","Sex"), sep = " ")

#reassign spayed/neutered status to general value
temp[temp$FixedStatus=="Spayed",1] <- "Spayed-Neutered"
temp[temp$FixedStatus=="Neutered",1] <- "Spayed-Neutered"

#convert blanks to Unknown
temp[temp$FixedStatus=="",1] <- "Unknown"
temp[is.na(temp$Sex),2] <- "Unknown"

#convert to factors
temp$FixedStatus <- factor(temp$FixedStatus, levels = c("Spayed-Neutered", "Intact", "Unknown"))
temp$Sex <- factor(temp$Sex)

#add to data set
val_data <- cbind(val_data,temp)

#clean up
rm(temp)

#euthanization rates by sex
data %>% group_by(Sex) %>% summarize(n = n(), euth_prop = mean(Euthanized == TRUE)) %>% arrange(-euth_prop)

#sex chisq
chisq.test(data$Sex,data$Euthanized)

#euthanization rates by fixed status
data %>% group_by(FixedStatus) %>% summarize(n = n(), euth_prop = mean(Euthanized == TRUE)) %>% arrange(-euth_prop)

#fixed status chisq
chisq.test(data$FixedStatus,data$Euthanized)


#### SEASONAL EFFECTS ####

#extract month
data <- data %>% mutate(Month = month(DateTime, label = TRUE))
#remove ordering
data$Month <- factor(data$Month, ordered = FALSE)

#### REPEAT FOR VALIDATION DATA ####

#extract month
val_data <- val_data %>% mutate(Month = month(DateTime, label = TRUE))
#remove ordering
val_data$Month <- factor(val_data$Month, ordered = FALSE)

#euth_prop plotted
data %>% group_by(Month) %>% summarize(n = n(), euth_prop = mean(Euthanized == TRUE)) %>% ggplot(aes(x = Month, y = euth_prop)) + geom_point() + geom_line(aes(group=1), col = "red") + ylab("Proportion of Animals Euthanized")

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

#assign danger months
data <- data %>% mutate(MonthStatus = factor(ifelse(Month=="Mar"|Month=="Apr"|Month=="May"|Month=="Jun","Danger","Neutral"), levels = c("Neutral","Danger")))

val_data <- val_data %>% mutate(MonthStatus = factor(ifelse(Month=="Mar"|Month=="Apr"|Month=="May"|Month=="Jun","Danger","Neutral"), levels = c("Neutral","Danger")))


#### BREED EFFECTS ####

#random breeds
data %>% group_by(Breed) %>% summarize(n = n()) %>% filter(n>10) %>% sample_n(.,20)

cat("\nMost Frequently Euthanized Breeds\n")
#30 most commonly euthanized breeds, filtered by n >= 10
data %>% group_by(Breed) %>% summarize(n = n(), euth_prop = mean(Euthanized == TRUE)) %>% arrange(-euth_prop) %>% filter(n >= 10) %>% print(n=30)
cat("\nLeast Frequently Euthanized Breeds\n")
#30 least commonly euthanized breeds, filtered by n >= 10
data %>% group_by(Breed) %>% summarize(n = n(), euth_prop = mean(Euthanized == TRUE)) %>% arrange(euth_prop) %>% filter(n >= 10) %>% print(n=30)

#top 30 most commonly euthanized breeds, with breeds separated
data %>% mutate(Breed2 = str_remove(Breed," Mix")) %>% group_by(Breed2) %>% summarize(n = n(), euth_prop = mean(Euthanized == TRUE)) %>% arrange(-euth_prop) %>% filter(n >= 10) %>% slice_max(., order_by = euth_prop, n = 30) %>% separate(., Breed2, into = c("1","2","3"), sep = "/") %>% print(n=nrow(.))

#single breeds counted in this group
top30breeds <- data %>% mutate(Breed2 = str_remove(Breed," Mix")) %>% group_by(Breed2) %>% summarize(n = n(), euth_prop = mean(Euthanized == TRUE)) %>% arrange(-euth_prop) %>% filter(n >= 10) %>% slice_max(., order_by = euth_prop, n = 30) %>% separate(., Breed2, into = c("1","2","3"), sep = "/")
c(as.vector(top30breeds$`1`),as.vector(top30breeds$`2`),as.vector(top30breeds$`3`)) %>% table() %>% as.data.frame() %>% arrange(-Freq)
rm(top30breeds)

#single breeds counted in this group
bottom60breeds <- data %>% mutate(Breed2 = str_remove(Breed," Mix")) %>% group_by(Breed2) %>% summarize(n = n(), euth_prop = mean(Euthanized == TRUE)) %>% arrange(euth_prop) %>% filter(n >= 10) %>% slice_min(., order_by = euth_prop, n = 60) %>% separate(., Breed2, into = c("1","2","3"), sep = "/")
c(as.vector(bottom60breeds$`1`),as.vector(bottom60breeds$`2`),as.vector(bottom60breeds$`3`)) %>% table() %>% as.data.frame() %>% arrange(-Freq)
rm(bottom60breeds)

#define danger vs. safe breeds
bottom60breeds <- data %>% mutate(Breed2 = str_remove(Breed," Mix")) %>% group_by(Breed2) %>% summarize(n = n(), euth_prop = mean(Euthanized == TRUE)) %>% arrange(euth_prop) %>% filter(n >= 10) %>% slice_min(., order_by = euth_prop, n = 60) %>% separate(., Breed2, into = c("1","2","3"), sep = "/")
bottom60breeds2 <- c(as.vector(bottom60breeds$`1`),as.vector(bottom60breeds$`2`),as.vector(bottom60breeds$`3`)) %>% table() %>% as.data.frame()
colnames(bottom60breeds2) <- c("Breed","Freq")

top30breeds <- data %>% mutate(Breed2 = str_remove(Breed," Mix")) %>% group_by(Breed2) %>% summarize(n = n(), euth_prop = mean(Euthanized == TRUE)) %>% arrange(-euth_prop) %>% filter(n >= 10) %>% slice_max(., order_by = euth_prop, n = 30) %>% separate(., Breed2, into = c("1","2","3"), sep = "/")
top30breeds2 <- c(as.vector(top30breeds$`1`),as.vector(top30breeds$`2`),as.vector(top30breeds$`3`)) %>% table() %>% as.data.frame() %>% arrange(-Freq)
colnames(top30breeds2) <- c("Breed","Freq")
hi_topbreeds <- c(as.vector(top30breeds$`1`),as.vector(top30breeds$`2`),as.vector(top30breeds$`3`)) %>% table() %>% as.data.frame() %>% arrange(-Freq)
colnames(hi_topbreeds) <- c("Breed","Freq")

danger_breeds <- hi_topbreeds[!(hi_topbreeds$Breed %in% bottom60breeds2$Breed),1] %>% as.character()

safe_breeds <- bottom60breeds2[!(bottom60breeds2$Breed %in% top30breeds2$Breed),1] %>% as.character()

#define function for detecting any of the breeds in breed string
multi_string_detect<-function(x,y){
  temp<-sapply(y, function(z){str_detect(x, z)})
  apply(temp, 1, any)
}

#extract and assign danger/safe breeds in training data
BreedStatusTable <- data.frame(Breed = data$Breed, Danger = multi_string_detect(data$Breed,danger_breeds), Safe = multi_string_detect(data$Breed,safe_breeds)) %>% mutate(BreedStatus = ifelse(Danger == TRUE, "Danger", ifelse(Safe == TRUE, "Safe", "Neutral")))

data <- data %>% mutate(BreedStatus = factor(BreedStatusTable$BreedStatus, levels = c("Neutral", "Safe", "Danger"), ordered = FALSE))

#extract and assign danger/safe breeds in validation data
BreedStatusTable_val <- data.frame(Breed = val_data$Breed, Danger = multi_string_detect(val_data$Breed,danger_breeds), Safe = multi_string_detect(val_data$Breed,safe_breeds)) %>% mutate(BreedStatus = ifelse(Danger == TRUE, "Danger", ifelse(Safe == TRUE, "Safe", "Neutral")))

val_data <- val_data %>% mutate(BreedStatus = factor(BreedStatusTable_val$BreedStatus, levels = c("Neutral", "Safe", "Danger"), ordered = FALSE))

#clean up
rm(bottom60breeds,bottom60breeds2,BreedStatusTable,BreedStatusTable_val,hi_topbreeds,top30breeds,top30breeds2)

cat("\nCounts of Breed Statuses in Training Set\n")
table(data$BreedStatus)

#check significance of groups
data %>% group_by(BreedStatus) %>% summarize(euth_prop = mean(Euthanized == TRUE)) %>% arrange(-euth_prop)
chisq.test(data$BreedStatus,data$Euthanized)


#### COLOR EFFECTS ####

#top colors
top30colors <- data %>% group_by(Color) %>% summarize(n = n(), euth_prop = mean(Euthanized == TRUE)) %>% arrange(-euth_prop) %>% filter(n >= 10) %>% slice_max(., order_by = euth_prop, n = 30) %>% separate(., Color, into = c("1","2","3","4","5","6"), sep = "/| ")
cat("\nMost Frequently Euthanized Unique Colors\n")
c(as.vector(top30colors$`1`),as.vector(top30colors$`2`),as.vector(top30colors$`3`),as.vector(top30colors$`4`)) %>% table() %>% as.data.frame() %>% arrange(-Freq)
rm(top30colors)

#bottom colors
bottom30colors <- data %>% group_by(Color) %>% summarize(n = n(), euth_prop = mean(Euthanized == TRUE)) %>% arrange(euth_prop) %>% filter(n >= 10) %>% slice_min(., order_by = euth_prop, n = 30) %>% separate(., Color, into = c("1","2","3","4","5","6"), sep = "/| ")
cat("\nLeast Frequently Euthanized Unique Colors\n")
c(as.vector(bottom30colors$`1`),as.vector(bottom30colors$`2`),as.vector(bottom30colors$`3`),as.vector(bottom30colors$`4`)) %>% table() %>% as.data.frame() %>% arrange(-Freq)
rm(bottom30colors)


#### FINAL DATA ####

#set AnimalType as factor
data$AnimalType <- factor(data$AnimalType, levels = c("Cat","Dog"), ordered = FALSE)
val_data$AnimalType <- factor(val_data$AnimalType, levels = c("Cat","Dog"), ordered = FALSE)

#set Euthanized as factor
data$Euthanized <- factor(data$Euthanized, levels = c("FALSE","TRUE"), ordered = FALSE)
val_data$Euthanized <- factor(val_data$Euthanized, levels = c("FALSE","TRUE"), ordered = FALSE)

#produce data tables with final predictors
final_data <- data %>% select(AnimalType, AgeInYears, FixedStatus, Sex, MonthStatus, BreedStatus, Euthanized)
final_val_data <- val_data %>% select(AnimalType, AgeInYears, FixedStatus, Sex, MonthStatus, BreedStatus, Euthanized)

#print top 10 rows
head(final_data, n=10)


#################################
###       MODEL BUILDING      ###
#################################

#### LOGISTIC REGRESSION ####

#fit linear model
glm_fit <- glm(data = final_data, Euthanized ~ AnimalType + AgeInYears + FixedStatus + Sex + MonthStatus + BreedStatus, family = binomial(link = "logit"))

#print coefficients
summary(glm_fit)$coefficients

#predict conditional probability of euthanization
glm_probs <- predict(glm_fit, newdata = final_data, type = "response")

#### ROC CURVE ####

#define TPR function
calcTPR <- function(probs, cutoff){
  #generate predictions
  predictions <- factor(probs >= cutoff, levels = c("FALSE","TRUE"))
  #produce confusion matrix
  cm <- confusionMatrix(predictions, reference = final_data$Euthanized, positive = "TRUE")
  #measure true positive rate
  TPR <- cm$table[2,2]/(cm$table[2,2] + cm$table[1,2])
  #print TPR
  TPR
}

#define FPR function
calcFPR <- function(probs, cutoff){
  #generate predictions
  predictions <- factor(probs >= cutoff, levels = c("FALSE","TRUE"))
  #produce confusion matrix
  cm <- confusionMatrix(predictions, reference = final_data$Euthanized, positive = "TRUE")
  #measure true positive rate
  FPR <- cm$table[2,1]/(cm$table[2,1] + cm$table[1,1])
  #print TPR
  FPR
}

#produce curve values
cutoffs <- seq(0,1,0.01)
ROC_glm_train <- data.frame(Cutoff = numeric(length = length(cutoffs)), TPR = numeric(length = length(cutoffs)), FPR = numeric(length = length(cutoffs)))
for(i in 1:length(cutoffs)){
  ROC_glm_train[i,1] <- cutoffs[i]
  ROC_glm_train[i,2] <- calcTPR(glm_probs,cutoffs[i])
  ROC_glm_train[i,3] <- calcFPR(glm_probs,cutoffs[i])
}

#plot curve
curve <- ROC_glm_train %>% ggplot(aes(x = FPR, y = TPR)) + 
  geom_line(col="red3",size=1.5) + geom_abline(intercept = 0, slope = 1) + 
  scale_x_continuous(limits=c(0, 1), expand = c(0, 0)) + 
  scale_y_continuous(limits=c(0, 1), expand = c(0, 0)) + 
  theme_minimal() + xlab("False Positive Rate") + ylab("True Positive Rate") +
  ggtitle("Training ROC, Logistic Regression")

print(curve)

#calculate AUC
auc_glm_train <- integrate(approxfun(x = ROC_glm_train$FPR, y = ROC_glm_train$TPR, ties = mean), min(ROC_glm_train$FPR), max(ROC_glm_train$FPR),subdivisions = 2000)$value

#print AUC
cat("\nTraining AUC, Logistic Regression:\n",auc_glm_train,"\n")

#clean up
rm(curve,cutoffs,i)


#### K NEAREST NEIGHBORS ####

#convert features to numeric and scale
knn_data <- final_data
knn_data$AnimalType <- as.numeric(ifelse(knn_data$AnimalType == "Cat",1,0))
knn_data$AgeInYears[which(is.na(knn_data$AgeInYears)==TRUE)] <- median(na.omit(knn_data$AgeInYears))
knn_data$AgeInYears <- knn_data$AgeInYears/max(knn_data$AgeInYears)
knn_data$FixedStatus <- as.numeric(ifelse(knn_data$FixedStatus == "Spayed-Neutered",1,ifelse(knn_data$FixedStatus == "Unknown",0.3,0)))
knn_data$Sex <- as.numeric(ifelse(knn_data$Sex == "Female",1,ifelse(knn_data$FixedStatus == "Unknown",0.5,0)))
knn_data$MonthStatus <- as.numeric(ifelse(knn_data$MonthStatus == "Danger",1,0))
knn_data$BreedStatus <- as.numeric(ifelse(knn_data$BreedStatus == "Danger",1,ifelse(knn_data$BreedStatus == "Neutral",0.3,0)))
#breed codings determined by final_data %>% group_by(BreedStatus) %>% summarize(euth_prop = mean(Euthanized == TRUE))
#then 0.3 = (neut_prop - safe_prop)/(danger_prop - safe_prop)

#train model
knn_fit <- train(Euthanized ~ ., data = knn_data, method = "knn")

knn_fit

#get probabilities
knn_probs <- predict(knn_fit, type = "prob")[,2]

#generate ROC
cutoffs <- seq(0,1,0.01)
ROC_knn_train <- data.frame(Cutoff = numeric(length = length(cutoffs)), TPR = numeric(length = length(cutoffs)), FPR = numeric(length = length(cutoffs)))
for(i in 1:length(cutoffs)){
  ROC_knn_train[i,1] <- cutoffs[i]
  ROC_knn_train[i,2] <- calcTPR(knn_probs,cutoffs[i])
  ROC_knn_train[i,3] <- calcFPR(knn_probs,cutoffs[i])
}

#plot curve
curve <- ROC_knn_train %>% ggplot(aes(x = FPR, y = TPR)) + 
  geom_line(col="red3",size=1.5) + geom_abline(intercept = 0, slope = 1) + 
  scale_x_continuous(limits=c(0, 1), expand = c(0, 0)) + 
  scale_y_continuous(limits=c(0, 1), expand = c(0, 0)) + 
  theme_minimal() + xlab("False Positive Rate") + ylab("True Positive Rate") +
  ggtitle("Training ROC, KNN")

print(curve)

#calculate AUC
auc_knn_train <- integrate(approxfun(x = ROC_knn_train$FPR, y = ROC_knn_train$TPR, ties = mean), min(ROC_knn_train$FPR), max(ROC_knn_train$FPR),subdivisions = 2000)$value

#print AUC
cat("\nTraining AUC, K Nearest Neighbors:\n",auc_knn_train,"\n")

#clean up
rm(curve,cutoffs,i,knn_data)


#### RANDOM FOREST ####

#create RF dataset and fill NAs
rf_data <- final_data
rf_data$AgeInYears[which(is.na(rf_data$AgeInYears)==TRUE)] <- median(na.omit(rf_data$AgeInYears))

#### TUNE ####
if(version$year >= 2019) {
  set.seed(2, sample.kind="Rounding")
} else {
  set.seed(2)
}

#define mtry tuning function
rf_mtry <- function(x){
  #train random forest
  rf_fit_fxn <- randomForest(Euthanized ~ ., data = rf_data, ntree = 300, sampsize = c(500,500), importance = TRUE, mtry = x)
  
  #get predictions
  rf_probs_fxn <- predict(rf_fit_fxn, type = "prob")[,2]
  
  #generate ROC
  cutoffs_fxn <- seq(0,1,0.01)
  
  ROC_fxn <- data.frame(Cutoff = numeric(length = length(cutoffs_fxn)), TPR = numeric(length = length(cutoffs_fxn)), FPR = numeric(length = length(cutoffs_fxn)))
  for(i in 1:length(cutoffs_fxn)){
    ROC_fxn[i,1] <- cutoffs_fxn[i]
    ROC_fxn[i,2] <- calcTPR(rf_probs_fxn,cutoffs_fxn[i])
    ROC_fxn[i,3] <- calcFPR(rf_probs_fxn,cutoffs_fxn[i])
  }
  
  #calculate and return AUC
  auc_fxn <- integrate(approxfun(x = ROC_fxn$FPR, y = ROC_fxn$TPR, ties = mean), min(ROC_fxn$FPR), max(ROC_fxn$FPR),subdivisions=2000)$value
  return(auc_fxn)
}

#try different values for mtry
mtrys <- seq(1:6)
data.frame(mtry = mtrys, AUC = sapply(mtrys,FUN=rf_mtry))

#define ntree tuning function
rf_ntree <- function(x){
  #train random forest
  rf_fit_fxn <- randomForest(Euthanized ~ ., data = rf_data, ntree = x, sampsize = c(500,500), importance = TRUE, mtry = 2)
  
  #get predictions
  rf_probs_fxn <- predict(rf_fit_fxn, type = "prob")[,2]
  
  #generate ROC
  cutoffs_fxn <- seq(0,1,0.01)
  
  ROC_fxn <- data.frame(Cutoff = numeric(length = length(cutoffs_fxn)), TPR = numeric(length = length(cutoffs_fxn)), FPR = numeric(length = length(cutoffs_fxn)))
  for(i in 1:length(cutoffs_fxn)){
    ROC_fxn[i,1] <- cutoffs_fxn[i]
    ROC_fxn[i,2] <- calcTPR(rf_probs_fxn,cutoffs_fxn[i])
    ROC_fxn[i,3] <- calcFPR(rf_probs_fxn,cutoffs_fxn[i])
  }
  
  #calculate and return AUC
  auc_fxn <- integrate(approxfun(x = ROC_fxn$FPR, y = ROC_fxn$TPR, ties = mean), min(ROC_fxn$FPR), max(ROC_fxn$FPR),subdivisions=2000)$value
  return(auc_fxn)
}

#try different values of ntree
ntrees <- c(1,3,5,10,15,25,50,75,125,200,325,525,850)
data.frame(ntree = ntrees, AUC = sapply(ntrees,FUN=rf_ntree))

#try a more finely-tuned ntree series past 200
ntrees <- c(200,225,250,275,300,325,350,375,400)
data.frame(ntree = ntrees, AUC = sapply(ntrees,FUN=rf_ntree))

#define sample size tuning function
rf_sampsize <- function(x){
  #train random forest
  rf_fit_fxn <- randomForest(Euthanized ~ ., data = rf_data, ntree = 200, sampsize = c(x,x), importance = TRUE, mtry = 2)
  
  #get predictions
  rf_probs_fxn <- predict(rf_fit_fxn, type = "prob")[,2]
  
  #generate ROC
  cutoffs_fxn <- seq(0,1,0.01)
  
  ROC_fxn <- data.frame(Cutoff = numeric(length = length(cutoffs_fxn)), TPR = numeric(length = length(cutoffs_fxn)), FPR = numeric(length = length(cutoffs_fxn)))
  for(i in 1:length(cutoffs_fxn)){
    ROC_fxn[i,1] <- cutoffs_fxn[i]
    ROC_fxn[i,2] <- calcTPR(rf_probs_fxn,cutoffs_fxn[i])
    ROC_fxn[i,3] <- calcFPR(rf_probs_fxn,cutoffs_fxn[i])
  }
  
  #calculate and return AUC
  auc_fxn <- integrate(approxfun(x = ROC_fxn$FPR, y = ROC_fxn$TPR, ties = mean), min(ROC_fxn$FPR), max(ROC_fxn$FPR),subdivisions = 2000)$value
  return(auc_fxn)
}

#try different values for sampsize
sampsizes <- c(25,50,75,100,200,350,500,750,1000)
data.frame(sampsize = sampsizes, AUC = sapply(sampsizes,FUN=rf_sampsize))

#clean up
rm(rf_data,ntrees,mtrys,sampsizes)

#create RF dataset and fill NAs
rf_data <- final_data
rf_data$AgeInYears[which(is.na(rf_data$AgeInYears)==TRUE)] <- median(na.omit(rf_data$AgeInYears))

#run final model

#train random forest
rf_fit <- randomForest(Euthanized ~ ., data = rf_data, ntree = 200, sampsize = c(100,100), importance = TRUE, mtry = 2)

#get predictions
rf_probs <- predict(rf_fit, type = "prob")[,2]

#generate ROC
cutoffs <- seq(0,1,0.01)
ROC_rf_train <- data.frame(Cutoff = numeric(length = length(cutoffs)), TPR = numeric(length = length(cutoffs)), FPR = numeric(length = length(cutoffs)))
for(i in 1:length(cutoffs)){
  ROC_rf_train[i,1] <- cutoffs[i]
  ROC_rf_train[i,2] <- calcTPR(rf_probs,cutoffs[i])
  ROC_rf_train[i,3] <- calcFPR(rf_probs,cutoffs[i])
}

#plot curve
curve <- ROC_rf_train %>% ggplot(aes(x = FPR, y = TPR)) + 
  geom_line(col="red3",size=1.5) + geom_abline(intercept = 0, slope = 1) + 
  scale_x_continuous(limits=c(0, 1), expand = c(0, 0)) + 
  scale_y_continuous(limits=c(0, 1), expand = c(0, 0)) + 
  theme_minimal() + xlab("False Positive Rate") + ylab("True Positive Rate") +
  ggtitle("Training ROC, Random Forest")

print(curve)

#calculate AUC
auc_rf_train <- integrate(approxfun(x = ROC_rf_train$FPR, y = ROC_rf_train$TPR, ties = mean), min(ROC_rf_train$FPR), max(ROC_rf_train$FPR),subdivisions = 2000)$value

#print AUC
cat("\nTraining AUC, Random Forest:\n",auc_rf_train,"\n")

#clean up
rm(curve,cutoffs,i,rf_data)

#check var importance
cat("\nImportance of Variables in Random Forest Model:\n")
rf_fit$importance


#### MODEL COMPARISON ####

#combine all training ROC data
all_ROCs_train <- rbind(mutate(ROC_glm_train,Method = rep("GLM",101)), 
                        mutate(ROC_knn_train,Method = rep("KNN",101)), 
                        mutate(ROC_rf_train, Method = rep("RandomForest",101)))

#plot all training ROCs together
allplot <- all_ROCs_train %>% ggplot(aes(x = FPR, y = TPR, col = Method)) + 
  geom_line(size=1) + geom_abline(intercept = 0, slope = 1, col = "gray") + 
  scale_x_continuous(limits=c(0, 1), expand = c(0, 0)) + 
  scale_y_continuous(limits=c(0, 1), expand = c(0, 0)) + 
  theme_minimal() + xlab("False Positive Rate") + 
  ylab("True Positive Rate") + ggtitle("Training ROC Curves")

print(allplot)
rm(allplot)

#print all AUCs
data.frame(Model = c("Logistic Regression","K Nearest Neighbors","Random Forest"), Training_AUC = c(auc_glm_train,auc_knn_train,auc_rf_train)) %>% arrange(-Training_AUC)


#### MODEL VALIDATION ####

#new logistic regression predictions
glm_probs_val <- predict(glm_fit, newdata = final_val_data, type = "response")

#new knn predictions
#first recreate knn data set
knn_data <- final_data
knn_data$AnimalType <- as.numeric(ifelse(knn_data$AnimalType == "Cat",1,0))
knn_data$AgeInYears[which(is.na(knn_data$AgeInYears)==TRUE)] <- median(na.omit(knn_data$AgeInYears))
knn_data$AgeInYears <- knn_data$AgeInYears/max(knn_data$AgeInYears)
knn_data$FixedStatus <- as.numeric(ifelse(knn_data$FixedStatus == "Spayed-Neutered",1,ifelse(knn_data$FixedStatus == "Unknown",0.3,0)))
knn_data$Sex <- as.numeric(ifelse(knn_data$Sex == "Female",1,ifelse(knn_data$FixedStatus == "Unknown",0.5,0)))
knn_data$MonthStatus <- as.numeric(ifelse(knn_data$MonthStatus == "Danger",1,0))
knn_data$BreedStatus <- as.numeric(ifelse(knn_data$BreedStatus == "Danger",1,ifelse(knn_data$BreedStatus == "Neutral",0.3,0)))

#then treat validation data similarly
knn_val_data <- final_val_data
knn_val_data$AnimalType <- as.numeric(ifelse(knn_val_data$AnimalType == "Cat",1,0))
knn_val_data$AgeInYears[which(is.na(knn_val_data$AgeInYears)==TRUE)] <- median(na.omit(knn_data$AgeInYears))
knn_val_data$AgeInYears <- knn_val_data$AgeInYears/max(knn_data$AgeInYears)
knn_val_data$FixedStatus <- as.numeric(ifelse(knn_val_data$FixedStatus == "Spayed-Neutered",1,ifelse(knn_val_data$FixedStatus == "Unknown",0.3,0)))
knn_val_data$Sex <- as.numeric(ifelse(knn_val_data$Sex == "Female",1,ifelse(knn_val_data$FixedStatus == "Unknown",0.5,0)))
knn_val_data$MonthStatus <- as.numeric(ifelse(knn_val_data$MonthStatus == "Danger",1,0))
knn_val_data$BreedStatus <- as.numeric(ifelse(knn_val_data$BreedStatus == "Danger",1,ifelse(knn_val_data$BreedStatus == "Neutral",0.3,0)))

#make predictions
knn_probs_val <- predict(knn_fit, newdata = knn_val_data, type = "prob")[,2]

#clean up
rm(knn_data,knn_val_data)

#new random forest predictions
rf_probs_val <- predict(rf_fit, newdata = final_val_data, type = "prob")[,2]

#ROC and AUC calculations

#define TPR function for validation set
calcTPR_val <- function(probs, cutoff){
  #generate predictions
  predictions <- factor(probs >= cutoff, levels = c("FALSE","TRUE"))
  #produce confusion matrix
  cm <- confusionMatrix(predictions, reference = final_val_data$Euthanized, positive = "TRUE")
  #measure true positive rate
  TPR <- cm$table[2,2]/(cm$table[2,2] + cm$table[1,2])
  #print TPR
  TPR
}

#define FPR function for validation set
calcFPR_val <- function(probs, cutoff){
  #generate predictions
  predictions <- factor(probs >= cutoff, levels = c("FALSE","TRUE"))
  #produce confusion matrix
  cm <- confusionMatrix(predictions, reference = final_val_data$Euthanized, positive = "TRUE")
  #measure true positive rate
  FPR <- cm$table[2,1]/(cm$table[2,1] + cm$table[1,1])
  #print TPR
  FPR
}

#define cutoffs
cutoffs <- seq(0,1,0.01)

#logit ROC and AUC on validation set
ROC_glm_val <- data.frame(Cutoff = numeric(length = length(cutoffs)), TPR = numeric(length = length(cutoffs)), FPR = numeric(length = length(cutoffs)))
for(i in 1:length(cutoffs)){
  ROC_glm_val[i,1] <- cutoffs[i]
  ROC_glm_val[i,2] <- calcTPR_val(glm_probs_val,cutoffs[i])
  ROC_glm_val[i,3] <- calcFPR_val(glm_probs_val,cutoffs[i])
}

auc_glm_val <- integrate(approxfun(x = ROC_glm_val$FPR, y = ROC_glm_val$TPR, ties = mean), min(ROC_glm_val$FPR), max(ROC_glm_val$FPR),subdivisions = 2000)$value

#knn ROC and AUC on validation set
ROC_knn_val <- data.frame(Cutoff = numeric(length = length(cutoffs)), TPR = numeric(length = length(cutoffs)), FPR = numeric(length = length(cutoffs)))
for(i in 1:length(cutoffs)){
  ROC_knn_val[i,1] <- cutoffs[i]
  ROC_knn_val[i,2] <- calcTPR_val(knn_probs_val,cutoffs[i])
  ROC_knn_val[i,3] <- calcFPR_val(knn_probs_val,cutoffs[i])
}

auc_knn_val <- integrate(approxfun(x = ROC_knn_val$FPR, y = ROC_knn_val$TPR, ties = mean), min(ROC_knn_val$FPR), max(ROC_knn_val$FPR),subdivisions = 2000)$value

#random forest ROC and AUC on validation set
ROC_rf_val <- data.frame(Cutoff = numeric(length = length(cutoffs)), TPR = numeric(length = length(cutoffs)), FPR = numeric(length = length(cutoffs)))
for(i in 1:length(cutoffs)){
  ROC_rf_val[i,1] <- cutoffs[i]
  ROC_rf_val[i,2] <- calcTPR_val(rf_probs_val,cutoffs[i])
  ROC_rf_val[i,3] <- calcFPR_val(rf_probs_val,cutoffs[i])
}

auc_rf_val <- integrate(approxfun(x = ROC_rf_val$FPR, y = ROC_rf_val$TPR, ties = mean), min(ROC_rf_val$FPR), max(ROC_rf_val$FPR),subdivisions = 2000)$value

#combine all training ROC data
all_ROCs_val <- rbind(mutate(ROC_glm_val,Method = rep("GLM",101)), 
                      mutate(ROC_knn_val,Method = rep("KNN",101)), 
                      mutate(ROC_rf_val, Method = rep("RandomForest",101)))

#plot all training ROCs together
allplot <- all_ROCs_val %>% ggplot(aes(x = FPR, y = TPR, col = Method)) + 
  geom_line(size=1) + geom_abline(intercept = 0, slope = 1, col = "gray") + 
  scale_x_continuous(limits=c(0, 1), expand = c(0, 0)) + 
  scale_y_continuous(limits=c(0, 1), expand = c(0, 0)) + 
  theme_minimal() + xlab("False Positive Rate") + 
  ylab("True Positive Rate") + ggtitle("Validation ROC Curves")

print(allplot)
rm(allplot)

#print all AUCs
data.frame(Model = c("Logistic Regression","K Nearest Neighbors","Random Forest"), Validation_AUC = c(auc_glm_val,auc_knn_val,auc_rf_val)) %>% arrange(-Validation_AUC)


