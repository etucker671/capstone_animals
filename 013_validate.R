#new logistic regression predictions
glm_probs_val <- predict(glm_fit, newdata = final_val_data, type = "response")

#new knn predictions
#first recreate knn data set
knn_data <- select(final_data,-Month,-Season)
knn_data$AnimalType <- as.numeric(ifelse(knn_data$AnimalType == "Cat",1,0))
knn_data$AgeInYears[which(is.na(knn_data$AgeInYears)==TRUE)] <- median(na.omit(knn_data$AgeInYears))
knn_data$AgeInYears <- knn_data$AgeInYears/max(knn_data$AgeInYears)
knn_data$FixedStatus <- as.numeric(ifelse(knn_data$FixedStatus == "Spayed-Neutered",1,ifelse(knn_data$FixedStatus == "Unknown",0.25,0)))
knn_data$Sex <- as.numeric(ifelse(knn_data$Sex == "Female",1,ifelse(knn_data$FixedStatus == "Unknown",0.5,0)))
knn_data$MonthStatus <- as.numeric(ifelse(knn_data$MonthStatus == "Danger",1,0))
knn_data$BreedStatus <- as.numeric(ifelse(knn_data$BreedStatus == "Danger",1,ifelse(knn_data$BreedStatus == "Neutral",0.3,0)))

#then treat validation data similarly
knn_val_data <- select(final_val_data,-Month,-Season)
knn_val_data$AnimalType <- as.numeric(ifelse(knn_val_data$AnimalType == "Cat",1,0))
knn_val_data$AgeInYears[which(is.na(knn_val_data$AgeInYears)==TRUE)] <- median(na.omit(knn_data$AgeInYears))
knn_val_data$AgeInYears <- knn_val_data$AgeInYears/max(knn_data$AgeInYears)
knn_val_data$FixedStatus <- as.numeric(ifelse(knn_val_data$FixedStatus == "Spayed-Neutered",1,ifelse(knn_val_data$FixedStatus == "Unknown",0.25,0)))
knn_val_data$Sex <- as.numeric(ifelse(knn_val_data$Sex == "Female",1,ifelse(knn_val_data$FixedStatus == "Unknown",0.5,0)))
knn_val_data$MonthStatus <- as.numeric(ifelse(knn_val_data$MonthStatus == "Danger",1,0))
knn_val_data$BreedStatus <- as.numeric(ifelse(knn_val_data$BreedStatus == "Danger",1,ifelse(knn_val_data$BreedStatus == "Neutral",0.3,0)))

#make predictions
knn_probs_val <- predict(knn_fit, newdata = knn_val_data, type = "prob")[,2]

#clean up
rm(knn_data,knn_val_data)

#new random forest predictions
rf_probs_val <- predict(rf_fit, newdata = final_val_data, type = "prob")[,2]

#### ROC AND AUC CALCULATIONS ####

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

#print all AUCs
data.frame(Model = c("Logistic Regression","K Nearest Neighbors","Random Forest"), Validation_AUC = c(auc_glm_val,auc_knn_val,auc_rf_val)) %>% arrange(-Validation_AUC)

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
