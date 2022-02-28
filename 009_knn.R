#convert features to numeric and scale
knn_data <- select(final_data,-Month,-Season)
knn_data$AnimalType <- as.numeric(ifelse(knn_data$AnimalType == "Cat",1,0))
knn_data$AgeInYears[which(is.na(knn_data$AgeInYears)==TRUE)] <- median(na.omit(knn_data$AgeInYears))
knn_data$AgeInYears <- knn_data$AgeInYears/max(knn_data$AgeInYears)
knn_data$FixedStatus <- as.numeric(ifelse(knn_data$FixedStatus == "Spayed-Neutered",1,ifelse(knn_data$FixedStatus == "Unknown",0.25,0)))
knn_data$Sex <- as.numeric(ifelse(knn_data$Sex == "Female",1,ifelse(knn_data$FixedStatus == "Unknown",0.5,0)))
knn_data$MonthStatus <- as.numeric(ifelse(knn_data$MonthStatus == "Danger",1,0))
knn_data$BreedStatus <- as.numeric(ifelse(knn_data$BreedStatus == "Danger",1,ifelse(knn_data$BreedStatus == "Neutral",0.3,0)))
#breed codings determined by final_data %>% group_by(BreedStatus) %>% summarize(euth_prop = mean(Euthanized == TRUE))
#then 0.3 = (neut_prop - safe_prop)/(danger_prop - safe_prop)

#train model
knn_fit <- train(Euthanized ~ ., data = knn_data, method = "knn")

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
auc_knn_train <- integrate(approxfun(x = ROC_knn_train$FPR, y = ROC_knn_train$TPR, ties = mean), min(ROC_knn_train$FPR), max(ROC_knn_train$FPR))$value

#print AUC
auc_knn_train

#clean up
rm(curve,cutoffs,i,knn_data)




