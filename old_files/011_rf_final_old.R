#create RF dataset and fill NAs
rf_data <- select(final_data,-Month,-Season)
rf_data$AgeInYears[which(is.na(rf_data$AgeInYears)==TRUE)] <- median(na.omit(rf_data$AgeInYears))

#### FINAL MODEL ####

#train random forest
rf_fit <- randomForest(Euthanized ~ ., data = rf_data, ntree = 200, sampsize = c(200,200), importance = TRUE, mtry = 2)

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
auc_rf_train <- integrate(approxfun(x = ROC_rf_train$FPR, y = ROC_rf_train$TPR, ties = mean), min(ROC_rf_train$FPR), max(ROC_rf_train$FPR))$value

#print AUC
auc_rf_train

#clean up
rm(curve,cutoffs,i,rf_data)
