#fit linear model
glm_fit <- glm(data = final_data, Euthanized ~ AnimalType + AgeInYears + FixedStatus + Sex + MonthStatus + BreedStatus, family = binomial(link = "logit"))

#predict conditional probability of euthanization
glm_probs <- predict(glm_fit, newdata = final_data, type = "response")

#function to calculate results at various prediction cutoffs
results <- function(probs, cutoff){
  #generate predictions
  predictions <- factor(probs >= cutoff, levels = c("FALSE","TRUE"))
  #produce confusion matrix
  cm <- confusionMatrix(predictions, reference = final_data$Euthanized, positive = "TRUE")
  #measure true positive and false positive rates
  TPR <- cm$table[2,2]/(cm$table[2,2] + cm$table[1,2])
  FPR <- cm$table[2,1]/(cm$table[2,1] + cm$table[1,1])
  #print rates
  cat("True Positive Rate =", TPR,"\n")
  cat("False Positive Rate =", FPR)
}

#test at various cutoffs
#prob > 0.3
#results(glm_probs,0.3)
#prob > 0.2
#results(glm_probs,0.2)
#prob > 0.1
#results(glm_probs,0.1)
#prob > 0.05
#results(glm_probs,0.05)


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
auc_glm_train <- integrate(approxfun(x = ROC_glm_train$FPR, y = ROC_glm_train$TPR, ties = mean), min(ROC_glm_train$FPR), max(ROC_glm_train$FPR))$value

#clean up
rm(curve,cutoffs,i)