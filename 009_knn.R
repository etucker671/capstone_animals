#separate x and y
features <- final_data %>% select(AnimalType,AgeInYears,FixedStatus,Sex,Month,BreedStatus)
outcomes <- final_data$Euthanized

#train model
knn_fit <- train(x = features, y = outcomes, method = "glm")

#get probabilities
knn_probs <- predict(knn_fit,newdata = features, type = "prob")[,2]

#generate ROC
cutoffs <- seq(0,1,0.01)
ROC <- data.frame(Cutoff = numeric(length = length(cutoffs)), TPR = numeric(length = length(cutoffs)), FPR = numeric(length = length(cutoffs)))
for(i in 1:length(cutoffs)){
  ROC[i,1] <- cutoffs[i]
  ROC[i,2] <- calcTPR(knn_probs,cutoffs[i])
  ROC[i,3] <- calcFPR(knn_probs,cutoffs[i])
}

#plot curve
curve <- ROC %>% ggplot(aes(x = FPR, y = TPR)) + 
  geom_line(col="red3",size=1.5) + geom_abline(intercept = 0, slope = 1) + 
  scale_x_continuous(limits=c(0, 1), expand = c(0, 0)) + 
  scale_y_continuous(limits=c(0, 1), expand = c(0, 0)) + 
  theme_minimal() + xlab("False Positive Rate") + ylab("True Positive Rate") +
  ggtitle("Training ROC, KNN")

print(curve)

#clean up
rm(curve,ROC,cutoffs,i, features, outcomes, knn_fit)




