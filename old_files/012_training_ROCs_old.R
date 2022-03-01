#combine all training ROC data
all_ROCs_train <- rbind(mutate(ROC_glm_train,Method = rep("GLM",101)), 
                        mutate(ROC_knn_train,Method = rep("KNN",101)), 
                        mutate(ROC_rf_train, Method = rep("RandomForest",101)))

#plot all training ROCs together
allplot <- all_ROCs_train %>% ggplot(aes(x = FPR, y = TPR, col = Method)) + 
  geom_line(size=1) + geom_abline(intercept = 0, slope = 1) + 
  scale_x_continuous(limits=c(0, 1), expand = c(0, 0)) + 
  scale_y_continuous(limits=c(0, 1), expand = c(0, 0)) + 
  theme_minimal() + xlab("False Positive Rate") + 
  ylab("True Positive Rate")

print(allplot)
rm(allplot)