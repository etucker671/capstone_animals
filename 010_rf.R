#create RF dataset and fill NAs
rf_data <- select(final_data,-Month,-Season)
rf_data$AgeInYears[which(is.na(rf_data$AgeInYears)==TRUE)] <- median(na.omit(rf_data$AgeInYears))

#### TUNING (can skip) ####

#define mtry tuning function
rf_mtry <- function(x){
  #train random forest
  rf_fit_fxn <- randomForest(Euthanized ~ ., data = rf_data, ntree = 200, sampsize = c(500,500), importance = TRUE, mtry = x)
  
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
  auc_fxn <- integrate(approxfun(x = ROC_fxn$FPR, y = ROC_fxn$TPR, ties = mean), min(ROC_fxn$FPR), max(ROC_fxn$FPR))$value
  return(auc_fxn)
}

#try different values for mtry
mtrys <- seq(1:6)
data.frame(mtry = mtrys, AUC = sapply(mtrys,FUN=rf_mtry))

#go with mtry = 2

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
  auc_fxn <- integrate(approxfun(x = ROC_fxn$FPR, y = ROC_fxn$TPR, ties = mean), min(ROC_fxn$FPR), max(ROC_fxn$FPR))$value
  return(auc_fxn)
}

#try different values of ntree
ntrees <- c(1,3,5,10,15,25,50,75,125,200,325,525,850)
data.frame(ntree = ntrees, AUC = sapply(ntrees,FUN=rf_ntree))

#try a more finely-tuned ntree series past 100
ntrees <- c(100,125,150,175,200,225,250)
data.frame(ntree = ntrees, AUC = sapply(ntrees,FUN=rf_ntree))

#try a more finely-tuned ntree series under 15
ntrees <- seq(1:15)
data.frame(ntree = ntrees, AUC = sapply(ntrees,FUN=rf_ntree))

#go with n = 125

#define sample size tuning function
rf_sampsize <- function(x){
  #train random forest
  rf_fit_fxn <- randomForest(Euthanized ~ ., data = rf_data, ntree = 125, sampsize = c(x,x), importance = TRUE, mtry = 2)
  
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
  auc_fxn <- integrate(approxfun(x = ROC_fxn$FPR, y = ROC_fxn$TPR, ties = mean), min(ROC_fxn$FPR), max(ROC_fxn$FPR))$value
  return(auc_fxn)
}

#try different values for sampsize
sampsizes <- c(25,50,75,100,200,350,500,750,1000,1500)
data.frame(sampsize = sampsizes, AUC = sapply(sampsizes,FUN=rf_sampsize))

#go with sampsize = 1000

#clean up
rm(i,rf_data,ntrees,mtrys,sampsizes)
