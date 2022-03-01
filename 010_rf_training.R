#create RF dataset and fill NAs
rf_data <- select(final_data,-Month,-Season)
rf_data$AgeInYears[which(is.na(rf_data$AgeInYears)==TRUE)] <- median(na.omit(rf_data$AgeInYears))

#### TUNING (can skip) ####
set.seed(1, sample.kind = "Rounding")

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

#go with mtry = 3

#define ntree tuning function
rf_ntree <- function(x){
  #train random forest
  rf_fit_fxn <- randomForest(Euthanized ~ ., data = rf_data, ntree = x, sampsize = c(500,500), importance = TRUE, mtry = 3)
  
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

#go with ntree = 300

#define sample size tuning function
rf_sampsize <- function(x){
  #train random forest
  rf_fit_fxn <- randomForest(Euthanized ~ ., data = rf_data, ntree = 300, sampsize = c(x,x), importance = TRUE, mtry = 3)
  
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

#go with sampsize = 100

#clean up
rm(rf_data,ntrees,mtrys,sampsizes)
