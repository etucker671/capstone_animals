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

#extract data from train.csv
data <- read.csv("animaloutcomes.csv", header = TRUE)

#create binary euthanization feature
data <- data %>% mutate(Euthanized = OutcomeType == "Euthanasia")

#split data
set.seed(1, sample.kind = "Rounding")
index <- createDataPartition(data$Euthanized, p = 0.8, list = FALSE)
full_data <- data
data <- full_data[index,]
val_data <- full_data[-index,]

#clean up
rm(index)