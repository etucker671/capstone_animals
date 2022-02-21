#Function that takes in a list of package names, check for their installation,
#and installs them if they're not there. 
#Libraries loaded for all packages.
package.names <- c("tidyverse","lubridate","randomForest","caret")

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

package.check(package.names)