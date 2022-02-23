#extract sex and spay/neuter status
temp <- separate(data[7], col = "SexuponOutcome", into = c("FixedStatus","Sex"), sep = " ")

#reassign spayed/neutered status to general value
temp[temp$FixedStatus=="Spayed",1] <- "Spayed-Neutered"
temp[temp$FixedStatus=="Neutered",1] <- "Spayed-Neutered"

#convert to factors
temp$FixedStatus <- as.factor(temp$FixedStatus)
temp$Sex <- as.factor(temp$Sex)

#add to data set
data <- cbind(data,temp)

#clean up
rm(temp)