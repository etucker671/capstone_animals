#extract sex and spay/neuter status
temp <- separate(data[7], col = "SexuponOutcome", into = c("FixedStatus","Sex"), sep = " ")

#reassign spayed/neutered status to general value
temp[temp$FixedStatus=="Spayed",1] <- "Spayed-Neutered"
temp[temp$FixedStatus=="Neutered",1] <- "Spayed-Neutered"

#convert blanks to Unknown
temp[temp$FixedStatus=="",1] <- "Unknown"
temp[is.na(temp$Sex),2] <- "Unknown"

#convert to factors
temp$FixedStatus <- factor(temp$FixedStatus, levels = c("Spayed-Neutered", "Intact", "Unknown"))
temp$Sex <- factor(temp$Sex)

#add to data set
data <- cbind(data,temp)

#clean up
rm(temp)


#### REPEAT FOR VALIDATION SET ####

#extract sex and spay/neuter status
temp <- separate(val_data[7], col = "SexuponOutcome", into = c("FixedStatus","Sex"), sep = " ")

#reassign spayed/neutered status to general value
temp[temp$FixedStatus=="Spayed",1] <- "Spayed-Neutered"
temp[temp$FixedStatus=="Neutered",1] <- "Spayed-Neutered"

#convert blanks to Unknown
temp[temp$FixedStatus=="",1] <- "Unknown"
temp[is.na(temp$Sex),2] <- "Unknown"

#convert to factors
temp$FixedStatus <- factor(temp$FixedStatus, levels = c("Spayed-Neutered", "Intact", "Unknown"))
temp$Sex <- factor(temp$Sex)

#add to data set
val_data <- cbind(val_data,temp)

#clean up
rm(temp)