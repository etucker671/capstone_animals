#extract month
data <- data %>% mutate(Month = month(DateTime, label = TRUE))
#remove ordering
data$Month <- factor(data$Month, ordered = FALSE)

#add season
data <- data %>% mutate(Season = char("0"))
data$Season[data$Month == "Jan" | data$Month == "Feb" | data$Month == "Mar"] <- "Winter"
data$Season[data$Month == "Apr" | data$Month == "May" | data$Month == "Jun"] <- "Spring"
data$Season[data$Month == "Jul" | data$Month == "Aug" | data$Month == "Sep"] <- "Summer"
data$Season[data$Month == "Oct" | data$Month == "Nov" | data$Month == "Dec"] <- "Fall"
data$Season <- factor(data$Season, levels = c("Spring","Summer","Fall","Winter"))


#### REPEAT FOR VALIDATION DATA ####

#extract month
val_data <- val_data %>% mutate(Month = month(DateTime, label = TRUE))
#remove ordering
val_data$Month <- factor(val_data$Month, ordered = FALSE)

#add season
val_data <- val_data %>% mutate(Season = char("0"))
val_data$Season[val_data$Month == "Jan" | val_data$Month == "Feb" | val_data$Month == "Mar"] <- "Winter"
val_data$Season[val_data$Month == "Apr" | val_data$Month == "May" | val_data$Month == "Jun"] <- "Spring"
val_data$Season[val_data$Month == "Jul" | val_data$Month == "Aug" | val_data$Month == "Sep"] <- "Summer"
val_data$Season[val_data$Month == "Oct" | val_data$Month == "Nov" | val_data$Month == "Dec"] <- "Fall"
val_data$Season <- factor(val_data$Season, levels = c("Spring","Summer","Fall","Winter"))

