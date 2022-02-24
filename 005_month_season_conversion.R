#extract month
data <- data %>% mutate(Month = month(DateTime, label = TRUE))

#add season
data <- data %>% mutate(Season = char("0"))
data$Season[data$Month == "Jan" | data$Month == "Feb" | data$Month == "Mar"] <- "Winter"
data$Season[data$Month == "Apr" | data$Month == "May" | data$Month == "Jun"] <- "Spring"
data$Season[data$Month == "Jul" | data$Month == "Aug" | data$Month == "Sep"] <- "Summer"
data$Season[data$Month == "Oct" | data$Month == "Nov" | data$Month == "Dec"] <- "Fall"
data$Season <- factor(data$Season, levels = c("Spring","Summer","Fall","Winter"))