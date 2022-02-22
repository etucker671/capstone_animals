#create conversion dataframe
ages <- data.frame(matrix(ncol = 4, nrow = nrow(data)))
x <- c("number", "unit_orig", "unit_new", "age_years")
colnames(ages) <- x
rm(x)

#Extract numbers
ages$number <- as.numeric(str_extract(data$AgeuponOutcome, "\\d+"))

#Extract units
ages$unit_orig <- str_extract(data$AgeuponOutcome, "[a-z]+")

#Check which units exist
table(ages$unit_orig)

#remove "s" to consolidate units
ages$unit_new <- str_remove(ages$unit_orig,"s")

#Check new units
table(ages$unit_new)

#Produce all values in years
ages <- ages %>% mutate(age_years = ifelse(unit_new == "day", number/365, ifelse(unit_new == "month", number/12, ifelse(unit_new == "week", number/52,number))))

#check output
ages

#merge into data
data_orig <- data
data <- cbind(data,AgeInYears = ages$age_years)

#clean up
rm(ages)
