#danger months
data <- data %>% mutate(MonthStatus = factor(ifelse(Month=="Mar"|Month=="Apr"|Month=="May"|Month=="Jun","Danger","Safe")))

#danger breeds
bottom60breeds <- data %>% mutate(Breed2 = str_remove(Breed," Mix")) %>% group_by(Breed2) %>% summarize(n = n(), euth_prop = mean(Euthanized == TRUE)) %>% arrange(euth_prop) %>% filter(n >= 10) %>% slice_min(., order_by = euth_prop, n = 60) %>% separate(., Breed2, into = c("1","2","3"), sep = "/") %>% print(n=nrow(.))
bottom60breeds2 <- c(as.vector(bottom60breeds$`1`),as.vector(bottom60breeds$`2`),as.vector(bottom60breeds$`3`)) %>% table() %>% as.data.frame() %>% arrange(-Freq)
colnames(bottom60breeds2) <- c("Breed","Freq")

top30breeds <- data %>% mutate(Breed2 = str_remove(Breed," Mix")) %>% group_by(Breed2) %>% summarize(n = n(), euth_prop = mean(Euthanized == TRUE)) %>% arrange(-euth_prop) %>% filter(n >= 10) %>% slice_max(., order_by = euth_prop, n = 30) %>% separate(., Breed2, into = c("1","2","3"), sep = "/") %>% print(n=nrow(.))
top30breeds2 <- c(as.vector(top30breeds$`1`),as.vector(top30breeds$`2`),as.vector(top30breeds$`3`)) %>% table() %>% as.data.frame() %>% arrange(-Freq)
colnames(top30breeds2) <- c("Breed","Freq")
hi_topbreeds <- c(as.vector(top30breeds$`1`),as.vector(top30breeds$`2`),as.vector(top30breeds$`3`)) %>% table() %>% as.data.frame() %>% arrange(-Freq) %>% filter(Freq > 1)
colnames(hi_topbreeds) <- c("Breed","Freq")

danger_breeds <- hi_topbreeds[!(hi_topbreeds$Breed %in% bottom60breeds2$Breed),1] %>% as.character()

safe_breeds <- bottom60breeds2[!(bottom60breeds2$Breed %in% top30breeds2$Breed),1] %>% as.character()

multi_string_detect<-function(x,y){
  temp<-sapply(y, function(z){str_detect(x, z)})
  apply(temp, 1, any)
}

BreedStatusTable <- data.frame(Breed = data$Breed, Danger = multi_string_detect(data$Breed,danger_breeds), Safe = multi_string_detect(data$Breed,safe_breeds)) %>% mutate(BreedStatus = ifelse(Danger == TRUE, "Danger", ifelse(Safe == TRUE, "Safe", "Neutral")))

data <- data %>% mutate(BreedStatus = BreedStatusTable$BreedStatus)

#clean up
rm(bottom60breeds,bottom60breeds2,BreedStatusTable,hi_topbreeds,top30breeds,top30breeds2)

