setwd("D:/git/Challenge_hui-zhen-tam/Healthcare Data Challenge Data")

bill_id=read.csv("bill_id.csv")
bill_amount=read.csv("bill_amount.csv")
bill <- merge(bill_id,bill_amount,by="bill_id")

clinical_data=read.csv("clinical_data.csv")
demographics=read.csv("demographics.csv")
colnames(demographics)[1] <- "id"
colnames(bill)[2] <- "id"
patient <- merge(clinical_data,demographics,by="id")

data <- merge(bill,patient,by=c("id","date_of_admission"))

library(dplyr)
total <- data %>% group_by(id,date_of_admission) %>%
  summarise(total_bill = sum(amount))

complete <- merge(total,patient,by=c("id","date_of_admission"))
