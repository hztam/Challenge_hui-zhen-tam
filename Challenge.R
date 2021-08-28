setwd("D:/git/Challenge_hui-zhen-tam/Healthcare Data Challenge Data")

#merge dataset
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
colnames(complete)[3] <- "bill"

write.csv(complete, file = "complete.csv")

#############################################################
#distribution of variables
install.packages("tidyverse")
library(ggplot2)

data=read.table("complete.csv",sep=",",header=T)
data$resident_status=factor(data$resident_status)
data$race=factor(data$race)
data$gender=factor(data$gender)

a<-ggplot(data=data,aes(x=resident_status,y=bill))
a+ geom_point()+ggtitle("Plot of total bill by resident status") +
  xlab("Resident status") + ylab("Total bill amount")

ggplot

plot(bill~gender,data=data)
plot(bill~resident_status,data=data)

mean(bill)
summary(bill)
quantile(bill)
