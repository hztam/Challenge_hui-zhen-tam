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
install.packages("tidyverse")
library(ggplot2)
require(ggplot2)
require(scales)
install.packages("gmodels")
library(gmodels)

data=read.table("complete.csv",sep=",",header=T)
data$resident_status=factor(data$resident_status)
data$race=factor(data$race)
data$gender=factor(data$gender)

#distribution of variables
mean(data$bill)
summary(data$bill)
quantile(data$bill)
CrossTable(data$gender)

CrossTable(data$race)
ggplot(data=data,aes(y=race))+geom_bar(color="black")+coord_flip()+
  ggtitle("Number of patient by race") +
  xlab("Count") + ylab("Race")

summary(data$Age)
ggplot(data=data,aes(y=Age))+geom_histogram(binwidth=1, color="black")+coord_flip()+
  ggtitle("Histogram of patient age") +
  xlab("Density") + ylab("Age (years)")

summary(data$weight)
ggplot(data=data,aes(y=weight))+geom_histogram(binwidth=1, color="black")+coord_flip()+
  ggtitle("Distribution of patient weight") +
  xlab("Density") + ylab("Weight (kg)")

summary(data$height)
ggplot(data=data,aes(y=height))+geom_histogram(binwidth=1, color="black")+coord_flip()+
  ggtitle("Distribution of patient height") +
  xlab("Density") + ylab("height (cm)")

summary(data$bill)
ggplot(data=data,aes(y=bill))+geom_histogram(binwidth=700, color="black")+coord_flip()+
  ggtitle("Distribution of bill amount") +
  xlab("Density") + ylab("Bill amount")
ggplot(data=data,aes(y=log(bill)))+geom_histogram(binwidth=0.05, color="black")+coord_flip()+
  ggtitle("Distribution of bill amount") +
  xlab("Density") + ylab("Bill amount")

a<-ggplot(data=data,aes(x=resident_status,y=bill))
a+geom_boxplot()+scale_y_continuous(labels=dollar)+
  ggtitle("Boxplot of total bill by resident status") +
  xlab("Resident status") + ylab("Total bill amount")

plot(bill~gender,data=data)
plot(bill~resident_status,data=data)


