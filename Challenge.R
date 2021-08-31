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

b<-ggplot(data=data,aes(x=gender,y=bill))
b+geom_boxplot()+scale_y_continuous(labels=dollar)+
  ggtitle("Boxplot of total bill by gender") +
  xlab("Gender") + ylab("Total bill amount")

c<-ggplot(data=data,aes(x=race,y=bill))
c+geom_boxplot()+scale_y_continuous(labels=dollar)+
  ggtitle("Boxplot of total bill by race") +
  xlab("Race") + ylab("Total bill amount")

d<-ggplot(data=data,aes(x=Age,y=bill))
d+geom_jitter()+scale_y_continuous(labels=dollar)+
  ggtitle("Scatter plot of total bill against age") +
  xlab("Age (years)") + ylab("Total bill amount")

e<-ggplot(data=data,aes(x=weight,y=bill))
e+geom_jitter()+scale_y_continuous(labels=dollar)+
  ggtitle("Scatter plot of total bill against weight") +
  xlab("Weight (kg)") + ylab("Total bill amount")

#linear regression model
lm1<-lm(log(bill)~gender, data=data)
summary(lm1)
exp(coef(lm1)[2])

lm2<-lm(log(bill)~race, data=data)
summary(lm2)
exp(coef(lm2)[2])
exp(coef(lm2)[3])
exp(coef(lm2)[4])

data<-within(data, resident_status<-relevel(resident_status, ref = 3))
lm3<-lm(log(bill)~resident_status, data=data)
summary(lm3)
exp(coef(lm3)[2])
exp(coef(lm3)[3])

lm4<-lm(log(bill)~Age, data=data)
summary(lm4)
exp(coef(lm4)[2])

lm5<-lm(log(bill)~lab_result_3, data=data)
summary(lm5)
exp(coef(lm5)[2])

lm6<-lm(log(bill)~symptom_5, data=data)
summary(lm6)
exp(coef(lm6)[2])

lm7<-lm(log(bill)~preop_medication_6, data=data)
summary(lm7)
exp(coef(lm7)[2])

lm8<-lm(log(bill)~medical_history_7, data=data)
summary(lm8)
exp(coef(lm8)[2])

lm9<-lm(log(bill)~weight, data=data)
summary(lm9)
exp(coef(lm9)[2])

