##################################################################################
########## Title: Car sales
########## Company: Kaeggle
########## Name of Coder: Akash Thampi
########## Date: 3rd Jan 2019
##################################################################################

#### Uploading Libraries ####
library(caret)
library(dplyr)
library(ggplot2)
library(mice)
library(lubridate)
library(anytime)
library(magrittr)
library(tidyverse)
library(zoo)
library(corrplot)
library(ggcorrplot)
library(Boruta)

#### Upload Datasets ####
setwd("C:/Users/akash/OneDrive/Desktop/")

Car_sales <-
  read.csv(
    "Car_sales.csv",
    header = TRUE,
    sep = ",",
    na.strings = "."
  )

#### Data Cleaning ####
### Identifying Missing information ###
summary(Car_sales)

### converting formats ###

sapply(Car_sales1, class)
Car_sales$Latest.Launch <- as.Date(Car_sales$Latest.Launch)

Car_sales$Latest.Launch<-strptime(Car_sales$Latest.Launch, "%d-%B-%y")
Car_sales$Latest.Launch<-as.Date(Car_sales$Latest.Launch, "%d-%B-%y")
df<-Car_sales

md.pattern(Car_sales)

## 
### eliminating rows having data with missing values except for the 35 obsetvations x4 year.resale value#

na_df<- Car_sales[which(is.na(Car_sales$X4.year.resale.value)),]
summary(na_df) ## this is valuable information cannot be excluded #

## Eliminating NA's in all the Attributes other than the resale value #

na_col<- c(6:14)
na_observations<-which(is.na(Car_sales[,na_col]))

Car_sales1<- Car_sales[-na_observations,]
summary(Car_sales1)

Car_sales1<- Car_sales1[-which(is.na(Car_sales1$Curb.weight)),] 

boxplot(Car_sales1$X4.year.resale.value)

Car_sales1$X4.year.resale.value%<>% na.locf()
Car_sales1$Fuel.efficiency%<>% na.aggregate()


Car_sales1$Manufacturer %<>% as.factor()
Car_sales1$Model%<>% as.factor()

#### understanding importance of attributes to car sales ####
sapply(Car_sales1, class)
colss<- c(1:2,5,15)

dff<- Car_sales1[,-colss]
str(dff)


dff<- dff[,]
corrplot(cor(dff),order ="hclust")

ggcorrplot(cor(dff), hc.order = TRUE, type = "lower",
           lab = TRUE)

ggcorrplot(cor(dff), method = "circle", type = "lower",
           lab = TRUE)
### Variable importance ###

## runing the linear model ##
set.seed(123)
fitcontrol<- trainControl(method = "repeatedcv", number = 3, repeats = 2)

train_lm<- train(Sales.in.thousands~., data = Car_sales1, trControl= fitcontrol, method= "lm")

predictors(train_lm)

varImp(train_lm)

### Boruta ###

## Run Boruta ##

boruta.train<- Boruta(Sales.in.thousands~.,data=Car_sales1, doTrace= 20)
print(boruta.train)

### removing less important attributes from the varimp and correlation and boruta##
# remove Latest.launch, fuel.efficiency, vehicle type
str(Car_sales1)

hist(Car_sales1$Engine.size)

Car_sales2<- Car_sales1 %>% select(-Vehicle.type, -Latest.Launch, -Fuel.efficiency)

## Dividing data frame into test and train ##

set.seed(123)
inTrain<- createDataPartition(Car_sales2$Sales.in.thousands, p=0.8, list = FALSE)
trainset<- Car_sales2[inTrain,]
testset<- Car_sales2[-inTrain,]

## train on rf #
set.seed(123)
train_rf<- train(Sales.in.thousands~., data = trainset, method = "rf", trControl= fitcontrol)
predictors(train_rf)
print(train_rf)

# predict #

test_rf<- predict(train_rf,testset)
postResample(test_rf, testset$Sales.in.thousands)


## train on knn #

train_knn<- train(Sales.in.thousands~., data = trainset, method = "knn", trControl= fitcontrol)
predictors(train_knn)
print(train_knn)

# predict #

test_knn<- predict(train_knn,testset)
postResample(test_knn, testset$Sales.in.thousands)

## train on ranger-rf #

train_ranger<- train(Sales.in.thousands~., data = trainset, method = "ranger", trControl= fitcontrol,set.seed(123))
predictors(train_ranger)
print(train_ranger)

# predict #

test_ranger<- predict(train_ranger,testset)
postResample(test_ranger, testset$Sales.in.thousands)

## outlier detection ##
summary(Car_sales2)
# 
# # X4.year.resale.value # 
# boxplot(Car$X4.year.resale.value)
# 
# t = 3
# m1= mean(Car_sales2$X4.year.resale.value)
# s1=sd(Car_sales2$X4.year.resale.value)
# 
# b1_resale=m1-t*s1
# b2_resale= m1+t*s1
# 
# y1= ifelse(Car_sales2$X4.year.resale.value>= b1_resale &
#              Car_sales2$X4.year.resale.value<= b2_resale,0,1)
# vect_y1<- which(y1== 1)
# 
# Car<- Car_sales2[-vect_y1,]
# 
# ## Price.in thousanda #
# t = 3
# m2= mean(Car$Price.in.thousands)
# s2=sd(Car$Price.in.thousands)
# 
# b1_price=m2-t*s2
# b2_price= m2+t*s2
# 
# y2= ifelse(Car$Price.in.thousands>= b1_price &
#              Car$Price.in.thousands<= b2_price,0,1)
# vect_y2<- which(y2== 1)
# 
# Car<- Car[-vect_y2,]
# 
# 
# ## Dividing data frame into test and train ##
# 
# set.seed(123)
# inTrain<- createDataPartition(Car$Sales.in.thousands, p=0.8, list = FALSE)
# trainset<- Car[inTrain,]
# testset<- Car[-inTrain,]
# 
# ## train on rf #
# set.seed(123)
# train_rf<- train(Sales.in.thousands~., data = trainset, method = "rf", trControl= fitcontrol)
# predictors(train_rf)
# print(train_rf)
# 
# # predict #
# 
# test_rf<- predict(train_rf,testset)
# postResample(test_rf, testset$Sales.in.thousands)
# 
# 
# ## train on knn #
# 
# train_knn<- train(Sales.in.thousands~., data = trainset, method = "knn", trControl= fitcontrol)
# predictors(train_knn)
# print(train_knn)
# 
# # predict #
# 
# test_knn<- predict(train_knn,testset)
# postResample(test_knn, testset$Sales.in.thousands)
# 
# ## train on ranger-rf #
# 
# train_ranger<- train(Sales.in.thousands~., data = trainset, method = "ranger", trControl= fitcontrol,set.seed(123))
# predictors(train_ranger)
# print(train_ranger)
# 
# # predict #
# 
# test_ranger<- predict(train_ranger,testset)
# postResample(test_ranger, testset$Sales.in.thousands)
