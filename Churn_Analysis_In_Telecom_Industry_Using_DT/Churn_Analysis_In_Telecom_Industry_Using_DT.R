# title: "Churn Analysis In Telecom Industry"
# author: "Balaso Kumbhar"
# date: "Feb 01, 2021"


# Problem Statement

# Customer churn occurs when customers or subscribers stop doing business with a company
# or service, also known as customer attrition. It is also referred as loss of clients or
# customers. One industry in which churn rates are particularly useful is the
# telecommunications industry, because most customers have multiple options from which to
# choose within a geographic location.
# Build classification models using decision tree algorithm to predict whether the customer be
# churned or not on the basis of its billing information and customer demographics.

# Set Working Directory
setwd("D:/R_Files Recovery/Projects/Project_3_imarticus")

# Importing necessary libraries

library(dplyr)
library(rpart)
library(rpart.plot)
library(randomForest)
library(ggplot2)
library(plyr)
library(caret)
library(pROC)
library(corrplot)
library(party)

# Reading Data set

data <-read.csv("churn.csv")

# Data Preprocessing

# Check the missing data

sapply(data,function(x) sum(is.na(x)))

# There is no misssing values present in the data set


# Normalizing quantitative variables (MonthlyServiceCharges and TotalAmount)

data[,19:20]=scale(data[,19:20])


# Creating features for variable "tenure"

for(i in 1:nrow(data)){
  if(data[i,"tenure"] >= 0 & data[i,"tenure"] <=12){
    data$tenure[i]<-"Between 0 & 12 months"
  }else if(data[i,"tenure"] > 12 & data[i,"tenure"] <= 24){
    data$tenure[i]<-"Between 1 & 2 years"
  }else if(data[i,"tenure"] > 24 & data[i,"tenure"] <= 36){
    data$tenure[i]<-"Between 2 & 3 years"
  }else if(data[i,"tenure"] > 36 & data[i,"tenure"] <= 48){
    data$tenure[i]<-"Between 3 & 4 years"
  }else if(data[i,"tenure"] > 48 & data[i,"tenure"] <= 60){
    data$tenure[i]<-"Between 4 & 5 years" 
  }else {data$tenure[i] <- "> 5 years"
  }
}


data$SeniorCitizen=as.factor(ifelse(data$SeniorCitizen==1, 'Yes', 'No'))


# We will change "No phone service" to "No" for column "MultipleLines"

data$MultipleConnections <- as.factor(mapvalues(data$MultipleConnections,from=c("No phone service"),to=c("No")))
                                           

# We will change "No internet service" to "No" for six columns, they are:
#   
# OnlineSecurity  
# OnlineBackup
# DeviceProtectionService
# TechnicalHelp
# OnlineTV
# OnlineMovies

col <- c(10:15)
for(i in 1:ncol(data[,col])) {
  data[,col][,i] <- as.factor(mapvalues (data[,col][,i], from =c("No internet service"),to=c("No")))
}


# Factorization of remaining variables


data$gender=factor(data$gender)
data$Partner=factor(data$Partner)
data$Dependents=factor(data$Dependents)
data$tenure=factor(data$tenure)
data$CallService=factor(data$CallService)
data$InternetConnection=factor(data$InternetConnection)
data$Agreement=factor(data$Agreement)
data$BillingMethod=factor(data$BillingMethod)
data$PaymentMethod=factor(data$PaymentMethod)
data$Churn=factor(data$Churn)

str(data)

data_new <- data[ ,  c(2:21)]

dim(data_new)

# Modeling

# splitting the data set into train and test

set.seed(123)


sam <- sample( 2, nrow(data_new), replace = T, prob = c(0.8,0.2))
sam

train <- data_new[ sam == 1 , ]

test <- data_new[ sam == 2 , ]



# Correlation matrix between quantitative variables

corrmatrix<-round(cor(x=data[,c("MonthlyServiceCharges","TotalAmount")],y=data[,c("MonthlyServiceCharges","TotalAmount")]),2)
corrplot(corrmatrix,method="ellipse")

# MonthlyServiceCharges and TotalAmount are correlated, we'll keep TotalCharges


# Model Building

# model_1
tree <- ctree(Churn ~ . , data = train , controls = ctree_control( mincriterion = .99, minsplit = 2000))
plot(tree)

prediction <- predict(tree, test)

# Confusion Matrix

cm <- confusionMatrix(prediction, test$Churn)
cm 

# Model_2

tree <- ctree(Churn ~ . , data = train , controls = ctree_control( mincriterion = .99, minbucket = 2000))
plot(tree)

prediction <- predict(tree, test)

# Confusion Matrix

cm <- confusionMatrix(prediction, test$Churn)
cm

# Model_3

tree <- ctree(Churn ~ . , data = train, controls = ctree_control(mincriterion = .99, maxdepth = 5 , minsplit = 1500) )
plot(tree)

prediction <- predict(tree, test)

# Confusion Matrix

cm <- confusionMatrix(prediction, test$Churn)
cm

# Model_4

tree <- rpart(Churn ~ ., data = train, method = "class")
rpart.plot(tree, type=1, extra=100, branch.lty=3, box.palette="RdYlGn", tweak = 1.2, fallen.leaves = FALSE)

prediction <- predict(tree, test, type = "class")


# Confusion Matrix

cm <- confusionMatrix(prediction, test$Churn)
cm

# Model 4 has high accuracy which is built by using rpart package 
