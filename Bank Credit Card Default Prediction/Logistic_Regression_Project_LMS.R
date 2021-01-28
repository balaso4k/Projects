

# title: "Bank Credit Card Default Prediction"

# author: "Balaso Kumbhar"
# date: "Jan 28, 2021"

# Problem Statement

# The banks with the invent of credit card were more focused on the number of
# customers using their credit service but the drawback of them not being able to pay
# back the credit in time was an issue that soon followed, a system was in need to
# effectively decide the credit limit to be allowed to a person based on his previous
# credit history.
# Build a classification model using logistic regression to predict the credibility of the
# customer, in order to minimize the risk and maximize the profit of German Credit
# Bank.

# Set Working Directory
setwd("D:/R_Files Recovery/Projects/Project_2__imarticus")

# Loading Required Libraries

library(tidyverse)
library(GGally)
library(plyr)
library(knitr)
library(pROC)
library(caret)
library(rpart)
library(nnet)
library(car)


data <- read.csv('BankCreditCard.csv')

glimpse(data)

summary(data)

prop.table(table(data$Default_Payment )) # Have a class-imbalance



# Check for Missing Values 

sapply(data, function(x) {sum(is.na(x))})




## Data Pre-Processing

# Remove ID Variable Which is unique identfier 

data$Customer.ID  <- NULL


# Check for the correlation

round(cor(data),2)


# Check the Multicollinearity of independent variables using Variance Inflation Factor(VIF) function

model = lm(formula = Default_Payment ~ . , data = data )
summary(model)

vif(model)

# As per Variance Inflation Factor(VIF) is more than 5  we drop that particular variable
# So here the VIF of Jan_Bill_Amount, Feb_Bill_Amount, March_Bill_Amount, April_Bill_Amount,
# May_Bill_Amount, June_Bill_Amount is greater than the 5 so we have to reject that variables  

data <- data[ , -c(12:17)]

str(data)


# Factorization of variables discrete variables

data$Default_Payment <- as.factor(ifelse(data$Default_Payment == 1, "Yes", "No"))

data$Gender <- as.factor(ifelse(data$Gender == 1, "Male",
                                ifelse(data$Academic_Qualification == 2, "Female", "Do not prefer to say")))

data$Marital <- as.factor(ifelse(data$Marital == 1, "Married",
                                ifelse(data$Marital == 2, "Single", "Other")))

data$Academic_Qualification <- as.factor(ifelse(data$Academic_Qualification == 1, "Undergraduate",
                                                ifelse(data$Academic_Qualification == 2, "Graduate",
                                                       ifelse(data$Academic_Qualification == 3, "Postgraduate",
                                                              ifelse(data$Academic_Qualification == 4, "Professional",
                                                                     ifelse(data$Academic_Qualification == 5, "Other", "Unkown"))))))


data$Repayment_Status_Jan <- as.factor(data$Repayment_Status_Jan)

data$Repayment_Status_Feb <- as.factor(data$Repayment_Status_Feb)

data$Repayment_Status_March <- as.factor(data$Repayment_Status_March)

data$Repayment_Status_April <- as.factor(data$Repayment_Status_April)

data$Repayment_Status_May <- as.factor(data$Repayment_Status_May)

data$Repayment_Status_June <- as.factor(data$Repayment_Status_June)


## Create the test train samples for model

set.seed(123)

sam <- createDataPartition(data$Default_Payment,p = 0.7,list = F)

train_sam <- data[sam, ]

x_train_sam <- train_sam %>% select(-Default_Payment)

y_train_sam <- train_sam$Default_Payment

test_sam <- data[-sam,]


fiveMetric <- function(...) c(twoClassSummary(...), defaultSummary(...))

ctrl <- trainControl(method = "cv",number = 5,summaryFunction = fiveMetric,
                     classProbs = T, verboseIter = T)
              
# Apply Logistic Regression       

# Model 

set.seed(123)
glm_model <- train(Default_Payment ~ . , data = train_sam, method = "glmStepAIC", 
                   trControl = ctrl, preProcess = c("nzv","BoxCox"),  metric = "Accuracy", family = binomial())
glm_model
summary(glm_model)


#Predict The Model

class_pred <- predict(glm_model, newdata = test_sam, type = "raw")

prob_pred  <- predict(glm_model, newdata = test_sam, type = "prob")


confusionMatrix(class_pred, test_sam$Default_Payment)

# Plot ROC curve

glm_ROC <- roc(test_sam$Default_Payment, prob_pred[,"No"])
glm_ROC

plot(glm_ROC, print.auc = T,legacy.axis = T)




