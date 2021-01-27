
# title: "House Prices Prediction Using Linear regression technique"
# author: "Balaso Kumbhar"
# date: "Jan 26, 2021"

# Problem Statement

# There are a number of factors which determine property prices, some are logical,
# based on economic theories and population density and some are based on more
# intangible factors, like availability of amenities & necessities, neighborhood, etc.
# Build a linear regression model to predict the price of the property from the dataset
# having attributes such as sale type, sale condition etc

# Set Working Directory
setwd("D:/R_Files Recovery/Projects/Project_1_imarticus")


library('scales')
library('dplyr') 
library('mice')
library('data.table')
library('gridExtra')
library('e1071')

# Reading Data set

pp_train <-read.csv("Property_Price_Train.csv", stringsAsFactors = F)
pp_test  <-read.csv("Property_Price_Test.csv", stringsAsFactors = F)

dim(pp_train)
dim(pp_test)

str(pp_train)
str(pp_test)

# Number of columns that consists of Character data in pp_train data set

sum(sapply(pp_train[ ,1:81], typeof) == "character")

# Number of columns that consists of  numerical data in pp_train data set

sum(sapply(pp_train[,1:81], typeof) == "integer")

# converting the numeric data type in integer

pp_train$Garage_Area <- as.integer(pp_train$Garage_Area)
pp_train$W_Deck_Area <- as.integer(pp_train$W_Deck_Area)
pp_train$Open_Lobby_Area <- as.integer(pp_train$Open_Lobby_Area)
pp_train$Enclosed_Lobby_Area <- as.integer(pp_train$Enclosed_Lobby_Area)



# Summary 

summary(pp_train[,sapply(pp_train[,1:81], typeof) == "integer"])


cat('pp_Train has', dim(pp_train)[1], 'rows and', dim(pp_train)[2], 'columns.') # Train has 1460 rows and 81 columns.


cat('pp_Test has', dim(pp_test)[1], 'rows and', dim(pp_test)[2], ' columns.') # Test has 1459 rows and 80  columns.



# The percentage of data missing in pp_train

sum(is.na(pp_train)) / (nrow(pp_train) *ncol(pp_train))


# The percentage of data missing in pp_test

sum(is.na(pp_test)) / (nrow(pp_test) * ncol(pp_test))


# duplicated rows in pp_train

cat("The number of duplicated rows are", nrow(pp_train) - nrow(unique(pp_train)))



summary(pp_train)
summary(pp_test)



# Now deal with missing values

# Fireplace_quality, Pool_quality, fence_quality, Miscellaneous_feature, Lane type
# since here more than 50% values are null values, let us ignore/remove those columns

col_to_be_dropped  <- c("Fireplace_Quality"  , "Pool_Quality" ,
                        "Fence_Quality" ,
                        "Miscellaneous_Feature" , "Lane_Type")

pp_train <- pp_train[ , (!names(pp_train) %in% col_to_be_dropped)]
pp_test <- pp_test[ , (!names(pp_test) %in% col_to_be_dropped)]


dim(pp_train)
dim(pp_train)

# Combine The train And test Data Set

pp_test$Sale_Price<-rep(NA,1459)

prop_price <- bind_rows(pp_train,pp_test)

str(prop_price)

summary(prop_price)

dim(prop_price)

summary(prop_price)

colSums(is.na(prop_price))

# Replace the messing values with their median and mode values

prop_price$Zoning_Class[is.na(prop_price$Zoning_Class)]                <- "RLD"
prop_price$Lot_Extent[is.na(prop_price$Lot_Extent)]                    <- 70
prop_price$Utility_Type[is.na(prop_price$Utility_Type)]                <- 'AllPub'
prop_price$Exterior1st[is.na(prop_price$Exterior1st)]                  <- 'VinylSd'
prop_price$Exterior2nd[is.na(prop_price$Exterior2nd)]                  <- 'Exterior2nd'
prop_price$Brick_Veneer_Type[is.na(prop_price$Brick_Veneer_Type)]      <- 'None'
prop_price$Brick_Veneer_Area[is.na(prop_price$Brick_Veneer_Area)]      <- 103
prop_price$Basement_Height[is.na(prop_price$Basement_Height)]          <- "TA"
prop_price$Basement_Condition[is.na(prop_price$Basement_Condition)]    <- "TA"
prop_price$Exposure_Level[is.na(prop_price$Exposure_Level)]            <- "No"
prop_price$BsmtFinType1[is.na(prop_price$BsmtFinType1)]                <- "Unf"
prop_price$BsmtFinSF1[is.na(prop_price$BsmtFinSF1)]                    <- 368
prop_price$BsmtFinType2[is.na(prop_price$BsmtFinType2)]                <- "Unf"
prop_price$BsmtFinSF2[is.na(prop_price$BsmtFinSF2)]                    <- 50
prop_price$BsmtUnfSF[is.na(prop_price$BsmtUnfSF)]                      <- 467
prop_price$Total_Basement_Area[is.na(prop_price$Total_Basement_Area)]  <- 989
prop_price$Electrical_System[is.na(prop_price$Electrical_System)]      <- "SBrkr"
prop_price$Underground_Full_Bathroom[is.na(prop_price$Underground_Full_Bathroom)]      <- 0
prop_price$Underground_Half_Bathroom[is.na(prop_price$Underground_Half_Bathroom)]      <- 0
prop_price$Kitchen_Quality[is.na(prop_price$Kitchen_Quality)]          <- 'TA'
prop_price$Functional_Rate[is.na(prop_price$Functional_Rate)]          <- 'TF'
prop_price$Garage[is.na(prop_price$Garage)]                            <- 'Attchd'
prop_price$Garage_Built_Year[is.na(prop_price$Garage_Built_Year)]      <- 2005
prop_price$Garage_Finish_Year[is.na(prop_price$Garage_Finish_Year)]    <- "Unf"
prop_price$Garage_Size[is.na(prop_price$Garage_Size)]                  <- 2
prop_price$Garage_Area[is.na(prop_price$Garage_Area)]                  <- 477
prop_price$Garage_Quality[is.na(prop_price$Garage_Quality)]            <- 'TA'
prop_price$Garage_Condition[is.na(prop_price$Garage_Condition)]        <- 'TA'
prop_price$Sale_Type[is.na(prop_price$Sale_Type)]                      <- 'WD'


# Factorizing the categorical variables

prop_price$Brick_Veneer_Type <- factor(prop_price$Brick_Veneer_Type)
prop_price$Basement_Height <- factor(prop_price$Basement_Height)
prop_price$Basement_Condition <- factor(prop_price$Basement_Condition)
prop_price$Exposure_Level <- factor(prop_price$Exposure_Level)
prop_price$BsmtFinType1 <- factor(prop_price$BsmtFinType1)
prop_price$BsmtFinType2 <- factor(prop_price$BsmtFinType2)
prop_price$Electrical_System <- factor(prop_price$Electrical_System)
prop_price$Garage <- factor(prop_price$Garage)
prop_price$Garage_Finish_Year <- factor(prop_price$Garage_Finish_Year)
prop_price$Garage_Quality <- factor(prop_price$Garage_Quality)
prop_price$Garage_Condition <- factor(prop_price$Garage_Condition)
prop_price$Zoning_Class <- factor(prop_price$Zoning_Class)
prop_price$Road_Type <- factor(prop_price$Road_Type)
prop_price$Property_Shape <- factor(prop_price$Property_Shape )
prop_price$Land_Outline <- factor(prop_price$Land_Outline)
prop_price$Utility_Type <- factor(prop_price$Utility_Type)
prop_price$Lot_Configuration <- factor(prop_price$Lot_Configuration)
prop_price$Property_Slope <- factor(prop_price$Property_Slope)
prop_price$Neighborhood <- factor(prop_price$Neighborhood)
prop_price$Condition1 <- factor(prop_price$Condition1)
prop_price$Condition2 <- factor(prop_price$Condition2)
prop_price$House_Type <- factor(prop_price$House_Type)
prop_price$House_Design <- factor(prop_price$House_Design)
prop_price$Roof_Design <- factor(prop_price$Roof_Design)
prop_price$Roof_Quality <- factor(prop_price$Roof_Quality)
prop_price$Exterior1st <- factor(prop_price$Exterior1st)
prop_price$Exterior2nd <- factor(prop_price$Exterior2nd)
prop_price$Exterior_Material <- factor(prop_price$Exterior_Material)
prop_price$Exterior_Condition <- factor(prop_price$Exterior_Condition)
prop_price$Foundation_Type <- factor(prop_price$Foundation_Type)
prop_price$Heating_Type <- factor(prop_price$Heating_Type)
prop_price$Heating_Quality <- factor(prop_price$Heating_Quality)
prop_price$Air_Conditioning <- factor(prop_price$Air_Conditioning)
prop_price$Kitchen_Quality <- factor(prop_price$Kitchen_Quality)
prop_price$Functional_Rate <- factor(prop_price$Functional_Rate)
prop_price$Pavedd_Drive <- factor(prop_price$Pavedd_Drive)
prop_price$Sale_Type <- factor(prop_price$Sale_Type)
prop_price$Sale_Condition <- factor(prop_price$Sale_Condition)
prop_price$Construction_Year <- factor(prop_price$Construction_Year)
prop_price$Remodel_Year <- factor(prop_price$Remodel_Year)
prop_price$Garage_Built_Year <- factor(prop_price$Garage_Built_Year)
prop_price$Garage_Finish_Year <- factor(prop_price$Garage_Finish_Year)
prop_price$Construction_Year <- factor(prop_price$Construction_Year)


summary(prop_price)


# Convert the factors in numeric variables

col_names  <- c("Zoning_Class" ,  "Road_Type" , "Property_Shape" , "Land_Outline" , 
                "Utility_Type" , "Lot_Configuration" , "Property_Slope" ,
                "Neighborhood" , "Condition1" , "Condition2" , "House_Type" , 
                "House_Design" , "Roof_Design" , "Roof_Quality",
                "Exterior1st" , "Exterior2nd", "Brick_Veneer_Type" ,"Exterior_Material", 
                "Exterior_Condition", "Foundation_Type",
                "Basement_Height" ,  "Basement_Condition" , "Exposure_Level" ,
                "BsmtFinType1" ,"BsmtFinType2" ,"Heating_Type",
                "Heating_Quality" , "Air_Conditioning" , "Electrical_System" , "Kitchen_Quality",
                "Garage", "Garage_Finish_Year", "Garage_Quality" , 
                "Garage_Condition" , "Pavedd_Drive"  , "Sale_Type" , "Sale_Condition", "Functional_Rate")

prop_price[col_names]   <- sapply(prop_price[col_names] , as.numeric)

# Remove the ID variable

prop_price <- within(prop_price, rm(Id))


# determining skew of each numeric variable

pp_Column_classes <- sapply(names(prop_price),function(x){class(prop_price[[x]])})
pp_numeric_columns <-names(pp_Column_classes[pp_Column_classes != "factor"])

pp_skew <- sapply(pp_numeric_columns,function(x){skewness(prop_price[[x]],na.rm = T)})

pp_skew <- pp_skew[pp_skew > 0.75]

# transform excessively skewed features with log(x + 1)

for(x in names(pp_skew)) {
  prop_price[[x]] <- log(prop_price[[x]] + 1)
}

colSums(is.na(prop_price))


# Again split data set 

# Train and test dataset creation

pp_train_new <- prop_price[c(1:1459), ]

dim(pp_train_new)

str(pp_train_new)


pp_new <- pp_train_new[  , c(1:17,20:56,59:63,67:75)]

colSums(is.na(pp_train_new))

# Sampling the train data 
set.seed(123)
pp_sam  <-  sample( 2 , nrow(pp_new), replace = T , prob = c(0.8 ,0.2))

train  <-  pp_new[ pp_sam == 1 , ]
test  <-  pp_new[ pp_sam == 2 , ]

dim(train)
dim(test)

# Build the model
pp_model <- lm( Sale_Price ~ . , data = train)
summary(pp_model)

# Predict the model on train data
pred_train <- predict(pp_model , train)

# Predict the model on test data
pred_test <- predict(pp_model,test)

# Error in train data prediction
error_train <- train$Sale_Price - pred_train
mean(error_train)   # mean of the errors should be close to zero


# RMSE Calculation

# Error in test data prediction
error_test <- test$Sale_Price - pred_test
mean(error_test)   # mean of the errors should be close to zero

# Calculate Mean Squared error
MSE <-  mean(error_test^2)
MSE

# Calculate RMSE

RMSE <- MSE ^ 0.5
RMSE

# RMSE  =  0.1698795


