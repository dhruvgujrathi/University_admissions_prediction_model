install.packages("ISLR") #Package containing 'College' dataset
install.packages("tidyverse") #Data Manipulation & Visualization
install.packages("Hmisc") #Imputing missing values, high level graphics, etc.
install.packages("funModeling") 
install.packages("stats")
library(ISLR) 
library(tidyverse)
library(Hmisc)
library(funModeling)
library(stats)
data("College") #Selecting dataset 'College'
# defining this function
getdata <- function(ISLR)
{
  e <- new.env()
  name <- data(College, envir = e)[1]
  e[[name]]
}

#Loading the dataset calling getdata()
x <- getdata("College")
x
#Performing EDA
basic_eda <- function(x) #Performing exploratory data analysis
{
  glimpse(x)
  print(status(x))
  freq(x) 
  print(profiling_num(x))
  plot_num(x)
  describe(x)
}
basic_eda(x)
#Performing Descriptive Statistics
summary(x)
mean(x$Grad.Rate)
median(x$Grad.Rate)
install.packages("modeest")
library(modeest)
mfv((x$Grad.Rate))
min(x$Grad.Rate)
max(x$Grad.Rate)
range(x$Grad.Rate)
var(x$Grad.Rate)
sd(x$Grad.Rate)
#Plotting the descriptive statistical characteristics
install.packages("ggpubr")
library(ggpubr)
ggboxplot(x, y = "Grad.Rate", width = 0.5)
gghistogram(x, x = "Grad.Rate", ylab = "No. of Students", bins = 9, 
            add = "mean")
ggqqplot(x, x = "Grad.Rate")
hist(x$Grad.Rate) #Plotting Histogram
plot(x$Grad.Rate) #Plotting Scatterplot

# Creating Train and Test set - maintaining % of event rate (70/30 split) library(caret)
set.seed(3456) #Setting the seed for future use
trainIndex <- sample(seq_len(nrow(College)), size = smp_size) # as it wouldn't balance the classes. Need upSample/downSample
college_train <- x[ trainIndex,] #Splitting the dataset
college_test <- x[ -trainIndex,] #Splitting the dataset
cat("Created college_train with ", nrow(college_train), " rows\n" ) #Created training set
cat("Created college_test with ", nrow(college_test), " rows" ) #Created Testing Set

table_train <- table(college_train$Private)
cat("\nTable of labels for train is = ", table_train) #table(college_train$Private))
table_train
table_test <- table(college_test$Private)
cat("\nTable of labels for test is = ", table(college_test$Private)) #table(college_test$Private))

# Fitting the regression model using 'glm' function
formula <-  Private ~ F.Undergrad + Outstate + Terminal + P.Undergrad + Apps
college_model = glm(formula = formula, family = "binomial", data = college_train, control = list(maxit = 50))
summary(college_model)

# Prediction model
predicted_test <- predict(college_model, college_test, type="response" )
predicted_test
install.packages("caret")
library(caret)
install.packages("InformationValue")
library(InformationValue)

# calculate Confusion Matrix with the optimal cutoff
optCutOff <- optimalCutoff(college_test$Private, predicted_test)[1]
cat("\nOptimal cutoff (threshold) is = ", optCutOff)

#Creating Confusion matrix
confusion_matrix <- InformationValue::confusionMatrix(college_test$Private,predicted_test, threshold = 0.5) #optCutOff) # try 0.5, explain tradeoff
confusion_matrix # predicted (columns), actual (rows)
#Creating Precision set
precision = confusion_matrix[1,1] / (confusion_matrix[1,2] + confusion_matrix[1,1])
cat("\nPrecision ", precision)
#Creating Specificity set
specificity   = confusion_matrix[2,2] / (confusion_matrix[2,2] + confusion_matrix[2,1])
cat("\nSpecificity   ", specificity  )
#Creating Recall set
recall = confusion_matrix[1,1] / (confusion_matrix[2,1] + confusion_matrix[1,1])
cat("\nRecall ", recall)
#Creating Accuracy set
accuracy = (confusion_matrix[1,1] + confusion_matrix[2,2]) / sum(confusion_matrix)
cat("\nAccuracy ", accuracy)
#Installing ggplot2 package
install.packages("ggplot2")
library(ggplot2)
#Installing pROC package
install.packages("pROC")
library(pROC)
#Plotting the ROC Curve
plot(roc(college_test$Private,predicted_test, legacy.axes=TRUE))
#Determining the AUC
auc(roc(college_test$Private,predicted_test, legacy.axes=TRUE))
