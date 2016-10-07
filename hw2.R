library(e1071)
library(caret)

class_output <- read.csv("classification-output-data.csv")
#Raw Confusion Matrix
conf_matrix <- table(class_output$class,class_output$scored.class)
cat("Confusion Matrix\n")
print(conf_matrix)
#Rows represent acctual 
sum(conf_matrix[1,])==sum(class_output$class==0)
sum(conf_matrix[2,])==sum(class_output$class==1)

#Columns represent Predicted value
sum(conf_matrix[,1])==sum(class_output$scored.class==0)
sum(conf_matrix[,2])==sum(class_output$scored.class==1)

accuracy=function(df){
  #True positive - Actual class is positive and predicted class is positive
  TP = sum((df$class==1) & (df$scored.class==1))
  #True Negative - Actual class is negative and predicted class is negative
  TN = sum((df$class==0) & (df$scored.class==0))
  #False positive - Actual class is negative and predicted class is positive
  FP = sum((df$class==0) & (df$scored.class==1))
  #False Negative - Actual class is positive and predicted class is negative
  FN = sum((df$class==1) & (df$scored.class==0))
  #Compute Accuracy
  accuracy = (TP+TN)/(TP+TN+FP+FN)
  return(accuracy)
}

cat("\nAccuracy:",accuracy_var <- accuracy(class_output))

# Classification Error Rate
class_error_rate <- function(df){
  #True positive - Actual class is positive and predicted class is positive
  TP = sum((df$class==1) & (df$scored.class==1))
  #True Negative - Actual class is negative and predicted class is negative
  TN = sum((df$class==0) & (df$scored.class==0))
  #False positive - Actual class is negative and predicted class is positive
  FP = sum((df$class==0) & (df$scored.class==1))
  #False Negative - Actual class is positive and predicted class is negative
  FN = sum((df$class==1) & (df$scored.class==0))
  
  error_rate <- (FP+FN)/(TP+FP+TN+FN)
  return(error_rate)
}

cat("Classification Error Rate:",class_error_var <- class_error_rate(class_output))

# Check that Accuracy and Error Rate sums 1
print(accuracy_var + class_error_var)

# Sensitivity
sensitivity <- function(df){
  #True positive - Actual class is positive and predicted class is positive
  TP = sum((df$class==1) & (df$scored.class==1))
  #True Negative - Actual class is negative and predicted class is negative
  TN = sum((df$class==0) & (df$scored.class==0))
  #False positive - Actual class is negative and predicted class is positive
  FP = sum((df$class==0) & (df$scored.class==1))
  #False Negative - Actual class is positive and predicted class is negative
  FN = sum((df$class==1) & (df$scored.class==0))
  
  sensitivity <- TP/(TP+FN)
  return(sensitivity)
}

cat("Sensitivty: ", sensitivity_var <- sensitivity(class_output))

#Function to compute precision
precision=function(df){
  #True positive - Actual class is positive and predicted class is positive
  TP = sum((df$class==1) & (df$scored.class==1))
  #False positive - Actual class is negative and predicted class is positive
  FP = sum((df$class==0) & (df$scored.class==1))
  #Compute Precision 
  precision = TP/(TP+FP)
  return(precision)
}

cat("\nPrecision:", precision_var <- precision(class_output))

#Function to compute Specificity 
specificity=function(df){
  #True Negative - Actual class is negative and predicted class is negative
  TN = sum((df$class==0) & (df$scored.class==0))
  #False positive - Actual class is negative and predicted class is positive
  FP = sum((df$class==0) & (df$scored.class==1))
  #Compute Specificity
  specificity = TN/(TN+FP)
  return(specificity)
}

cat("Specificity:",specificity_var <- specificity(class_output))

f1score <- function(df, precision_var, sensitivity_var){
  #True positive - Actual class is positive and predicted class is positive
  TP = sum((df$class==1) & (df$scored.class==1))
  #True Negative - Actual class is negative and predicted class is negative
  TN = sum((df$class==0) & (df$scored.class==0))
  #False positive - Actual class is negative and predicted class is positive
  FP = sum((df$class==0) & (df$scored.class==1))
  #False Negative - Actual class is positive and predicted class is negative
  FN = sum((df$class==1) & (df$scored.class==0))
  
  f1score <- (2*precision_var*sensitivity_var)/(precision_var+sensitivity_var)
  return(f1score)
}

cat("\nF1Score: ", f1score_var <- f1score(class_output, precision_var, sensitivity_var))


#9 - The F1 score is calculated using Sensitivity and Specificty and lies on the ROC curve.
# By that definition the F1 score will always lie between 0 and 1

#12 Caret Package
caret_conf_matrix <- confusionMatrix(class_output$class,class_output$scored.class)
#Both the caret package and table function produce the same confusion matrix
print(caret_conf_matrix$table)
cat("Confusion Matrix\n")
print(conf_matrix)

#The Sensitivity created by caret does not match our own function (0.474 with caret vs 0.799 with our own)
print(caret_conf_matrix$byClass[1])
print(sensitivity_var)
#The Specificity is closer, but not a perfect match (0.96 with caret and 0.84 with our own)
print(caret_conf_matrix$byClass[2])
print(specificity_var)
