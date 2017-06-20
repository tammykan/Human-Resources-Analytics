## app.R ##
# 1052 nccu data science final project
# Human Resources Analytics

# load library
library(ROCR)
library(randomForest)
library(rpart)
library(rpart.plot)
library(party)

library(e1071)
library(caret)
library(corrplot)
library(dplyr)


# sensitivity = TP/(TP+FN) = recall
# specificity = TN/(TN+FP)
# percision = TP/(TP+FP)
# f1 = 2*precision*recall/(precision+recall)
# eval <- prediction(d$pred.score,d$reference)
# auc <- round(attributes(performance(eval,'auc'))$y.values[[1]], digits = 2)

# get the values of sensitivity, specificity, precision, f1 and recall
getPerformanceTable <- function(model, matrix) {
  sensitivity <- round(matrix[4] / (matrix[4] + matrix[1]), digits = 2)
  specificity <- round(matrix[2] / (matrix[2] + matrix[3]), digits = 2)
  precision <- round(matrix[4] / (matrix[4] + matrix[3]), digits = 2)
  f1 <- round(2*precision*sensitivity / (precision+sensitivity), digits = 2)
  recall <- sensitivity
  
  performanceTable <- data.frame(Model = model, Sensitivity = sensitivity, Specificity = specificity, 
                                 Precision = precision, Recall = recall, F1 = f1)
  return(performanceTable)
}


# read data
hrdata <- read.csv('HR_comma_sep.csv', header = TRUE)
hrdata <- as.data.frame(hrdata)

# summary of the data
head(hrdata)
summary(hrdata)
str(hrdata)
sum(is.na(hrdata)) # check numbers of missing values

# data preprocessing
# transform the factor variables into numeric data
levels(hrdata$salary)<-c("low","medium","high")
hrdata$salary <- as.numeric(hrdata$salary)
hrdata$left = as.factor(hrdata$left)


# split data into training and testing data
trainIndex <- createDataPartition(hrdata$left, p = 0.7, list = FALSE, times = 1)

trainData <- hrdata[trainIndex,]
testData  <- hrdata[-trainIndex,]

# logistic regression model
model_glm <- glm(left ~., data = trainData, family = 'binomial')
# predict output of testing data
prediction_glm <- predict(model_glm, testData, type = 'response')
prediction_glm <- ifelse(prediction_glm > 0.5,1,0)
cm_glm <- table(Truth = testData$left, Pred = prediction_glm)
table_glm <- getPerformanceTable("Logistic Regression", cm_glm)
# accuracy
print(paste("Ligistic Regression Accuracy: ", round(mean(prediction_glm == testData$left), digits = 2)) )

# decision tree model
model_dt <- rpart(left ~., data = trainData, method="class", minbucket = 25)
prediction_dt <- predict(model_dt, testData, type = "class")
cm_dt <- table(Truth = testData$left, Pred = prediction_dt)
table_dt <- getPerformanceTable("Decision Tree", cm_dt)
print(paste("Decision Tree Accuracy: ", round(mean(prediction_dt == testData$left), digits = 2)) )

# random forest model
model_rf <- randomForest(as.factor(left) ~., data = trainData, nsize = 20, ntree = 200)
prediction_rf <- predict(model_rf, testData)
cm_rf <- table(Truth = testData$left, Pred = prediction_rf)
table_rf <- getPerformanceTable("Random Forest", cm_rf)
print(paste("Random Tree Accuracy: ", round(mean(prediction_rf == testData$left), digits = 2)) )

# svm model / tune the parameter to get better accuracy
model_svm <- svm(left~ ., data = trainData, gamma = 0.25, cost = 10)
prediction_svm <- predict(model_svm, testData)
#prediction_svm <- ifelse(prediction_svm > 0.5,1,0)
cm_svm <- table(Truth = testData$left, Pred = prediction_svm)
table_svm <- getPerformanceTable("SVM", cm_svm)
print(paste("SVM Accuracy: ", round(mean(prediction_svm == testData$left), digits = 2)) )


# logistic regression
predict_glm_ROC <- predict(model_glm, testData, type = "response")
pred_glm <- prediction(predict_glm_ROC, testData$left)
perf_glm <- performance(pred_glm, "tpr", "fpr")

# decision tree
predict_dt_ROC <- predict(model_dt, testData)
pred_dt <- prediction(predict_dt_ROC[,2], testData$left)
perf_dt <- performance(pred_dt, "tpr", "fpr")

# random forest
predict_rf_ROC <- predict(model_rf, testData, type="prob")
pred_rf <- prediction(predict_rf_ROC[,2], testData$left)
perf_rf <- performance(pred_rf, "tpr", "fpr")

# svm
predict_svm_ROC <- predict(model_svm, testData)
pred_svm <- prediction(as.numeric(predict_svm_ROC), testData$left)
perf_svm <- performance(pred_svm, "tpr", "fpr")

# auc
auc_glm <- performance(pred_glm,"auc")
auc_glm <- round(as.numeric(auc_glm@y.values), 2)
auc_dt <- performance(pred_dt,"auc")
auc_dt <- round(as.numeric(auc_dt@y.values), 2)
auc_rf <- performance(pred_rf,"auc")
auc_rf <- round(as.numeric(auc_rf@y.values), 2)
auc_svm <- performance(pred_svm,"auc")
auc_svm <- round(as.numeric(auc_svm@y.values), 2)

# accuracy
auc <- rbind(auc_glm, auc_dt, auc_rf, auc_svm)
accuracy <- rbind(table_glm, table_dt, table_rf, table_svm)
cbind(accuracy, "AUC" = auc)

