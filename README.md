# Human-Resources-Analytics
1052 nccu data science course - final project

## Introduction
Human Resources Analytics is an interesting dataset from Kaggle to explore. Our goal is trying to understand why our best and most experienced employees are leaving the company prematurely. We have this database with ten variables and ten thousand observations. Our challege consists in guessing the reasons behind their leaving and to predict which valuable employees will leave next.

### Goal
* Understand the data and its variables
* Perform exploratory analysis by visualizing variables of interest
* Perform predictive analysis based on variables

This project uses R to analyze the dataset, and combined with Shiny App for information visualization.

## Data Information
[Data Source: Human Resources Analytics(From Kaggle)](https://www.kaggle.com/ludobenistant/hr-analytics)

This dataset contains 10 variables and 15K rows. Each row corresponds to an employee.

Below are the descriptions about these variables:

Variable Name | Description
------------ | -------------
satisfaction_leve | Level of satisfaction (0-1)
last_evaluation | Last evaluation
number_project | Number of projects completed while at work
average_montly_hours | Average monthly hours at workplace
time_spend_company | Number of years spent in the company
Work_accident | Whether the employee had a workplace accident
left | Whether the employee left the workplace or not (1 or 0) Factor
promotion_last_5years | Whether the employee was promoted in the last five years
sales(String) | Department in which they work for
salary(String) | Relative level of salary (high)

## Data Analysis
* The Total Number of employee: 14999
* The Number of employee who left the company: 3571
* The Number of employee who didn't left the company: 11428
* The proportion of employee who left: 0.24

```
# read data
hrdata <- read.csv('HR_comma_sep.csv', header = TRUE)

# summary of the data
head(hrdata)
summary(hrdata)
# check numbers of missing values
sum(is.na(hrdata))
```

```
# transform the factor variables into numeric data
levels(hrdata$salary) <- c("low", "medium", "high")
hrdata$salary <- as.numeric(hrdata$salary)
hrdata$left = as.factor(hrdata$left)
```

## Model Prediction
Use four different models to predict results, and compare their performance with multiple evaluation methods.

### Model

```
# split data into training and testing data
trainIndex <- createDataPartition(hrdata$left, p = 0.7, list = FALSE, times = 1)
trainData <- hrdata[trainIndex,]
testData  <- hrdata[-trainIndex,]
```

* Logistic Regression
```
model_glm <- glm(left ~., data = trainData, family = 'binomial')
# predict output of testing data
prediction_glm <- predict(model_glm, testData, type = 'response')
prediction_glm <- ifelse(prediction_glm > 0.5,1,0)
# get confusion matrix
cm_glm <- table(Truth = testData$left, Pred = prediction_glm)
table_glm <- getPerformanceTable("Logistic Regression", cm_glm)
# accuracy
print(paste("Ligistic Regression Accuracy: ", round(mean(prediction_glm == testData$left), digits = 2)))
```

* Decision Tree
* Random Forest
* Support Vector Machine (SVM)

### Evaluation Performance
Model | Sensitivity	| Specificity	| Precision	| Recall	| F1	| AUC
------------ | ------------- | ------------- | ------------- | ------------- | ------------- | -------------
Logistic Regression	| 0.10	| 0.74	| 0.61	| 0.10	| 0.17	| 0.82
Decision Tree	| 0.23	| 0.58	| 0.94	| 0.23	| 0.37	| 0.97
Random Forest	| 0.23	| 0.77	| 0.99	| 0.23	| 0.37	| 0.99
SVM	| 0.23	| 0.46	| 0.93	| 0.23	| 0.37	| 0.96

## Data Visualization
Use Plotly and ggplot packages in R for data visualization, and present the graphs in shiny app.


## Shiny App
[Human Resources Analytics Shiny App](https://tammykanshiny.shinyapps.io/human_resources_analytics/)

app.R is the code for this shiny app.
