## app.R ##
# 1052 nccu data science final project
# Human Resources Analytics

# load library
library(shiny)
library(shinydashboard)
library(plotly)
library(ggplot2)

library(ROCR)
library(randomForest)
library(rpart)
library(rpart.plot)
library(party)
#library(plotROC)

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

left = nrow(hrdata[hrdata$left == 1,])
no_left = nrow(hrdata[hrdata$left == 0,])

# data preprocessing
sapply(hrdata,class)
sales <- unique(hrdata$sales)

# convert satisfication, sales and salary to factor
hrdata$salesN <- as.numeric(1:10)[match(hrdata$sales, sales)] 
hrdata$salaryN <- as.numeric(1:3)[match(hrdata$salary, c('low', 'medium', 'high'))]
hrdata$satisfaction <- as.factor(hrdata$satisfaction)

# one more new variable for 'left' for string representation
hrdata$leftString[hrdata$left ==  1] = 'Left'
hrdata$leftString[hrdata$left ==  0] = 'Not Left'


## ui  
ui <- dashboardPage(
  skin = "green", 
  # Header content
  dashboardHeader(title = "Human Resources Analytics", titleWidth = 300),
  # Sidebar content
  dashboardSidebar(
    sidebarMenu(
      menuItem("About", tabName = "about", icon = icon("user")),
      menuItem("Data Information", tabName = "info", icon = icon("info-circle")),
      
      menuItem("Data Analysis", tabName = "plot", icon = icon("bar-chart"), startExpanded = TRUE, 
               menuSubItem("Data Analysis(1)", tabName = "plot1"), menuSubItem("Data Analysis(2)", tabName = "plot2"), menuSubItem("Data Analysis(3)", tabName = "plot3")),
      
      menuItem("Model Prediction", tabName = "model", icon = icon("check")),
      menuItem("Source Code", href = "https://github.com/tammykan/Human-Resources-Analytics", icon = icon("code"))
    )
  ),
  
  # Body content
  dashboardBody(
    # Load D3.js
    tags$head(tags$script(src = 'http://d3js.org/d3.v3.min.js')),
    
    tabItems(
      # about tab content
      tabItem(tabName = "about",
              h2("Human Resources Analytics"),
              p("1052 NCCU Data Science Course - Final Project"),
              img(src="hr.jpg", width = "100%", height = "50%"),
              br(),
              h3("Introduction"),
              h4("Human Resources Analytics is an interesting dataset to explore. 
                 Our goal is trying to understand why our best and most experienced employees are leaving the company prematurely. 
                 We have this database with ten variables and ten thousand observations. 
                 Our challege consists in guessing the reasons behind their leaving and to predict which valuable employees will leave next."),
              
              helpText(a("Data Source: https://www.kaggle.com/ludobenistant/hr-analytics" , href = "https://www.kaggle.com/ludobenistant/hr-analytics"))
      ),
      
      # information tab content
      tabItem(tabName = "info",
              # information
              fluidRow(
                h2("Data"),
                box(
                  title = "Variables Information", status = "success", width = 12, 
                  solidHeader = TRUE, collapsible = TRUE, 
                  tableOutput("summary")
                ),
                box(
                  title = "Employee Information", status = "success", width = 12, 
                  solidHeader = TRUE, collapsible = TRUE, 
                  h5(paste("The Total Number of employee: ", nrow(hrdata))),
                  h5(paste("The Number of employee who left the company: ", left)),
                  h5(paste("The Number of employee who didn't left the company: ", no_left)),
                  h5(paste("The proportion of employee who left: ", round((left/nrow(hrdata)), digits = 2)) )
                ),
                box(
                  title = "Correlation Plot", status = "success", width = 12, 
                  solidHeader = TRUE, collapsible = TRUE, 
                  plotOutput("corrplot")
                )
              )
      ),
      
      # plot tab content
      tabItem(tabName = "plot1", h2("Total Employee"),
              # plot
              fluidRow(
                column(width = 4, plotlyOutput("plot1"), plotlyOutput("plot2"), plotlyOutput("plot3")),
                column(width = 4, plotlyOutput("plot4"), plotlyOutput("plot5"), plotlyOutput("plot6")),
                column(width = 4, plotlyOutput("plot7"), plotlyOutput("plot8"), plotlyOutput("plot9"))
                
              )
      ),
      
      tabItem(tabName = "plot2", h2("Left Employee"),
              # plot
              fluidRow(
                column(width = 4, plotlyOutput("plot10"), plotlyOutput("plot11"), plotlyOutput("plot12")),
                column(width = 4, plotlyOutput("plot13"), plotlyOutput("plot14"), plotlyOutput("plot15")),
                column(width = 4, plotlyOutput("plot16"), plotlyOutput("plot17"), plotlyOutput("plot18"))
              )
      ),
      
      tabItem(tabName = "plot3", h2("Left Employee and Not Left Employee"),
              # plot
              fluidRow(
                #column(width = 4, plotlyOutput("plot19"), plotlyOutput("plot20"), plotlyOutput("plot21")),
                #column(width = 4, plotlyOutput("plot22"), plotlyOutput("plot23"), plotlyOutput("plot24")),
                #column(width = 4, plotlyOutput("plot25"), plotlyOutput("plot26"), plotlyOutput("plot27"))
                column(width = 6, plotOutput("com1"), plotOutput("com2"), plotOutput("com3")),
                column(width = 6, plotOutput("com4"), plotOutput("com5"), plotOutput("com6"))
                
              )
      ),
      
      # model tab content
      tabItem(tabName = "model",
              fluidRow(
                box(
                  width = 12, title = "Decision Tree Model", 
                  plotOutput("model_dt")
                ),
                
                tabBox(
                  title = "Model Prediction", width = 12, side = "right",  
                  # The id lets us use input$tabset1 on the server to find the current tab
                  id = "tabset", height = "250px",
                  tabPanel("Logistic Regression", tableOutput("prediction_glm"), textOutput("mean_glm")),
                  tabPanel("Decision Tree", tableOutput("prediction_dt"), textOutput("mean_dt")),
                  tabPanel("Random Forest", tableOutput("prediction_rf"), textOutput("mean_rf")),
                  tabPanel("SVM", tableOutput("prediction_svm"), textOutput("mean_svm"))
                ),
                
                box(
                  title = "Model Performance", status = "primary", width = 12,
                  tableOutput("accuracy")
                ),
                
                box(
                  title = "ROC Curve", status = "primary", width = 12,
                  plotOutput("roc_curve")
                )
              )
      )
    )
  )
)

## server
server <- function(input, output, session) {
  
  # table information
  output$summary <- renderTable({
    str1 = "Highest being 1 and lowest is 0.09. And there are 10187 employees with satisfaction above 0.5."
    str2 = "There are employees who are assigned upto 7 project and as least as 2 projects."
    str3 = "On an average employees spend 200 hours/month in office."
    str4 = "The company has employees whose stay varied from 2 to 10 years."
    str5 = "This is the variable of interest. Out of 15K in dataset around 11K employees has not left the company."
    str6 = "Only around 300 employees are promoted in last 5 years."
    str7 = "There are around 6 major departments. And maximum(6K) belongs to sales department."
    str8 = "Classified into high/medium/low. And maximum employees belong to low(7K)."
    str9 = "Whether the employee had a workplace accident."
    str10 = "Last evaluation."
    
    name = c("satisfaction_level","number_project", "average_montly_hours", 
             "time_spend_company", "left", "promotion_last_5years", 
             "slaes", "salary", "Work_accident", "last_evaluation")
    description = c(str1, str2, str3, str4, str5, str6, str7, str8, str9, str10)
    table <- cbind("Variable Name"=name, "Description"=description)
  })
  
  # plot number of employee
  output$plot1 <- renderPlotly({
    ggplot(data = hrdata, aes(x = number_project)) + geom_bar(stat= "count") + ggtitle("Number of Project")
  })
  output$plot2 <- renderPlotly({
    ggplot(data = hrdata, aes(x = average_montly_hours)) + geom_bar(stat= "count") + ggtitle("Average Montly Hours")
  })
  output$plot3 <- renderPlotly({
    ggplot(data = hrdata, aes(x = time_spend_company)) + geom_bar(stat= "count") + ggtitle("Time Spend Company")
  })
  output$plot4 <- renderPlotly({
    ggplot(data = hrdata, aes(x = Work_accident)) + geom_bar(stat= "count") + ggtitle("Work Accident")
  })
  output$plot5 <- renderPlotly({
    ggplot(data = hrdata, aes(x = satisfaction_level)) + geom_bar(stat= "count") + ggtitle("Satisfaction Level")
  })
  output$plot6 <- renderPlotly({
    ggplot(data = hrdata, aes(x = last_evaluation)) + geom_bar(stat= "count") + ggtitle("Last Evaluation")
  })
  output$plot7 <- renderPlotly({
    ggplot(data = hrdata, aes(x = promotion_last_5years)) + geom_bar(stat= "count") + ggtitle("Promotion Last 5 years")
  })
  output$plot8 <- renderPlotly({
    ggplot(data = hrdata, aes(x = sales)) + geom_bar(stat= "count") + theme(axis.text.x=element_text(angle=60, hjust=1)) + ggtitle("Sales")
  })
  output$plot9 <- renderPlotly({
    ggplot(data = hrdata, aes(x = salary)) + geom_bar(stat= "count") + ggtitle("Salary")
  })
  
  # plot number of left employee
  output$plot10 <- renderPlotly({
    ggplot(data = subset(hrdata, hrdata$left == 1), aes(x = number_project)) + geom_bar(stat= "count") + ggtitle("Number of Project")
  })
  output$plot11 <- renderPlotly({
    ggplot(data = subset(hrdata, hrdata$left == 1), aes(x = average_montly_hours)) + geom_bar(stat= "count") + ggtitle("Average Montly Hours")
  })
  output$plot12 <- renderPlotly({
    ggplot(data = subset(hrdata, hrdata$left == 1), aes(x = time_spend_company)) + geom_bar(stat= "count") + ggtitle("Time Spend Company")
  })
  output$plot13 <- renderPlotly({
    ggplot(data = subset(hrdata, hrdata$left == 1), aes(x = Work_accident)) + geom_bar(stat= "count") + ggtitle("Work Accident")
  })
  output$plot14 <- renderPlotly({
    ggplot(data = subset(hrdata, hrdata$left == 1), aes(x = satisfaction_level)) + geom_bar(stat= "count") + ggtitle("Satisfaction Level")
  })
  output$plot15 <- renderPlotly({
    ggplot(data = subset(hrdata, hrdata$left == 1), aes(x = last_evaluation)) + geom_bar(stat= "count") + ggtitle("Last Evaluation")
  })
  output$plot16 <- renderPlotly({
    ggplot(data = subset(hrdata, hrdata$left == 1), aes(x = promotion_last_5years)) + geom_bar(stat= "count") + ggtitle("Promotion Last 5 years")
  })
  output$plot17 <- renderPlotly({
    ggplot(data = subset(hrdata, hrdata$left == 1), aes(x = sales)) + geom_bar(stat= "count") + theme(axis.text.x=element_text(angle=60, hjust=1)) + ggtitle("Sales")
  })
  output$plot18 <- renderPlotly({
    ggplot(data = subset(hrdata, hrdata$left == 1), aes(x = salary)) + geom_bar(stat= "count") + ggtitle("Salary")
  })
  
  # plot comparison of left and no left employee
  output$plot19 <- renderPlotly({
    ggplot(data = subset(hrdata, hrdata$left == 0), aes(x = number_project)) + geom_bar(stat= "count") + ggtitle("Number of Project")
  })
  output$plot20 <- renderPlotly({
    ggplot(data = subset(hrdata, hrdata$left == 0), aes(x = average_montly_hours)) + geom_bar(stat= "count") + ggtitle("Average Montly Hours")
  })
  output$plot21 <- renderPlotly({
    ggplot(data = subset(hrdata, hrdata$left == 0), aes(x = time_spend_company)) + geom_bar(stat= "count") + ggtitle("Time Spend Company")
  })
  output$plot22 <- renderPlotly({
    ggplot(data = subset(hrdata, hrdata$left == 0), aes(x = Work_accident)) + geom_bar(stat= "count") + ggtitle("Work Accident")
  })
  output$plot23 <- renderPlotly({
    ggplot(data = subset(hrdata, hrdata$left == 0), aes(x = satisfaction_level)) + geom_bar(stat= "count") + ggtitle("Satisfaction Level")
  })
  output$plot24 <- renderPlotly({
    ggplot(data = subset(hrdata, hrdata$left == 0), aes(x = last_evaluation)) + geom_bar(stat= "count") + ggtitle("Last Evaluation")
  })
  output$plot25 <- renderPlotly({
    ggplot(data = subset(hrdata, hrdata$left == 0), aes(x = promotion_last_5years)) + geom_bar(stat= "count") + ggtitle("Promotion Last 5 years")
  })
  output$plot26 <- renderPlotly({
    ggplot(data = subset(hrdata, hrdata$left == 0), aes(x = sales)) + geom_bar(stat= "count") + theme(axis.text.x=element_text(angle=60, hjust=1)) + ggtitle("Sales")
  })
  output$plot27 <- renderPlotly({
    ggplot(data = subset(hrdata, hrdata$left == 0), aes(x = salary)) + geom_bar(stat= "count") + ggtitle("Salary")
  })
  
  
  # left and not left employees
  output$com1 <- renderPlot({
    qplot(satisfaction_level, data = hrdata, fill = left, binwidth = 0.01)
  })
  
  output$com2 <- renderPlot({
    levels(hrdata$salary)<-c("low","medium","high")
    hrdata$salary<-as.numeric(hrdata$salary)
    qplot(salary, data = hrdata, fill = left, binwidth = 0.5)
  })
  
  output$com3 <- renderPlot({
    qplot(number_project, data = hrdata, fill = left, binwidth = 1)
  })
  
  output$com4 <- renderPlot({
    qplot(average_montly_hours, data = hrdata, fill = left, binwidth = 5)
  })
  
  output$com5 <- renderPlot({
    qplot(time_spend_company, data = hrdata, fill = left, binwidth = 1)
  })
  
  output$com6 <- renderPlot({
    qplot(promotion_last_5years, data = hrdata, fill = left, binwidth = 0.5)
  })

  # correlation plot  
  output$corrplot <- renderPlot({
    data <- read.csv("HR_comma_sep.csv")
    cor(data[,1:8])
    corrplot(cor(data[,1:8]), method = "circle")
  })
  
  # read data
  hrdata = read.csv("HR_comma_sep.csv", header = TRUE)
  hrdata$left = as.factor(hrdata$left)
  
  # split data into training and testing data
  trainIndex <- createDataPartition(hrdata$left, p = 0.7,
                                    list = FALSE, times = 1)
  #test.index <- sample(x=1:nrow(data), size=ceiling(0.8*nrow(data) ))
  trainData <- hrdata[trainIndex,]
  testData  <- hrdata[-trainIndex,]
  
  # logistic regression model
  model_glm <- glm(left ~., data = trainData, family = 'binomial')
  # predict output of testing data
  prediction_glm <- predict(model_glm, testData, type = 'response')
  prediction_glm <- ifelse(prediction_glm > 0.5,1,0)
  cm_glm <- table(Truth = testData$left, Pred = prediction_glm)
  table_glm <- getPerformanceTable("Logistic Regression", cm_glm)
  
  # confusion matrix
  output$prediction_glm <- renderTable({ cm_glm })
  # accuracy
  output$mean_glm <- renderText({ paste("Accuracy: ", round(mean(prediction_glm == testData$left), digits = 2)) })
  
  # decision tree model
  model_dt <- rpart(left ~., data = trainData, method="class", minbucket = 25)
  prediction_dt <- predict(model_dt, testData, type = "class")
  cm_dt <- table(Truth = testData$left, Pred = prediction_dt)
  table_dt <- getPerformanceTable("Decision Tree", cm_dt)
  
  # decision tree plot
  output$model_dt <- renderPlot({
    rpart.plot(model_dt)
  })
  output$prediction_dt <- renderTable({ cm_dt })
  output$mean_dt <- renderText({ paste("Accuracy: ", round(mean(prediction_dt == testData$left), digits = 2)) })
  
  
  # random forest model
  model_rf <- randomForest(as.factor(left) ~., data = trainData, nsize = 20, ntree = 200)
  prediction_rf <- predict(model_rf, testData)
  cm_rf <- table(Truth = testData$left, Pred = prediction_rf)
  table_rf <- getPerformanceTable("Random Forest", cm_rf)
  
  # random forest model plot
  output$model_rf <- renderPlot({
    rf <- ctree(left ~., data = hrdata, controls=cforest_control(mtry=2, mincriterion=0))
    plot(rf, type = "simple")
  })
  
  output$prediction_rf <- renderTable({ cm_rf })
  output$mean_rf <- renderText({ paste("Accuracy: ", round(mean(prediction_rf == testData$left), digits = 2)) })
  
  # svm model / tune the parameter to get better accuracy
  model_svm <- svm(left~ ., data = trainData, gamma = 0.25, cost = 10)
  prediction_svm <- predict(model_svm, testData)
  #prediction_svm <- ifelse(prediction_svm > 0.5,1,0)
  cm_svm <- table(Truth = testData$left, Pred = prediction_svm)
  table_svm <- getPerformanceTable("SVM", cm_svm)
  
  output$prediction_svm <- renderTable({ cm_svm })
  output$mean_svm <- renderText({ paste("Accuracy: ", round(mean(prediction_svm == testData$left), digits = 2)) })
  
  
  # null model
  
  
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
  output$accuracy <- renderTable({
    auc <- rbind(auc_glm, auc_dt, auc_rf, auc_svm)
    accuracy <- rbind(table_glm, table_dt, table_rf, table_svm)
    cbind(accuracy, "AUC"=auc)
  })
  
  # roc curve
  output$roc_curve <- renderPlot({
    plot(perf_glm, main = "ROC Curve for the Models", col='blue')
    plot(perf_dt,add=TRUE, col='red')
    plot(perf_rf, add=TRUE, col='green3')
    plot(perf_svm, add=TRUE, col='darkmagenta')
    legend('bottom', c("Logistic Regression", "Decision Tree", 
                       "Random Forest", "Support Vector Machine"), 
           fill = c('blue','red','green3','darkmagenta'), bty='n')
  })
  
  
}

shinyApp(ui, server)
