################################################################################
# Kaggle Titanic Challenge
# https://www.kaggle.com/c/titanic
################################################################################

################################################################################
# Dependencies and Libraries
################################################################################

for (.requirement in c("data.table", "magrittr", "rpart", "rattle",
                       "rpart.plot", "RColorBrewer")) {
  if (! .requirement %in% rownames(installed.packages())) {
    install.packages(.requirement, repos="http://cran.rstudio.com/")
  }
}

library(data.table)
library(magrittr)
library(rpart)
library(rattle)
library(rpart.plot)
library(RColorBrewer)

################################################################################
# Local dependencies
################################################################################

source ("R/common.R")

################################################################################
# Constants (change may be required for your own environment)
################################################################################

kSubmissionFileName <- "data/output/dtree.csv"

################################################################################
# Seed
################################################################################

set.seed(1994)

################################################################################
# Decision Tree Specific Methods
################################################################################

BuildParamOutputsTable <- function() {
  validationFactors <- c(.25, .28, .3, .32, .34, .4, .5)
  outputs <- data.table(validationFactor=validationFactors)
  outputs[, score := 0]
  return (outputs)
}

PrintParams <- function (params=params) {
  print("params:")
  print(params)
}

PrintParamsOutput <- function (params=params, output=output) {
  print("params:")
  print(params)
  print("output.score:")
  print(output$score)
}

SaveOutput <- function(outputs, outputIndex, output) {
  outputs[outputIndex, score := output$score]
}

GetBestOutputParams <- function (results) {
  sortedOutputs <- results[order(-score)]
  return (sortedOutputs[1])
}

LoadTransform <- function(input=NULL, params=NULL) {
  output <- LoadPassengerData(validationFactor = params$validationFactor)
  return (output)
}

TrainTransform <- function(input=NULL, params=NULL) {
  train <- input$train
  train <- Normalize(train)
  fit <- rpart(GetFormula(train),
               data=train,
               method="class")

  output <- input
  output$train <- train
  output$fit <- fit

  return (output)
}

ValidateTransform <- function(input=NULL, params=NULL) {
  validation <- input$validation
  validation <- Normalize(validation)
  validation.result <- predict(input$fit, validation, type="class")

  output <- input
  output$validation <- validation
  output$validation.result <- validation.result

  return (output)
}

EvaluateTransform <- function(input=NULL, params=NULL) {
  score <- Evaluate(input$validation.result, input$validation[, survived])
  output <- input
  output$score <- score
  return (output)
}

TestTransform <- function(input=NULL, params=NULL) {
  test <- input$test
  test <- Normalize(test)
  test.result <- predict(input$fit, test, type="class")
  test.submission <- data.frame(PassengerId=test[, passengerid], Survived=test.result)
  write.csv(test.submission, file=kSubmissionFileName, row.names=FALSE)

  output <- input
  output$test.result <- test.result
  output$test.submission <- test.submission

  return (output)
}

################################################################################
# Main Flow
################################################################################

outputs <- BuildParamOutputsTable()

for(outputIndex in 1:nrow(outputs)) {
  params <- outputs[outputIndex]

  output <- LoadTransform(params=params) %>%
    TrainTransform(input=., params=params) %>%
    ValidateTransform(input=., params=params) %>%
    EvaluateTransform(input=., params=params)

  PrintParamsOutput(params=params, output=output)
  SaveOutput(outputs, outputIndex, output)
}

params <- GetBestOutputParams(outputs)
PrintParams(params)
output <- LoadTransform(params=params) %>%
  TrainTransform(input=., params=params) %>%
  ValidateTransform(input=., params=params) %>%
  TestTransform(input=., params=params)
