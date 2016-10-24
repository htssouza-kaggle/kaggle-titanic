################################################################################
# Kaggle Titanic Challenge
# https://www.kaggle.com/c/titanic
################################################################################

################################################################################
# Dependencies and Libraries
################################################################################

for (.requirement in c("data.table", "magrittr", "xgboost")) {
  if (! .requirement %in% rownames(installed.packages())) {
    install.packages(.requirement, repos="http://cran.rstudio.com/")
  }
}

library(data.table)
library(magrittr)
library(xgboost)

################################################################################
# Local dependencies
################################################################################

source ("R/common.R")

################################################################################
# Constants (change may be required for your own environment)
################################################################################

kSubmissionFileName <- "data/output/xgb.csv"

################################################################################
# Seed
################################################################################

set.seed(1994)

################################################################################
# XGB Specific Methods
################################################################################

BuildParamOutputsTable <- function() {
  validationFactors <- c(.25, .28, .3, .35)
  nrounds <- c(25, 50, 80, 100, 120, 200, 300, 500, 1000)
  outputs <- CJ(validationFactors, nrounds)
  setnames(outputs, "V1", "validationFactor")
  setnames(outputs, "V2", "nround")
  outputs[, score := 0]
  return (outputs)
}

PrintParams <- function (params=params) {
  print("params:")
  print(params)
}

PrintOutputs <- function (outputs=outputs) {
  print("outputs:")
  print(outputs)
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

BuildXGBData <- function(x, ...) UseMethod("BuildXGBData")

BuildXGBData.data.table <- function(x, withLabel=FALSE) {

  # column filter and sort (uniform)
  cols <- names(x)[! names(x) %in% c("passengerid", "survived") ]
  cols <- unique(sort(cols))
  data <- x[, .SD, , .SDcols = cols]

  for(colIndex in 1:length(names(data))) {
    col <- data[, get(names(data)[colIndex])]
    colClass <- class(col)
    dirty <- FALSE
    if(colClass == "integer") {
      col <- as.numeric(col)
      dirty <- TRUE
    } else if(colClass == "factor") {
      col <- as.numeric(col)
      dirty <- TRUE
    } else if(colClass == "character") {
      col <- as.numeric(as.factor(col))
      dirty <- TRUE
    }
    if (dirty) {
      data[, eval(names(data)[colIndex]) := col]
    }
  }

  if (withLabel) {
    return (xgb.DMatrix (as.matrix(data), label=as.numeric(as.character(x[, survived])), missing=NaN))
  } else {
    return (xgb.DMatrix (as.matrix(data), missing=NaN))
  }
}

GetXGBPrediction <- function(x) {
  return (ifelse(x > 0.5, 1, 0))
}

TrainTransform <- function(input=NULL, params=NULL) {
  train <- input$train
  train <- Normalize(train, bypassFactorization=TRUE)

  fit <- xgboost(BuildXGBData(train, withLabel=TRUE),
                 objective="binary:logistic",
                 nround=params$nround)

  output <- input
  output$train <- train
  output$fit <- fit

  return (output)
}

ValidateTransform <- function(input=NULL, params=NULL) {
  validation <- input$validation
  validation <- Normalize(validation, bypassFactorization=TRUE)

  validation.result <- GetXGBPrediction(predict(input$fit, BuildXGBData(validation)))

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
  test <- Normalize(test, bypassFactorization=TRUE)

  test.result <- GetXGBPrediction(predict(input$fit, BuildXGBData(test)))
  test.submission <- data.table(PassengerId=test[, passengerid], Survived=test.result)
  test.submission[ is.na(Survived), Survived := as.factor(0)]
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

PrintOutputs(output)
params <- GetBestOutputParams(outputs)
PrintParams(params)
output <- LoadTransform(params=params) %>%
  TrainTransform(input=., params=params) %>%
  ValidateTransform(input=., params=params) %>%
  TestTransform(input=., params=params)
