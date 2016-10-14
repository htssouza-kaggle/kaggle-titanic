########################################################################################################################
# Kaggle Titanic Challenge
# https://www.kaggle.com/c/titanic
########################################################################################################################

########################################################################################################################
# Dependencies and Libraries
########################################################################################################################

for (.requirement in c("data.table", "rpart", "rattle", "rpart.plot", "RColorBrewer")) {
  if (! .requirement %in% rownames(installed.packages())) {
    install.packages(.requirement, repos="http://cran.rstudio.com/")
  }
}

library(data.table)
library(rpart)
library(rattle)
library(rpart.plot)
library(RColorBrewer)

################################################################################
# Local dependencies
################################################################################

source ("R/common.R")
source ("R/evaluate.R")

########################################################################################################################
# Constants (change may be required for your own environment)
########################################################################################################################

kSubmissionFileName <- "data/output/dtree.csv"

########################################################################################################################
# Seed
########################################################################################################################

set.seed(1994)

########################################################################################################################
# Decision Tree Implementation
########################################################################################################################

validationFactors <- c(.25, .28, .3, .32, .34, .4, .5)
results <- data.table(validationFactor=validationFactors)
results[, result := 0]

for(.validationFactor in validationFactors) {
  # load
  passengerData <- LoadPassengerData(validationFactor = .validationFactor)

  # train
  train <- passengerData$train
  train <- Normalize(train)
  fit <- rpart(GetFormula(train),
               data=train,
               method="class")

  # validation
  validation <- passengerData$validation
  validation <- Normalize(validation)
  validation.result <- predict(fit, validation, type = "class")

  # evaluation
  .result <- Evaluate(validation.result, validation[, survived])
  results[ validationFactor == .validationFactor, result := .result ]

  print(paste0("validationFactor = ", .validationFactor, ", result = ", .result))
}

print(results)
betterResult <- (results[order(-result), result])[1]
betterValidationFactor <- (results[result == betterResult, validationFactor])[1]
print(paste0("betterValidationFactor = ", betterValidationFactor, ", betterResult = ", betterResult))

betterValidationFactor <- .25
passengerData <- LoadPassengerData(validationFactor = betterValidationFactor)
train <- passengerData$train
train <- Normalize(train)
fit <- rpart(GetFormula(train),
             data=train,
             method="class")

plot(fit)
text(fit)
fancyRpartPlot(fit)

# test
test <- passengerData$test
test <- Normalize(test)
test.result <- predict(fit, test, type = "class")
test.submission <- data.frame(PassengerId = test[, passengerid], Survived = test.result)
write.csv(test.submission, file=kSubmissionFileName, row.names=FALSE)
