########################################################################################################################
# Kaggle Titanic Challenge
# https://www.kaggle.com/c/titanic
########################################################################################################################

########################################################################################################################
# Dependencies and Libraries
########################################################################################################################

for (.requirement in c("data.table", "randomForest")) {
  if (! .requirement %in% rownames(installed.packages())) {
    install.packages(.requirement, repos="http://cran.rstudio.com/")
  }
}

library(data.table)
library(randomForest)

################################################################################
# Local dependencies
################################################################################

source ("R/common.R")
source ("R/evaluate.R")

########################################################################################################################
# Constants (change may be required for your own environment)
########################################################################################################################

kSubmissionFileName <- "data/output/rforest.csv"

########################################################################################################################
# Seed
########################################################################################################################

set.seed(1994)

########################################################################################################################
# Decision Tree Implementation
########################################################################################################################

validationFactors <- c(.25, .28, .3, .32, .34, .4, .5)
ntrees <- c(50, 80, 90, 100, 110, 120, 150, 180, 300, 500, 1000, 2000, 3000, 5000)
results <- CJ(validationFactors, ntrees)
setnames(results, "V1", "validationFactor")
setnames(results, "V2", "ntree")
results[, result := 0]

for(.validationFactor in validationFactors) {

  passengerData <- LoadPassengerData(validationFactor = .validationFactor)

  for(.ntree in ntrees) {

    # train
    train <- passengerData$train
    train <- Normalize(train)
    fit <- randomForest(GetFormula(train),
                        data=train,
                        method="class",
                        ntree=.ntree)

    # validation
    validation <- passengerData$validation
    validation <- Normalize(validation)
    validation.result <- predict(fit, validation, type = "class")

    # evaluation
    .result <- evaluate(validation.result, validation[, survived])
    results[ validationFactor == .validationFactor & ntree == .ntree, result := .result ]

    print(paste0("validationFactor = ", .validationFactor,  ", trees = ", .ntree, ", result = ", .result))
  }
}

print(results)
betterResult <- (results[order(-result), result])[1]
betterNtree <- (results[result == betterResult, ntree])[1]
betterValidationFactor <- (results[result == betterResult, validationFactor])[1]
print(paste0("betterValidationFactor = ", betterValidationFactor,  ", betterNtree = ", betterNtree, ", betterResult = ", betterResult))

passengerData <- LoadPassengerData(validationFactor = betterValidationFactor)
train <- passengerData$train
train <- Normalize(train)
fit <- randomForest(GetFormula(train),
                    data=train,
                    method="class",
                    ntree=betterNtree)

# test
test <- passengerData$test
test <- Normalize(test)
test.result <- predict(fit, test, type = "class")
test.submission <- data.table(PassengerId = test[, passengerid], Survived = test.result)
test.submission[ is.na(Survived), Survived := as.factor(0)]
write.csv(test.submission, file=kSubmissionFileName, row.names=FALSE)
