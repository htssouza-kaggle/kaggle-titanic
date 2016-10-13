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
source ("R/mcc.R")

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

validationFactors <- c(.1, .2, .3, .4, .5)
ntrees <- c(50, 80, 100, 300, 500, 1000, 2000, 3000, 5000)
mccs <- CJ(validationFactors, ntrees)
setnames(mccs, "V1", "validationFactor")
setnames(mccs, "V2", "ntree")
mccs[, mcc := 0]

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
    mcc.result <- mccEval(validation.result, validation[, survived])
    mccs[ validationFactor == .validationFactor & ntree == .ntree, mcc := mcc.result  ]

    print(paste0("validationFactor = ", .validationFactor,  ", trees = ", .ntree, ", mcc = ", mcc.result))
  }
}

print(mccs)
betterMcc <- (mccs[order(-mcc), mcc])[1]
betterNtree <- mccs[mcc == betterMcc, ntree]
betterValidationFactor <- mccs[mcc == betterMcc, validationFactor]
print(paste0("betterValidationFactor = ", betterValidationFactor,  ", betterNtree = ", betterNtree))

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
write.csv(test.submission, file=kSubmissionFileName, row.names=FALSE)
