########################################################################################################################
# Kaggle Titanic Challenge
# https://www.kaggle.com/c/titanic
########################################################################################################################

########################################################################################################################
# Dependencies and Libraries
########################################################################################################################

for (.requirement in c("data.table", "stringi")) {
  if (! .requirement %in% rownames(installed.packages())) {
    install.packages(.requirement)
  }
}

library(data.table)
library(stringi)

########################################################################################################################
# Util Functions
########################################################################################################################

# if source col has word, set targetCol with 1, otherwise 0
extractWordAsFlag <- function(x, word, sourceCol, targetCol=NA) {
  if (is.na(targetCol)) targetCol <- word

  pattern <- word
  x[, eval(targetCol) := 0]
  x[grep(pattern, x[, get(sourceCol)], ignore.case=TRUE), eval(targetCol) := 1 ]
  x[, eval(sourceCol) := gsub(pattern, "", get(sourceCol), ignore.case=TRUE) ]
}

# replace complete words in similarWords list with the first word in this list
normalize <- function(x, col, similarWords) {
  baseWord <- similarWords[1]
  for (alternative_word in similarWords[2:length(similarWords)]) {
    pattern <- paste0("\\<", alternative_word, "\\>")
    x[, eval(col) :=  gsub(pattern, baseWord, get(col), ignore.case=TRUE) ]
  }
}

# replace wordS with word
normalizeSingular <- function(x, col, word) {
  normalize (x, col, c(word, paste0(word, "s")))
}

########################################################################################################################
# Constants (change may be required for your own environment)
########################################################################################################################

kTrainFileName <- "data/input/train.csv"
kTestFileName <- "data/input/test.csv"

########################################################################################################################
# Problem Specific Function
########################################################################################################################

LoadPassengerData <- function (sampleFactor = 1.0, validationFactor = 0.333) {

  if (sampleFactor > 1) stop("sampleFactor cannot be > 1")

  train <- fread(kTrainFileName)
  test <- fread(kTestFileName)

  # sampling
  sampleSize <- as.integer(nrow(train) * sampleFactor)
  train <- train[][][sample(.N, sampleSize)]

  # split train and validation
  validationSize <- as.integer(nrow(train) * validationFactor)
  trainSize <- nrow(train) - validationSize
  validation <- train[(trainSize+1):nrow(train)]
  train <- train[1:trainSize]

  output <- list (train=train,
                  validation=validation,
                  test=test)

  return (output)
}

# get formula to be used on fit
GetFormula <- function(x) {
  featureNames <- names(x)
  featureNames <- featureNames[! featureNames %in% c("passengerid", "survived") ]
  formulaText <- paste0("survived ~ ", paste0(featureNames, collapse=" + "))

  return (as.formula(formulaText))
}

# normalize column names, person name, title and cabin
NormalizePassenger <- function(x) {

  # normalize names
  names(x) <- tolower(names(x))
  x <- data.table(x)

  # tolower
  x[, name := tolower(name) ]

  # split last and first name
  nameTokens <- strsplit(x[, name], ", ")
  x[, firstname := sapply(nameTokens, FUN=function(a) { stri_trim(a[2]) }) ]
  x[, lastname  := sapply(nameTokens, FUN=function(a) { stri_trim(a[1]) }) ]
  x[, name := NULL ]

  # extract title from first name as flags
  # convert words to flags
  titles <- c("capt", "col", "don", "dr", "major", "master", "miss", "mlle", "mr", "mrs", "rev")
  for (title in titles) {
    extractWordAsFlag(x, paste0(title, "."), "firstname", paste0("is", title))
  }
  x[, firstname := stri_trim(firstname) ]

  # extract letter from cabin
  x[, cabin := tolower(cabin) ]
  cabinLetters <- c("a", "b", "c", "d", "e", "f", "g", "t")
  for (cabinLetter in cabinLetters) {
    extractWordAsFlag(x, cabinLetter, "cabin", paste0("cabin", cabinLetter))
  }
  x[, cabin := stri_trim(cabin) ]

  return (data.table(x))
}

Normalize <- function (x) {

  # basics
  x <- NormalizePassenger(x)

  # complete age
  meanAge <- mean(x[! is.na(age), age])
  x[is.na(age), age := meanAge]

  # embarked
  x[embarked == "", embarked := "S"]

  # factors
  x[, embarked := as.factor(embarked) ]
  x[, sex := as.factor(sex) ]
  if ("survived" %in% names(x)) x[, survived := as.factor(survived) ]

  # unused columns
  x[, cabin := NULL]
  x[, firstname := NULL]
  x[, lastname := NULL]
  x[, ticket := NULL]

  return (data.table(x))
}
