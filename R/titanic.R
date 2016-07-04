########################################################################################################################
# Kaggle Titanic Challenge
# https://www.kaggle.com/c/titanic
########################################################################################################################

########################################################################################################################
# Constants (change may be required for your own environment)
########################################################################################################################

TRAIN_FILE_NAME = 'train.csv'
TEST_FILE_NAME = 'test.csv'

########################################################################################################################
# Dependencies and Libraries
########################################################################################################################

requirements <- c('rpart', 'rattle', 'rpart.plot', 'RColorBrewer')
for (requirement in requirements) {
    if (! requirement %in% rownames(installed.packages())) {
        install.packages(requirement)
    }
}

library(rpart)
library(rattle)
library(rpart.plot)
library(RColorBrewer)

########################################################################################################################
# Shared Functions

trim <- function (x) {
    gsub("^\\s+|\\s+$", "", x)
}

extract_dimension <- function(x, unit, source_col, target_col=NA) {
    if (is.na(target_col)) target_col <- unit

    norm_pattern <- paste('\\<([0-9]*) (', unit, ')\\>', sep='')
    x[, source_col] <- gsub(norm_pattern, '\\1\\2', x[, source_col], ignore.case=TRUE)

    extract_pattern <- paste('.*\\<([0-9]*', unit, ')\\>.*', sep='')
    remove_pattern <- paste('\\<([0-9]*', unit, ')\\>', sep='')
    x[, target_col] <- gsub(extract_pattern, '\\1', x[, source_col], ignore.case=TRUE)
    x[, target_col] <- gsub(unit, '', x[, target_col], ignore.case=TRUE)
    x[, target_col] <- suppressWarnings(as.integer(x[, target_col]))

    x[grep(remove_pattern, x[, source_col], invert=TRUE, ignore.case=TRUE), target_col] <- NA
    x[, source_col] <- gsub(remove_pattern, '', x[, source_col], ignore.case=TRUE)
    x
}

extract_word_as_flag <- function(x, word, source_col, target_col=NA) {
    if (is.na(target_col)) target_col <- word

    pattern <- word
    x[, target_col] <- 0
    x[grep(pattern, x[, source_col], ignore.case=TRUE), target_col] <- 1
    x[, source_col] <- gsub(pattern, '', x[, source_col], ignore.case=TRUE)
    x
}

normalize <- function(x, col, similar_words) {
    base_word <- similar_words[1]
    for (alternative_word in similar_words[2:length(similar_words)]) {
        pattern <- paste('\\<', alternative_word, '\\>', sep='')
        x[, col] <- gsub(pattern, base_word, x[, col], ignore.case=TRUE)
    }
    x
}

normalize_singular <- function(x, col, word) {
    normalize (x, col, c(word, paste(word, 's', sep='')))
}

########################################################################################################################
# 1-) Loading file as data frames.
########################################################################################################################

#TRAIN_FILE_NAME = 'train.csv'

if (! exists('train_df')) train_df = read.csv(file = TRAIN_FILE_NAME, header = TRUE)
if (! exists('test_df')) test_df = read.csv(file = TEST_FILE_NAME, header = TRUE)

########################################################################################################################
# 2-) Data Preparation.
########################################################################################################################

prepare <- function(x) {

    # normalize names
    names(x) <- tolower(names(x))

    # tolower
    x$name <- tolower(x$name)

    # split last and first name
    name_tokens <- strsplit(x$name, ', ')
    x$firstname <- sapply(name_tokens, FUN=function(x) { x[2]})
    x$lastname <- sapply(name_tokens, FUN=function(x) { x[1]})
    x <- x[, ! names(x) %in% c('name')]
    x$firstname <- trim(x$firstname)

    # extract title from first name as flags
    # convert words to flags
    titles <- c('capt', 'col', 'don', 'dr', 'major', 'master', 'miss', 'mlle', 'mr', 'mrs', 'rev')
    for (title in titles) {
        x <- extract_word_as_flag(x, paste(title, '.', sep=''), 'firstname', paste('is', title, sep=''))
    }
    x$firstname <- trim(x$firstname)
    x
}


########################################################################################################################
# 3-) Decision Tree.
########################################################################################################################

# train
train <- prepare(train_df)
fit <- rpart(
            survived ~ pclass + sex + age + sibsp + parch + fare + embarked,
            data=train,
            method="class"
        )
plot(fit)
text(fit)
fancyRpartPlot(fit)

# predict
test <- prepare(test_df)

Prediction <- predict(fit, test, type = 'class')
submit <- data.frame(PassengerId = test$passengerid, Survived = Prediction)
write.csv(submit, file = 'dtree.csv', row.names = FALSE)

