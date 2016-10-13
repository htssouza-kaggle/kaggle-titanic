########################################################################################################################
# Kaggle Titanic Challenge
# https://www.kaggle.com/c/titanic
########################################################################################################################

########################################################################################################################
# Dependencies and Libraries
########################################################################################################################

for (.requirement in c("caret")) {
  if (! .requirement %in% rownames(installed.packages())) {
    install.packages(.requirement)
  }
}

library(caret)

########################################################################################################################
# Evaluation Functions (MCC)
########################################################################################################################

# eval function predict | values
mccEval <- function(y_hat, y) {
  cm <- confusionMatrix(y_hat, y)
  tt <- as.numeric(cm$table) # true table inside cm
  resp <- mcc(tt[4], tt[2], tt[3], tt[1]) # mcc(tt[2,2],tt[2,1],tt[1,2],tt[1,1])
  return(resp)
}

# direct eval function
mcc <- function(TP, FP, FN, TN) {
  num <- (TP * TN) - (FP * FN)
  den <- (TP + FP) * (TP + FN) * (TN + FP) * (TN + FN)

  if (den == 0) {
    return(0)
  } else {
    return(num / sqrt(den))
  }
}
