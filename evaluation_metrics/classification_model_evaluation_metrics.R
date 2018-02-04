# Source file for common classification model evaluation metrics.
# The functions given in this file can be used to compute a given classification model evaluation metric.

# Example 
#' data(cars)
#' logreg <- glm(formula = vs ~ hp + wt,
#'               family = binomial(link = "logit"), data = mtcars)
#' pred <- ifelse(logreg$fitted.values < 0.5, 0, 1)
#' ZeroOneLoss(y_pred = pred, y_true = mtcars$vs)
#' [1] 0.125 

ZeroOneLoss <- function(y_pred, y_true) {
  ZeroOneLoss <- mean(y_true != y_pred)
  return(ZeroOneLoss)
}


#' @title Accuracy
#'
#' @description
#' Compute the accuracy classification score.
#'
#' @param y_pred Predicted labels vector, as returned by a classifier
#' @param y_true Ground truth (correct) 0-1 labels vector
#' @return Accuracy
#' @examples
#' data(cars)
#' logreg <- glm(formula = vs ~ hp + wt,
#'               family = binomial(link = "logit"), data = mtcars)
#' pred <- ifelse(logreg$fitted.values < 0.5, 0, 1)
#' Accuracy(y_pred = pred, y_true = mtcars$vs)
#' @export

Accuracy <- function(y_pred, y_true) {
  Accuracy <- mean(y_true == y_pred)
  return(Accuracy)
}


#' @title Confusion Matrix
#'
#' @description
#' Compute confusion matrix to evaluate the accuracy of a classification.
#'
#' @param y_pred Predicted labels vector, as returned by a classifier
#' @param y_true Ground truth (correct) 0-1 labels vector
#' @return a table of Confusion Matrix
#' @examples
#' data(cars)
#' logreg <- glm(formula = vs ~ hp + wt,
#'               family = binomial(link = "logit"), data = mtcars)
#' pred <- ifelse(logreg$fitted.values < 0.5, 0, 1)
#' ConfusionMatrix(y_pred = pred, y_true = mtcars$vs)
#' @export

ConfusionMatrix <- function(y_pred, y_true) {
  Confusion_Mat <- table(y_true, y_pred)
  return(Confusion_Mat)
}


#' @title Confusion Matrix (Data Frame Format)
#'
#' @description
#' Compute data frame format confusion matrix for internal usage.
#'
#' @param y_pred Predicted labels vector, as returned by a classifier
#' @param y_true Ground truth (correct) 0-1 labels vector
#' @return a data.frame of Confusion Matrix
#' @examples
#' data(cars)
#' logreg <- glm(formula = vs ~ hp + wt,
#'               family = binomial(link = "logit"), data = mtcars)
#' pred <- ifelse(logreg$fitted.values < 0.5, 0, 1)
#' ConfusionDF(y_pred = pred, y_true = mtcars$vs)
#' @keywords internal
#' @export

ConfusionDF <- function(y_pred, y_true) {
  Confusion_DF <- transform(as.data.frame(ConfusionMatrix(y_pred, y_true)),
                            y_true = as.character(y_true),
                            y_pred = as.character(y_pred),
                            Freq = as.integer(Freq))
  return(Confusion_DF)
}
utils::globalVariables("Freq")

#' @title Precision
#'
#' @description
#' Compute the precision score.
#'
#' @param y_pred Predicted labels vector, as returned by a classifier
#' @param y_true Ground truth (correct) 0-1 labels vector
#' @param positive An optional character string for the factor level that
#'   corresponds to a "positive" result
#' @return Precision
#' @examples
#' data(cars)
#' logreg <- glm(formula = vs ~ hp + wt,
#'               family = binomial(link = "logit"), data = mtcars)
#' pred <- ifelse(logreg$fitted.values < 0.5, 0, 1)
#' Precision(y_pred = pred, y_true = mtcars$vs, positive = "0")
#' Precision(y_pred = pred, y_true = mtcars$vs, positive = "1")
#' @export

Precision <- function(y_true, y_pred, positive = NULL) {
  Confusion_DF <- ConfusionDF(y_pred, y_true)
  if (is.null(positive) == TRUE) positive <- as.character(Confusion_DF[1,1])
  TP <- as.integer(subset(Confusion_DF, y_true==positive & y_pred==positive)["Freq"])
  FP <- as.integer(sum(subset(Confusion_DF, y_true!=positive & y_pred==positive)["Freq"]))
  Precision <- TP/(TP+FP)
  return(Precision)
}


#' @title Recall
#'
#' @description
#' Compute the recall score.
#'
#' @param y_pred Predicted labels vector, as returned by a classifier
#' @param y_true Ground truth (correct) 0-1 labels vector
#' @param positive An optional character string for the factor level that
#'   corresponds to a "positive" result
#' @return Recall
#' @examples
#' data(cars)
#' logreg <- glm(formula = vs ~ hp + wt,
#'               family = binomial(link = "logit"), data = mtcars)
#' pred <- ifelse(logreg$fitted.values < 0.5, 0, 1)
#' Recall(y_pred = pred, y_true = mtcars$vs, positive = "0")
#' Recall(y_pred = pred, y_true = mtcars$vs, positive = "1")
#' @export

Recall <- function(y_true, y_pred, positive = NULL) {
  Confusion_DF <- ConfusionDF(y_pred, y_true)
  if (is.null(positive) == TRUE) positive <- as.character(Confusion_DF[1,1])
  TP <- as.integer(subset(Confusion_DF, y_true==positive & y_pred==positive)["Freq"])
  FN <- as.integer(sum(subset(Confusion_DF, y_true==positive & y_pred!=positive)["Freq"]))
  Recall <- TP/(TP+FN)
  return(Recall)
}


#' @title Sensitivity
#'
#' @description
#' Compute the sensitivity score.
#'
#' @param y_pred Predicted labels vector, as returned by a classifier
#' @param y_true Ground truth (correct) 0-1 labels vector
#' @param positive An optional character string for the factor level that
#'   corresponds to a "positive" result
#' @return Sensitivity
#' @examples
#' data(cars)
#' logreg <- glm(formula = vs ~ hp + wt,
#'               family = binomial(link = "logit"), data = mtcars)
#' pred <- ifelse(logreg$fitted.values < 0.5, 0, 1)
#' Sensitivity(y_pred = pred, y_true = mtcars$vs, positive = "0")
#' Sensitivity(y_pred = pred, y_true = mtcars$vs, positive = "1")
#' @export

Sensitivity  <- function(y_true, y_pred, positive = NULL) {
  Confusion_DF <- ConfusionDF(y_pred, y_true)
  if (is.null(positive) == TRUE) positive <- as.character(Confusion_DF[1,1])
  TP <- as.integer(subset(Confusion_DF, y_true==positive & y_pred==positive)["Freq"])
  FN <- as.integer(sum(subset(Confusion_DF, y_true==positive & y_pred!=positive)["Freq"]))
  Sensitivity <- TP/(TP+FN)
  return(Sensitivity)
}


#' @title Specificity
#'
#' @description
#' Compute the specificity score.
#'
#' @param y_pred Predicted labels vector, as returned by a classifier
#' @param y_true Ground truth (correct) 0-1 labels vector
#' @param positive An optional character string for the factor level that
#'   corresponds to a "positive" result
#' @return Specificity
#' @examples
#' data(cars)
#' logreg <- glm(formula = vs ~ hp + wt,
#'               family = binomial(link = "logit"), data = mtcars)
#' pred <- ifelse(logreg$fitted.values < 0.5, 0, 1)
#' Specificity(y_pred = pred, y_true = mtcars$vs, positive = "0")
#' Specificity(y_pred = pred, y_true = mtcars$vs, positive = "1")
#' @export

Specificity  <- function(y_true, y_pred, positive = NULL) {
  Confusion_DF <- ConfusionDF(y_pred, y_true)
  if (is.null(positive) == TRUE) positive <- as.character(Confusion_DF[1,1])
  TN <- as.integer(subset(Confusion_DF, y_true!=positive & y_pred!=positive)["Freq"])
  FP <- as.integer(sum(subset(Confusion_DF, y_true!=positive & y_pred==positive)["Freq"]))
  Specificity <- TN/(TN+FP)
  return(Specificity)
}


#' @title F1 Score
#'
#' @description
#' Compute the F1 Score.
#'
#' @param y_pred Predicted labels vector, as returned by a classifier
#' @param y_true Ground truth (correct) 0-1 labels vector
#' @param positive An optional character string for the factor level that
#'   corresponds to a "positive" result
#' @return F1 Score
#' @examples
#' data(cars)
#' logreg <- glm(formula = vs ~ hp + wt,
#'               family = binomial(link = "logit"), data = mtcars)
#' pred <- ifelse(logreg$fitted.values < 0.5, 0, 1)
#' F1_Score(y_pred = pred, y_true = mtcars$vs, positive = "0")
#' F1_Score(y_pred = pred, y_true = mtcars$vs, positive = "1")
#' @export

F1_Score <- function(y_true, y_pred, positive = NULL) {
  Confusion_DF <- ConfusionDF(y_pred, y_true)
  if (is.null(positive) == TRUE) positive <- as.character(Confusion_DF[1,1])
  Precision <- Precision(y_true, y_pred, positive)
  Recall <- Recall(y_true, y_pred, positive)
  F1_Score <- 2 * (Precision * Recall) / (Precision + Recall)
  return(F1_Score)
}


#' @title F-Beta Score
#'
#' @description
#' Compute the F-Beta Score
#'
#' @param y_pred Predicted labels vector, as returned by a classifier
#' @param y_true Ground truth (correct) 0-1 labels vector
#' @param positive An optional character string for the factor level that
#'   corresponds to a "positive" result
#' @param beta Weight of precision in harmonic mean
#' @return F-Beta Score
#' @examples
#' data(cars)
#' logreg <- glm(formula = vs ~ hp + wt,
#'               family = binomial(link = "logit"), data = mtcars)
#' pred <- ifelse(logreg$fitted.values < 0.5, 0, 1)
#' FBeta_Score(y_pred = pred, y_true = mtcars$vs, positive = "0", beta = 2)
#' FBeta_Score(y_pred = pred, y_true = mtcars$vs, positive = "1", beta = 2)
#' @export

FBeta_Score <- function(y_true, y_pred, positive = NULL, beta = 1) {
  Confusion_DF <- ConfusionDF(y_pred, y_true)
  if (is.null(positive) == TRUE) positive <- as.character(Confusion_DF[1,1])
  Precision <- Precision(y_true, y_pred, positive)
  Recall <- Recall(y_true, y_pred, positive)
  Fbeta_Score <- (1 + beta^2) * (Precision * Recall) / (beta^2 * Precision + Recall)
  return(Fbeta_Score)
}


#' @title Log loss / Cross-Entropy Loss
#'
#' @description
#' Compute the log loss/cross-entropy loss.
#'
#' @param y_pred Predicted probabilities vector, as returned by a classifier
#' @param y_true Ground truth (correct) 0-1 labels vector
#' @return Log loss/Cross-Entropy Loss
#' @examples
#' data(cars)
#' logreg <- glm(formula = vs ~ hp + wt,
#'               family = binomial(link = "logit"), data = mtcars)
#' LogLoss(y_pred = logreg$fitted.values, y_true = mtcars$vs)
#' @export

LogLoss <- function(y_pred, y_true) {
  eps <- 1e-15
  y_pred <- pmax(pmin(y_pred, 1 - eps), eps)
  LogLoss <- -mean(y_true * log(y_pred) + (1 - y_true) * log(1 - y_pred))
  return(LogLoss)
}


#' @title Multi Class Log Loss
#'
#' @description
#' Compute the multi class log loss.
#'
#' @param y_true Ground truth (correct) labels vector or a matrix of
#'   correct labels indicating by 0-1, same format as probabilities matrix
#' @param y_pred Predicted probabilities matrix, as returned by a classifier
#' @return Multi Class Log Loss
#' @examples
#' data(iris)
#' svm.model <- e1071::svm(Species~., data = iris, probability = TRUE)
#' pred <- predict(svm.model, iris, probability = TRUE)
#' MultiLogLoss(y_true = iris$Species, y_pred = attr(pred, "probabilities"))
#' @importFrom stats model.matrix
#' @export

MultiLogLoss <- function(y_pred, y_true) {
  if (is.matrix(y_true) == FALSE) {
    y_true <- model.matrix(~ 0 + ., data.frame(as.character(y_true)))
  }
  eps <- 1e-15
  N <- nrow(y_pred)
  y_pred <- pmax(pmin(y_pred, 1 - eps), eps)
  MultiLogLoss <- (-1 / N) * sum(y_true * log(y_pred))
  return(MultiLogLoss)
}


#' @title Area Under the Receiver Operating Characteristic Curve (ROC AUC)
#'
#' @description
#' Compute the Area Under the Receiver Operating Characteristic Curve (ROC AUC) from prediction scores.
#'
#' @param y_pred Predicted probabilities vector, as returned by a classifier
#' @param y_true Ground truth (correct) 0-1 labels vector
#' @return Area Under the ROC Curve (ROC AUC)
#' @examples
#' data(cars)
#' logreg <- glm(formula = vs ~ hp + wt,
#'               family = binomial(link = "logit"), data = mtcars)
#' AUC(y_pred = logreg$fitted.values, y_true = mtcars$vs)
#' @export

AUC <- function(y_pred, y_true) {
  rank <- rank(y_pred)
  n_pos <- as.double(sum(y_true == 1))
  n_neg <- as.double(sum(y_true == 0))
  AUC <- (sum(rank[y_true == 1]) - n_pos * (n_pos + 1) / 2) / (n_pos * n_neg)
  return(AUC)
}


#' @title Gini Coefficient
#'
#' @description
#' Compute the Gini Coefficient.
#'
#' @param y_pred Predicted probabilities vector, as returned by a classifier
#' @param y_true Ground truth (correct) 0-1 labels vector
#' @return Gini Coefficient
#' @examples
#' data(cars)
#' logreg <- glm(formula = vs ~ hp + wt,
#'               family = binomial(link = "logit"), data = mtcars)
#' Gini(y_pred = logreg$fitted.values, y_true = mtcars$vs)
#' @export

Gini <- function(y_pred, y_true) {
  NormalizedGini(y_pred, y_true)
}


#' @title Area Under the Precision-Recall Curve (PR AUC)
#'
#' @description
#' Compute the Area Under the Precision-Recall Curve (PR AUC) from prediction scores.
#'
#' @param y_pred Predicted probabilities vector, as returned by a classifier
#' @param y_true Ground truth (correct) 0-1 labels vector
#' @return Area Under the PR Curve (PR AUC)
#' @examples
#' data(cars)
#' logreg <- glm(formula = vs ~ hp + wt,
#'               family = binomial(link = "logit"), data = mtcars)
#' PRAUC(y_pred = logreg$fitted.values, y_true = mtcars$vs)
#' @export

PRAUC <- function(y_pred, y_true) {
  pred_obj <- ROCR::prediction(y_pred, y_true)
  perf_obj <- ROCR::performance(pred_obj, measure = "prec", x.measure = "rec")
  PRAUC <- Area_Under_Curve(perf_obj@x.values[[1]], perf_obj@y.values[[1]], method = "trapezoid", na.rm = TRUE)
  return(PRAUC)
}


#' @title Area Under the Lift Chart
#'
#' @description
#' Compute the Area Under the Lift Chart from prediction scores.
#'
#' @param y_pred Predicted probabilities vector, as returned by a classifier
#' @param y_true Ground truth (correct) 0-1 labels vector
#' @return Area Under the Lift Chart
#' @examples
#' data(cars)
#' logreg <- glm(formula = vs ~ hp + wt,
#'               family = binomial(link = "logit"), data = mtcars)
#' LiftAUC(y_pred = logreg$fitted.values, y_true = mtcars$vs)
#' @export

LiftAUC <- function(y_pred, y_true) {
  pred_obj <- ROCR::prediction(y_pred, y_true)
  perf_obj <- ROCR::performance(pred_obj, measure = "lift", x.measure = "rpp")
  LiftAUC <- Area_Under_Curve(perf_obj@x.values[[1]], perf_obj@y.values[[1]], method = "trapezoid", na.rm = TRUE)
  return(LiftAUC)
}


#' @title Area Under the Gain Chart
#'
#' @description
#' Compute the Area Under the Gain Chart from prediction scores.
#'
#' @param y_pred Predicted probabilities vector, as returned by a classifier
#' @param y_true Ground truth (correct) 0-1 labels vector
#' @return Area Under the Gain Chart
#' @examples
#' data(cars)
#' logreg <- glm(formula = vs ~ hp + wt,
#'               family = binomial(link = "logit"), data = mtcars)
#' GainAUC(y_pred = logreg$fitted.values, y_true = mtcars$vs)
#' @export

GainAUC <- function(y_pred, y_true) {
  pred_obj <- ROCR::prediction(y_pred, y_true)
  perf_obj <- ROCR::performance(pred_obj, measure = "tpr", x.measure = "rpp")
  GainAUC <- Area_Under_Curve(perf_obj@x.values[[1]], perf_obj@y.values[[1]], method = "trapezoid", na.rm = TRUE)
  return(GainAUC)
}


#' @title Kolmogorov-Smirnov Statistic
#'
#' @description
#' Compute the Kolmogorov-Smirnov statistic.
#'
#' @param y_pred Predicted probabilities vector, as returned by a classifier
#' @param y_true Ground truth (correct) 0-1 labels vector
#' @return Kolmogorov-Smirnov statistic
#' @examples
#' data(cars)
#' logreg <- glm(formula = vs ~ hp + wt,
#'               family = binomial(link = "logit"), data = mtcars)
#' KS_Stat(y_pred = logreg$fitted.values, y_true = mtcars$vs)
#' @export

KS_Stat <- function(y_pred, y_true) {
  pred_obj <- ROCR::prediction(y_pred, y_true)
  perf_obj <- ROCR::performance(pred_obj, measure = "tpr", x.measure = "fpr")
  KS_Stat <- max(perf_obj@y.values[[1]] - perf_obj@x.values[[1]])
  return(KS_Stat)
}




