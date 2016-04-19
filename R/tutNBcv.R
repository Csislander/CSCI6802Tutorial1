#' Cross validated NB for analysis, not used for tutorial.
#'
#' @param
#' @return
#'
#'
#'
#' @examples
#'
#'

tutNBcv <- function(inputFile, folds = 5){

  ctrl <- trainControl(method = "cv", number = folds)

  nbFit <- train(form = Class ~ .-SeqID,
                 data = inputFile,
                 method = "nb",
                 metric = "Accuracy",
                 trControl = ctrl)

  print(nbFit)

  return(nbFit)
}
