#' Cross validated NB for analysis
#' NOT VIEWABLE IN PACKAGE
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
