#' Cross validated RF for analysis
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

tutRFcv <- function(inputFile, ntree = 100, folds = 5){

  ctrl <- trainControl(method = "cv", number = folds)

  rfFit <- train(form = Class ~ .-SeqID,
                 data = inputFile,
                 method = "rf",
                 metric = "Accuracy",
                 trControl = ctrl,
                 tuneGrid = expand.grid(mtry = c(ntree)))

  print(rfFit)

  return(rfFit)
}
