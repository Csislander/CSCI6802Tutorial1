#' Cross validated KNN for analysis, not used for tutorial.
#'
#' @param
#' @return
#'
#'
#'
#' @examples
#'
#'

tutKNNcv <- function(inputFile, folds = 5, paramGrid = expand.grid(k = 2:30), repeatn = 3){

  #Run knn with cv on data
  ctrl <- trainControl(method = "repeatedcv", number = folds, repeats = repeatn)

  knnFit <- train(form = Class ~ .-SeqID,
                  data = inputFile,
                  method = "knn",
                  metric = "Accuracy",
                  trControl = ctrl,
                  tuneGrid = paramGrid)

  print(knnFit)
  print(plot(knnFit))

  return(knnFit)

}
