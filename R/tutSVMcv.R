#' Cross validated SVM for analysis, not used for tutorial.
#'
#'
#' @param
#' @return
#'
#'
#'
#' @examples
#'
#'

tutSVMcv <- function(inputFile, kernel = "linear", paramGrid = NULL, gridlen = 20, folds = 5){

  ctrl <- trainControl(method = "cv", number = folds)

  if(kernel == "linear"){
    kern <- "svmLinear2"
  } else if(kernel == "RBF"){
    kern <- "svmRadial"
  } else{
    warning("Invalid kernel. Defaulting to linear kernel")
    kern <- "svmLinear2"
  }

  if(!is.null(paramGrid)){
    svmFit <- train(form = Class ~ .-SeqID,
                    data = inputFile,
                    method = kern,
                    metric = "Accuracy",
                    trControl = ctrl,
                    tuneGrid = paramGrid)
  } else{
    svmFit <- train(form = Class ~ .-SeqID,
                    data = inputFile,
                    method = kern,
                    metric = "Accuracy",
                    trControl = ctrl,
                    tuneLength = gridLen)
  }

  print(svmFit)


  return(svmFit)
}
