#' Naive Bayes
#'
#' @param inputFile Input k-mer file
#' @return
#'
#'
#'
#' @export
#' @examples
#'
#'

tutNB <- function(inputFile, t = 33){
  if(!("caret" %in% installed.packages()[,1])){
    install.packages("caret")
  }

  library(caret)

  splitRows <- createDataPartition(inputFile$Class, p = t/100,
                                   list = FALSE,
                                   times = 1)

  trainData <- inputFile[-splitRows,]
  testData <- inputFile[splitRows,]

  nbFit <- train(form = Class ~ .-SeqID,
                 data = trainData,
                 method = "nb",
                 metric = "Accuracy")

  predictedTrain <- predict(nbFit, newdata = trainData)
  trainConf <- confusionMatrix(predictedTrain, trainData$Class)
  print(trainConf)

  predictedTest <- predict(nbFit, newdata = testData)
  testConf <- confusionMatrix(predictedTest, testData$Class)
  print(testConf)

  return(list(Model = nbFit, ConfMat = list(trainConf,testConf)))
}
