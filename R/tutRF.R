#' Random Forest
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

tutRF <- function(inputFile, ntree = 100, t = 33){
  if(!("caret" %in% installed.packages()[,1])){
    install.packages("caret")
  }

  library(caret)

  splitRows <- createDataPartition(inputFile$Class, p = t/100,
                                   list = FALSE,
                                   times = 1)

  trainData <- inputFile[-splitRows,]
  testData <- inputFile[splitRows,]

  rfFit <- train(form = Class ~ .-SeqID,
                 data = trainData,
                 method = "rf",
                 metric = "Accuracy",
                 tuneGrid = expand.grid(mtry = c(ntree)))

  predictedTrain <- predict(rfFit, newdata = trainData)
  trainConf <- confusionMatrix(predictedTrain, trainData$Class)
  print(trainConf)

  predictedTest <- predict(rfFit, newdata = testData)
  testConf <- confusionMatrix(predictedTest, testData$Class)
  print(testConf)

  return(list(Model = rfFit, ConfMat = list(trainConf,testConf)))
}
