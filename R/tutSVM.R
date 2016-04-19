tutSVM <- function(inputFile, kernel = "linear", paramGrid = NULL, gridlen = 20, t = 33){
  if(!("caret" %in% installed.packages()[,1])){
    install.packages("caret")
  }

  library(caret)

  splitRows <- createDataPartition(inputFile$Class, p = t/100,
                                   list = FALSE,
                                   times = 1)

  trainData <- inputFile[-splitRows,]
  testData <- inputFile[splitRows,]

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
                    data = trainData,
                    method = kern,
                    metric = "Accuracy",
                    tuneGrid = paramGrid)
  } else{
    svmFit <- train(form = Class ~ .-SeqID,
                    data = trainData,
                    method = kern,
                    metric = "Accuracy",
                    tuneLength = gridLen)
  }


  predictedTrain <- predict(svmFit, newdata = trainData)
  trainConf <- confusionMatrix(predictedTrain, trainData$Class)
  print(trainConf)

  predictedTest <- predict(svmFit, newdata = testData)
  testConf <- confusionMatrix(predictedTest, testData$Class)
  print(testConf)

  return(list(Model = svmFit, ConfMat = list(trainConf,testConf)))
}
