tutKNN <- function(inputFile, knum = 10, t = 33){
  if(!("caret" %in% installed.packages()[,1])){
    install.packages("caret")
  }

  library(caret)

  #Split the data, balanced on class
  splitRows <- createDataPartition(inputFile$Class, p = t/100,
                                   list = FALSE,
                                   times = 1)

  trainData <- inputFile[-splitRows,]
  testData <- inputFile[splitRows,]


  #Run knn on training data, predict on test
  knnFit <- train(form = Class ~ .-SeqID,
                  data = trainData,
                  method = "knn",
                  metric = "Accuracy",
                  tuneGrid = expand.grid(k = c(knum)))

  predictedTrain <- predict(knnFit, newdata = trainData)
  trainConf <- confusionMatrix(predictedTrain, trainData$Class)
  print(trainConf)

  predictedTest <- predict(knnFit, newdata = testData)
  testConf <- confusionMatrix(predictedTest, testData$Class)
  print(testConf)

  return(list(Model = knnFit, ConfMat = list(trainConf,testConf)))

}
