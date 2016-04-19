tutRFE <- function(inputFile, featureSize =seq(round(ncol(inputFile)*0.5),round(ncol(inputFile)*0.25), length.out = 5)){

  if(!("caret" %in% installed.packages()[,1])){
    install.packages("caret")
  }

  library(caret)

  corMat <- cor(inputFile[,3:ncol(inputFile)])
  remove <- findCorrelation(corMat, cutoff=0.99)

  woCor <- inputFile[,-(remove + 2)]

  ctrl <- rfeControl(functions = caretFuncs,
                     method = "cv",
                     verbose = FALSE)

  profile <- rfe(inputFile[,3:ncol(inputFile)], inputFile[,1],
                 sizes = featureSize,
                 rfeControl = ctrl)

  bestVars <- rownames(varImp(profile))

  return(cbind(inputFile[,1:2],inputFile[,bestVars]))
}
