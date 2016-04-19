#' Complete run of all combinations of methods. Used in analysis, not used for tutorial.
#'
#' @param inputFile Input original file
#' @return
#'
#'
#'
#' @examples
#'
#'

tutorialAutoAnalysis <- function(inputFile){

  kmerFile <- prot-index(inputFile, w = 1, W = 2, g = 1, G = 1)

  fsMethods <- c("noFS", "RFE", "COR")
  mlMethods <- c("KNN", "RF", "NB", "SVML", "SVMR")
  accuracies <- data.frame(FS = character(), ML = character(), accuracy = numeric(), accuracySD = numeric(), stringsAsFactors = FALSE)

  for(fs in fsMethods){
    switch(fs,
           noFS = {
             data <- kmerfile
           },
           RFE = {
             data <- tutRFE(kmerFile)
           },
           COR = {
             data <- tutCorrFilter(kmerFile)
           })

    for(ml in mlMethods){
      switch(ml,
             KNN = {
               model <- tutKNNcv(data)
               bestK <- model$bestTune$k
               accuracy <- model$results$Accuracy[which(bestK==model$results$k)]
               accuracySD <- model$results$AccuracySD[which(bestK==model$results$k)]
             },
             RF = {
               model <- tutRFcv(data)
               accuracy <- model$results$Accuracy
               accuracySD <- model$results$AccuracySD
             },
             NB = {
               model <- tutNBcv(data)
               useK <- model$bestTune$usekernel
               accuracy <- model$results$Accuracy[which(useK==model$results$usekernel)]
               accuracySD <- model$results$AccuracySD[which(useK==model$results$usekernel)]
             },
             SVML = {
               model <- tutSVMcv(data, "linear")
               bestG <- model$bestTune$gamma
               bestC <- model$bestTune$cost
               accuracy <- model$results$Accuracy[which(bestC==model$results$cost & bestG==model$results$gamma)]
               accuracySD <- model$results$AccuracySD[which(bestC==model$results$cost & bestG==model$results$gamma)]
             },
             SVMR = {
               model <- tutSVMcv(data, "RBF")
               bestS <- model$bestTune$sigma
               bestC <- model$bestTune$C
               accuracy <- model$results$Accuracy[which(bestC==model$results$C & bestS==model$results$sigma)]
               accuracySD <- model$results$AccuracySD[which(bestC==model$results$C & bestS==model$results$sigma)]
             })
      #Then save accuracy results, parameter choices into a dataframe
      accuracies <- rbind(accuracies, data.frame(FS = fs,ML = ml, accuracy = accuracy, accuracySD = accuracySD, stringsAsFactors = FALSE))
    }

  }

  return(accuracies)
}
