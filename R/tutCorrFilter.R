#' Correlational filtering
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


tutCorrFilter <- function(inputFile, c = 0.5, printMat = FALSE){

  if(!("caret" %in% installed.packages()[,1])){
    install.packages("caret")
  }

  library(caret)

  corMat <- cor(inputFile[,3:ncol(inputFile)])
  if(printMat){
    corMat
  }
  remove <- findCorrelation(corMat, cutoff=c)

  print("Removed features: ")
  print(names(inputFile)[remove + 2])

  return(inputFile[,-(remove + 2)])
}
