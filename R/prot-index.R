#############################################
#CSCI 6802: Bioinformatics Algorithms       #
#Course Project: R Recreation of tutorial 1 #
#File: prot-index.R                         #
#Thomas Crowell, thomas.crowell@dal.ca      #
#############################################

#' K-mer indexing function
#'
#' @param inputFile Path to datafile
#' @return
#'
#'
#'
#' @export
#' @examples
#'
#'

prot.index <- function(inputFile, w = 0, W = 0, g = 0, G = 0){
  #If minimum word length is longer than maximum word length, set minumum to max word length
  if(w > W){
    w <- W
    warning("Minimum word length is larger than maximum word length")
  }

  #If minimum gap length is longer than maximum gap length, set minumum to max gap length
  if(g > G){
    g <- G
    warning("Minimum gap length is larger than maximum gap length")
  }

  if(w < 1){
    w <- 1
    warning("Minimum word length must be 1 or greater. Changing to w = 1")
  }

  if(g < 0){
    g <- 0
    warning("Minimum gap length cannot be negative. Changing to g = 0")
  }

  #Read in the file first and adjust for ,s in protein names, create temp file
  wholeFile <- readChar(inputFile, file.info(inputFile)$size)
  wholeFile <- gsub(pattern = ", ", x = wholeFile, replacement = " ")
  newFile <- paste0(substr(inputFile, 0 , nchar(inputFile)-4),"new.txt")
  file.create(newFile)
  writeChar(object = wholeFile, con = newFile)

  #Read in the data
  data <- read.table(file = newFile,
            sep = ",",
            header = FALSE,
            stringsAsFactors = FALSE,
            col.names = c("SetID", "NCBIref", "Name", "Sequence"),
            skip = 1,
            skipNul = TRUE)

  #Remove temp file
  file.remove(newFile)

  cat("Opened input file ", inputFile, " \n",
      "\tWord sizes = ", w, " - ", W, "\n",
      "\tMax gap spec =" , g, " - ", G, "\n",
      "\n", sep = "")

  protAlphabet <- c('A','C','D','E','F','G','H','I','K','L','M','N','P','Q','R','S','T','V','W','Y')
  varNames <- c()

  if(W >= 1){
    for(wordLength in w:W){
      stopIndex <- length(protAlphabet)^wordLength - 1

      for(wordIndex in 0:stopIndex){
        kmer <- ""

        for(position in 0:(wordLength-1)){
          addChar <- protAlphabet[(wordIndex %% length(protAlphabet)^(position + 1) / length(protAlphabet)^(position)) + 1]
          kmer <- paste0(addChar, kmer)
          print(paste(kmer,position,wordIndex,wordLength))
        }

        varNames <- c(varNames, kmer)
      }
    }
  }

  if(G >= 1){
    for(gapLength in g:G){

      for(first in protAlphabet){

        for(second in protAlphabet){
          varNames <- c(varNames, paste0(first, second, gapLength))
        }
      }
    }
  }

  seqIndexHash <- data.frame(matrix(ncol = 2 + length(varNames), nrow = 0))
  colnames(seqIndexHash) <- c("Class", "SeqID", varNames)

  for(i in 1:nrow(data)){
    seque = data[i,"Sequence"]

    seqIndexHash[i,"Class"] <- data[i,"SetID"]
    seqIndexHash[i,"SeqID"] <- data[i,"NCBIref"]
    seqIndexHash[i,3:(length(varNames)+2)] <- 0

    for(j in 1:nchar(seque)){
      for(wordLength in w:W){
        subkmer <- substr(seque, j, j + wordLength - 1)
        seqIndexHash[i, subkmer] <- seqIndexHash[i, subkmer] + 1
      }

      for(gapLength in g:G){
        if((j + gapLength + 1) <= nchar(seque)){
          subkmer <- paste0(substr(seque, j, j), substr(seque, j + gapLength + 1, j + gapLength + 1), gapLength)
          seqIndexHash[i, subkmer] <- seqIndexHash[i, subkmer] + 1
        }
      }
    }

    for(col in 3:(length(varNames)+2)){
      index = colnames(seqIndexHash)[col]
      if(length(grep(pattern = "[A-Za-z]$", x = index, perl = TRUE))){
        seqIndexHash[i,col] <- seqIndexHash[i,col] / (nchar(seque) - nchar(index))
      }
      else if(length(grep(pattern = "[0-9]+$", x = index, perl = TRUE))){
        seqIndexHash[i,col] <- seqIndexHash[i,col] / (nchar(seque) - 2 - as.numeric(regmatches(index, regexec(pattern = "[0-9]+$", text = index))[[1]]))
      }
    }
  }

  #Append X to the beginning to avoid problems later
  #R repeatedly wants Class to be numeric, since it is only 0,1. Causes problem when modeling
  seqIndexHash[,"Class"] <- paste0("X",seqIndexHash[,"Class"])
  seqIndexHash[,"Class"] <- as.factor(seqIndexHash[,"Class"])

  return(seqIndexHash)
}


