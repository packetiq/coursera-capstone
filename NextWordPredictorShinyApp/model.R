library(tm)

#setwd("C:/Dropbox/Data Scientist Course/10 - Capstone Project/NextWordPredictorShinyApp")
debug <- FALSE

bigrams <- readRDS(file="./data/bigrams.Rds")
trigrams <- readRDS(file="./data/trigrams.Rds")
quadgrams <- readRDS(file="./data/quadgrams.Rds")

nextWordPredictor <- function(inputTxt) {

  if(nchar(inputTxt) > 0) {
      #clean input
      inputTxt <- tolower(inputTxt)
      inputTxt <- removeNumbers(inputTxt)
      inputTxt <- removePunctuation(inputTxt)
      inputTxt <- stripWhitespace(inputTxt)
      
      if (debug) { print(inputTxt) }
      
      #split into words
      inputList <- unlist(strsplit(inputTxt, " "))
      if (debug) { print(inputList) }
      
      numWords <- length(inputList)
      if (debug) { print(numWords) }
      
      runBigram <- function(words) {
          bigrams[bigrams$termone == words,]$termtwo
      }
      
      runTrigram <- function(words) {
          trigrams[trigrams$termone == words[1] &
                     trigrams$termtwo == words[2],]$termthree
      }
      
      runQuadgram <- function(words) {
          quadgrams[ quadgrams$termone == words[1] &
                       quadgrams$termtwo == words[2] &
                       quadgrams$termthree == words[3],]$termfour
      }
      
      if(numWords == 1) {
          if (debug) { print("running bigrams") }
          predList <- runBigram(inputList[1])
      } else if (numWords == 2) {
          if (debug) { print("running trigrams") }
          word1 <- inputList[1]
          word2 <- inputList[2]
          predList <- runTrigram(c(word1, word2))
          
          if(length(predList) == 0) {
              if (debug) { print("trigrams failed running bigrams") }
              predList <- runBigram(word2)
          }
      } else {
          if (debug) { print("running quadgram") }
          word1 <- inputList[numWords-2]
          word2 <- inputList[numWords-1]
          word3 <- inputList[numWords]
          predList <- runQuadgram(c(word1, word2, word3))
          
          if(length(predList) == 0){
              if (debug) { print("quadgram failed running trigram") }
              predList <- runTrigram(c(word2,word3))
          }
          
          if(length(predList) == 0){
              if (debug) { print("trigram failed running bigram") }
              predList <- runBigram(word3)
          }
      }
      
      if (debug) { print("returning top n predictors") }
      n <- 4
      tp <- length(predList)
      
      if( tp >= n){
          predList <- predList[1:n]

          if (debug) { print(cat("tp: ", tp, "  n: ", n)) }
      }
      
      # output the prediction list if it exists      
      as.character(predList)

  } else {
      ""
  }

}

