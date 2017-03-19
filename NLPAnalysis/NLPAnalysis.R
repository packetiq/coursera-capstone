# NLPAnalysis.R

# Load libraries
library(tm)
library(RWekajars)
library(RWeka)
library(wordcloud)
require(openNLP)
require(reshape2)

# Other options
set.seed(696)
sample_pct <- .4

setwd("C:/Dropbox/Data Scientist Course/10 - Capstone Project/NLPAnalysis")

##############################################################################
### Data Load
##############################################################################

# Download corpus files 
if (!file.exists("./Coursera-SwiftKey.zip")) {
  download.file("https://d396qusza40orc.cloudfront.net/dsscapstone/dataset/Coursera-SwiftKey.zip", 
                "Coursera-SwiftKey.zip")
  unzip("Coursera-SwiftKey.zip")
}


# Get data & stats
if (!file.exists("./data/corpus_stats.Rds") || 
    !file.exists("./data/blogs_data.Rds") ||
    !file.exists("./data/blogs_frequency.Rds") ||
    !file.exists("./data/news_data.Rds") ||
    !file.exists("./data/news_frequency.Rds") ||
    !file.exists("./data/twitter_data.Rds") ||
    !file.exists("./data/twitter_frequency.Rds")) {

  # File locations
  us_txt_dir <- "final/en_US/"
  blogs_txt <- paste(us_txt_dir, "en_US.blogs.txt", sep = "")
  news_txt <- paste(us_txt_dir, "en_US.news.txt", sep = "")
  twitter_txt <- paste(us_txt_dir, "en_US.twitter.txt", sep = "")
  # Source http://www.freewebheaders.com/full-list-of-bad-words-banned-by-google/
  bad_words_txt <- "full-list-of-bad-words-banned-by-google.txt"
  
  # Load text files & convert to lowercase
  blogs_data <- tolower(readLines(blogs_txt, skipNul = T))
  news_data <- tolower(readLines(news_txt, skipNul = T))
  twitter_data <- tolower(readLines(twitter_txt, skipNul = T))
  bad_words_data <- tolower(readLines(bad_words_txt, skipNul = T))
  
  blogs_size <- round(file.size(blogs_txt)/1048576, 2)
  news_size <- round(file.size(news_txt)/1048576, 2)
  twitter_size <- round(file.size(twitter_txt)/1048576, 2)
  
  # Get line counts
  blogs_lines <- length(blogs_data)
  news_lines <- length(news_data)
  twitter_lines <- length(twitter_data)
  
  # Get max line lengths
  blogs_char_cnt <- lapply(blogs_data, nchar)
  blogs_max_chars <- blogs_char_cnt[[which.max(blogs_char_cnt)]]
  
  news_char_cnt <- lapply(news_data, nchar)
  news_max_chars <- news_char_cnt[[which.max(news_char_cnt)]]
  
  twitter_char_cnt <- lapply(twitter_data, nchar)
  twitter_max_chars <- twitter_char_cnt[[which.max(twitter_char_cnt)]]
  
  # Get word counts (based on spaces)
  blogs_words <- sum( sapply(gregexpr("\\S+", blogs_data), length ) )
  news_words <- sum( sapply(gregexpr("\\S+", news_data), length ) )
  twitter_words <- sum( sapply(gregexpr("\\S+", twitter_data), length ) )
  
  # Summary of corpus stats
  corpus_stats <- data.frame( "Files" = c("Blogs", "News", "Twitter"),
                              "Lines" = c(blogs_lines, news_lines, twitter_lines),
                              "Longest_Line" = c(blogs_max_chars, news_max_chars, twitter_max_chars),
                              "Words" = c(blogs_words, news_words, twitter_words),
                              "File_Size_Mb" = c(blogs_size, news_size, twitter_size))
  
  saveRDS(blogs_data, "./data/blogs_data.Rds")
  saveRDS(news_data, "./data/news_data.Rds")
  saveRDS(twitter_data, "./data/twitter_data.Rds")
  saveRDS(corpus_stats,"./data/corpus_stats.Rds")
  
} else {
  blogs_data <- readRDS("./data/blogs_data.Rds")
  news_data <- readRDS("./data/news_data.Rds")
  twitter_data <- readRDS("./data/twitter_data.Rds")
  corpus_stats <- readRDS("./data/corpus_stats.Rds")
}


##############################################################################
### Exploratory Analysis
##############################################################################

### Functions for Analysis
##############################################################################

freq_func <- function(matrix){
  f <- sort(rowSums(as.matrix(matrix)), decreasing = TRUE)
  result <- data.frame(word = names(f), freq= f)
  return(result)
}

clean_corpus <- function(corpus) {
  
  corpus <- tm_map(corpus, removeNumbers)
  corpus <- tm_map(corpus, removePunctuation)
  corpus <- tm_map(corpus, removeWords, stopwords('english'))
  corpus <- tm_map(corpus, stripWhitespace)
  return (corpus)
}

get_frequency <- function(dtm) {
  matx <- as.matrix(dtm)
  freq <- colSums(matx)
  freq <- sort(freq, decreasing = TRUE)
  return (freq)
}


### Analyze Blogs Data
##############################################################################
if (!file.exists("./data/blogs_frequency.Rds")) {
  
  blogs_data_sample <- blogs_data[sample(1:blogs_lines, blogs_lines*sample_pct)]
  
  # Create a corpus
  blogs_cp <- Corpus(VectorSource(list(blogs_data_sample)))
  
  # Clean up corpus
  blogs_cp <- clean_corpus(blogs_cp)
  
  # Create doc term matrix
  blogs_dtm <- DocumentTermMatrix(blogs_cp, control = list(stopwords = TRUE))
  
  # Find frequent words
  blogs_frequency <- get_frequency(blogs_dtm)
  
  # Save
  saveRDS(blogs_frequency, "./data/blogs_frequency.Rds")

} else {
  blogs_frequency <- readRDS("./data/blogs_frequency.Rds")
}

### News Data
##############################################################################
if (!file.exists("./data/news_frequency.Rds"))  {
    

  news_data_sample <- news_data[sample(1:news_lines, news_lines*sample_pct)]
  
  # Create a corpus
  news_cp <- Corpus(VectorSource(list(news_data_sample)))
  
  # Clean up corpus
  news_cp <- clean_corpus(news_cp)
  
  # Create doc term matrix
  news_dtm <- DocumentTermMatrix(news_cp)
  
  # Find frequent words
  news_frequency <- get_frequency(news_dtm)
  
  # Save
  saveRDS(news_frequency, "./data/news_frequency.Rds")

} else {
  news_frequency <- readRDS("./data/news_frequency.Rds")
}

### Twitter Data
##############################################################################
if (!file.exists("./data/twitter_frequency.Rds")) {
  
  twitter_data_sample <- twitter_data[sample(1:twitter_lines, twitter_lines*sample_pct)]
  
  # Create a corpus
  twitter_cp <- Corpus(VectorSource(list(twitter_data_sample)))
  
  # Clean up corpus
  twitter_cp <- clean_corpus(twitter_cp)
  
  # Create doc term matrix
  twitter_dtm <- DocumentTermMatrix(twitter_cp)
  
  # Find frequent words
  twitter_frequency <- get_frequency(twitter_dtm)
  
  # Save
  saveRDS(twitter_frequency, "./data/twitter_frequency.Rds")
  
} else {
  news_frequency <- readRDS("./data/news_frequency.Rds")
}

### Full Data Set & Corpus
##############################################################################

if (!file.exists("./data/sample_frequency.Rds") || 
    !file.exists("./data/cleaned_corpus.Rds")) {
  
  # Create smaller samples for processing combined corpus
  sample_pct <- .1
  
  blogs_data_sample <- blogs_data[sample(1:blogs_lines, blogs_lines*sample_pct)]
  news_data_sample <- news_data[sample(1:news_lines, news_lines*sample_pct)]
  twitter_data_sample <- twitter_data[sample(1:twitter_lines, twitter_lines*sample_pct)]
  sample_data <- list(blogs_data_sample, news_data_sample, twitter_data_sample)
  
  # Create a corpus
  cp <- Corpus(VectorSource(sample_data))
  
  # Clean up corpus
  cp <- clean_corpus(cp)
  
  # Remove colorful language
  # cp <- tm_map(cp, removeWords, bad_words_data )
  
  # Create doc term matrix
  dtm <- DocumentTermMatrix(cp, control = list(stopwords = TRUE))
  
  # Find frequent words
  frequency <- get_frequency(dtm)
  
  # Create a wordcloud 
  # wordcloud(names(frequency), frequency, min.freq = 25, random.order = FALSE, colors = brewer.pal(8, "Spectral"))
  
  # Save
  saveRDS(frequency, "./data/sample_frequency.Rds")
  saveRDS(cp, "./data/cleaned_corpus.Rds")
  
} else {
  frequency <- readRDS("./data/sample_frequency.Rds")  
  cp <- readRDS("./data/cleaned_corpus.Rds")

}



##############################################################################
### Tokenization
##############################################################################

### Tokenization Functions
##############################################################################
ngramTokenizer <- function(n) {
  function(x) unlist(lapply(ngrams(words(x), n), paste, collapse = " "), use.names = FALSE)
}

# Generate unigram data set
generateNgramData <- function(corpus, n) {
  if(n == 1) {
    ng_tdm <- TermDocumentMatrix(corpus)
  } else {
    ng_tdm <- TermDocumentMatrix(corpus, control = list(tokenize = ngramTokenizer(n)))
  }
  
  ng_matrix <- as.matrix(ng_tdm)
  ng_matrix <- rowSums(ng_matrix)
  ng_matrix <- sort(ng_matrix, decreasing = TRUE)
  ngram <- data.frame(terms = names(ng_matrix), freq = ng_matrix)
  
  if(n == 2) columns <- c('one', 'two')
  if(n == 3) columns <- c('one', 'two', 'three')
  if(n == 4) columns <- c('one', 'two', 'three', 'four')
  
  if(n > 1) {
    ngram <- transform(ngram, terms = colsplit(ngram$terms, " ", names = columns ))
  }
  
  rownames(ngram) <- NULL
  ngram
}

### Create N-Grams
### Note - this takes a while
##############################################################################

# Unigrams
if (!file.exists("./data/unigrams.Rds")) {

  unigrams <- generateNgramData(cp, 1)
  # Calculate probabilities
  unigram_count <- sum(unigrams$freq)
  unigrams <- transform(unigrams, p = freq / unigram_count, pw = 0)

  saveRDS(unigrams, "./data/unigrams.Rds")
  
} else {
  unigrams <- readRDS("./data/unigrams.Rds")
}

# Bigrams
if (!file.exists("./data/bigrams.Rds")) {

  bigrams <- generateNgramData(cp, 2)
  bigram_count <- sum(bigrams$freq)
  bigrams <- transform(bigrams, p = freq / bigram_count, pone = 0, termone = terms$one, termtwo = terms$two, terms = NULL)

  saveRDS(bigrams, "./data/bigrams.Rds")
  
} else {
  
  bigrams <- readRDS("./data/bigrams.Rds")
}

# Trigrams
if (!file.exists("./data/trigrams.Rds")) {
  
  trigrams <- generateNgramData(cp, 3)
  trigram_count <- sum(trigrams$freq)
  trigrams <- transform(trigrams, p = freq / trigram_count, pw = 0, termone = terms$one, termtwo = terms$two, termthree = terms$three, terms = NULL)

  saveRDS(trigrams, "./data/trigrams.Rds")
  
} else {
  
  trigrams <- readRDS("./data/trigrams.Rds")
}

# Quadgrams
if (!file.exists("./data/quadgrams.Rds")) {
  
  quadgrams <- generateNgramData(cp, 4)
  quadgram_count <- sum(quadgrams$freq)
  quadgrams <- transform(quadgrams, p = freq / quadgram_count, pw = 0, termone = terms$one, termtwo = terms$two, termthree = terms$three, termfour = terms$four, terms = NULL)

  saveRDS(quadgrams, "./data/quadgrams.Rds")
  
} else {
  
  quadgrams <- readRDS("./data/quadgrams.Rds")
}


### Optimize data set size & Save
##############################################################################

# Reduce data set size by only keeping n-grams greater than the avg count
bigrams_sm <- bigrams[bigrams$freq > mean(bigrams$freq),]
trigrams_sm <- trigrams[trigrams$freq > mean(trigrams$freq),]
quadgrams_sm <- quadgrams[quadgrams$freq > mean(quadgrams$freq),]

saveRDS(bigrams_sm, file = "data/bigrams_sm.Rds")
saveRDS(trigrams_sm, file = "data/trigrams_sm.Rds")
saveRDS(quadgrams_sm, file = "data/quadgrams_sm.Rds")

###
### Notes

## Rda is just a short name for RData. You can just save(), load(), attach(), etc. just like you do with RData.

## Rds stores a single R object. You may readRDS() and save(), or load() and saveRDS() selectively.
## See: http://stat.ethz.ch/R-manual/R-devel/library/base/html/readRDS.html
