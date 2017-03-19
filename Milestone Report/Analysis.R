### Analysis.R

### Load Libraries & set options

library(knitr)    # install.packages("knitr")
library(dplyr)    # install.packages("dplyr")
library(stringi)  # install.packages("stringi")
library(tm)       # install.packages("tm")
# RWeka requires Java Runtime: https://www.java.com/en/download/manual.jsp
library(RWeka)    # install.packages("RWeka")
library(ggplot2)  # install.packages("ggplot2")
library(wordcloud)# install.packages("wordcloud")

sampleSize <- 1000

### Set working diretory

setwd("C:/Dropbox/Data Scientist Course/10 - Capstone Project/Milestone Report")


### Download data source 

if (!file.exists("Coursera-SwiftKey.zip")){
  download.file("https://d396qusza40orc.cloudfront.net/dsscapstone/dataset/Coursera-SwiftKey.zip", 
                "Coursera-SwiftKey.zip")
  unzip("Coursera-SwiftKey.zip")
}

### Read data files - get samples - get statistics

if (!(file.exists("./AllDataSamples.txt") & (file.exists("./fileSummary.RDS")))) {
  
  ### Read the data files
  
  blogs <- readLines("./final/en_US/en_US.blogs.txt", encoding = "UTF-8", skipNul=TRUE)
  news <- readLines("./final/en_US/en_US.news.txt", encoding = "UTF-8", skipNul=TRUE)
  twitter <- readLines("./final/en_US/en_US.twitter.txt", encoding = "UTF-8", skipNul=TRUE)
  
  ### Extract a data sample from each file
  
  sampleTwitter <- twitter[sample(1:length(twitter),sampleSize)]
  sampleNews <- news[sample(1:length(news),sampleSize)]
  sampleBlogs <- blogs[sample(1:length(blogs),sampleSize)]
  dataSample <- c(sampleTwitter,sampleNews,sampleBlogs)
  
  ### Save combined samples to text files
  
  writeLines(sampleTwitter, "./TwitterSamples.txt")
  writeLines(sampleNews, "./NewsSamples.txt")
  writeLines(sampleBlogs, "./BlogsSamples.txt")
  writeLines(dataSample, "./AllDataSamples.txt")
  
  ### Gather Data Set Statistics  
  
  # Get file sizes
  blogs.size <- file.info("final/en_US/en_US.blogs.txt")$size / 1024 ^ 2
  news.size <- file.info("final/en_US/en_US.news.txt")$size / 1024 ^ 2
  twitter.size <- file.info("final/en_US/en_US.twitter.txt")$size / 1024 ^ 2
  sample.size <- file.info("./DataSample.txt")$size / 1024 ^ 2
  
  # Get the number of words in each file
  # stri_count_words {stringi} - count the number of text boundaries 
  blogs.words <- stri_count_words(blogs)
  news.words <- stri_count_words(news)
  twitter.words <- stri_count_words(twitter)
  sample.words <- stri_count_words(dataSample)
  
  # Create a summary of the data sets
  fileSummary <- data.frame(source = c("blogs", "news", "twitter","dataSample"),
                            file.size.MB = c(blogs.size, news.size, twitter.size,sample.size),
                            num.lines = c(length(blogs), length(news), length(twitter), length(dataSample)),
                            num.words = c(sum(blogs.words), sum(news.words), sum(twitter.words), sum(sample.words)),
                            mean.words = c(mean(blogs.words), mean(news.words), mean(twitter.words), mean(sample.words)))
  
  colnames(fileSummary) <- c("File Name", "File Size (MB)", "Line Count", "Word Count", "Avg Words/Line")
  
  saveRDS(fileSummary, file = "./fileSummary.RDS")
  
  ## memory cleanup
  rm(blogs)
  rm(blogs.size)
  rm(blogs.words)
  rm(fileSummary)
  rm(news)
  rm(news.size)
  rm(news.words)
  rm(sample.size)
  rm(sample.words)
  rm(sampleBlogs)
  rm(sampleNews)
  rm(sampleTwitter)
  rm(twitter)
  rm(twitter.size)
  rm(twitter.words)
}

con <- file("./AllDataSamples.txt")
dataSample <- readLines(con)
close(con)
rm(con)  

fileSummaryDF <- readRDS("./fileSummary.RDS")

### Data Cleaning  

if (!file.exists("./cleanedCorpus.RDS")) {
  
  # Create a 'corpus' and clean up the data
  # A corpus creates a seperate document from each line in the source text + it's metadata
  # VCorpus {tm} - create volatile corpora
  # VectorSource {tm} - create a vector source which interprets each element of the vector as a document
  corpus <- VCorpus(VectorSource(dataSample))
  # content_transformer {tm} - create content transformers - functions which modify the content of an R object
  toSpace <- content_transformer(function(x, pattern) gsub(pattern, " ", x))
  # tm_map {tm} - transformation on corpora - apply functions (also denoted as mappings) to corpora
  corpus <- tm_map(corpus, toSpace, "(f|ht)tp(s?)://(.*)[.][a-z]+")
  corpus <- tm_map(corpus, toSpace, "@[^\\s]+")
  corpus <- tm_map(corpus, tolower)
  corpus <- tm_map(corpus, removeWords, stopwords("en"))
  corpus <- tm_map(corpus, removePunctuation)
  corpus <- tm_map(corpus, removeNumbers)
  corpus <- tm_map(corpus, stripWhitespace)
  corpus <- tm_map(corpus, PlainTextDocument)
  
  saveRDS(corpus, file = "./cleanedCorpus.RDS")
}  

corpus <- readRDS("./cleanedCorpus.RDS")

##
## End of preprocessing
##

## Inspection
#inspect(corpus[1:10])
#corpusDF <- data.frame(text=unlist(sapply(corpus, `[`, "content")), stringsAsFactors=F)
#corpusDF[1:10,]

### N-Gram Tokenization

if (!(file.exists("./uniFreq.RDS") & (file.exists("./biFreq.RDS")) & (file.exists("./triFreq.RDS")))) {
  
  if (!file.exists("./dtm.RDS")) {
    dtm <- DocumentTermMatrix(corpus)
    # inspect:
    # dtm
    # inspect(dtm[1:5, 1:100])
    
    saveRDS(dtm, file = "./dtm.RDS")
  }
  
  dtm <- readRDS("./dtm.RDS")
  # findFreqTerms(dtm, lowfreq=50)
  
  bi <- function(x) NGramTokenizer(x, Weka_control(min=2, max=2))
  tri <- function(x) NGramTokenizer(x, Weka_control(min=3, max=3))
  
  uniGramMatrix <- TermDocumentMatrix(corpus)
  biGramMatrix <- TermDocumentMatrix(corpus, control = list(tokenize = bi))
  triGramMatrix <- TermDocumentMatrix(corpus, control = list(tokenize = tri))
  
  freq_func <- function(matrix){
    f <- sort(rowSums(as.matrix(matrix)), decreasing = TRUE)
    result <- data.frame(word = names(f), freq= f)
    return(result)
  }
  
  uniFreq <- freq_func(removeSparseTerms(uniGramMatrix, 0.99))
  saveRDS(uniFreq, file = "./uniFreq.RDS")
  biFreq <- freq_func(removeSparseTerms(biGramMatrix, 0.999))
  saveRDS(biFreq, file = "./biFreq.RDS")
  triFreq <- freq_func(removeSparseTerms(triGramMatrix, 0.9999))
  saveRDS(triFreq, file = "./triFreq.RDS")

}
  
uniFreq <- readRDS("./uniFreq.RDS")
biFreq <- readRDS("./biFreq.RDS")
triFreq <- readRDS("./triFreq.RDS")


#wordcloud(corpus, scale=c(5,0.5), max.words=100, random.order=FALSE, rot.per=0.35, use.r.layout=FALSE, colors=brewer.pal(8, "Dark2"))
wordcloud(corpus, min.freq = 25, random.order = FALSE, colors = brewer.pal(8, "Spectral"))


plot_func <- function(freq,top,title) {
  freq_top <- head(freq,top)
  ggplot(freq_top,aes(x=reorder(word,freq), y=freq, fill=freq)) +
    geom_bar(stat = "identity") +
    theme_bw() +
    coord_flip() +
    theme(axis.title.y = element_blank()) +
    labs(y="Frequency", title=title)
}

plot_func(uniFreq,15,"Top Unigrams")
plot_func(biFreq,15,"Top Bigrams")
plot_func(triFreq,15,"Top Trigrams")

