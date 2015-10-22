# Clear workspace
rm(list = ls())

# Variables
numberOfGrams <- 1:2
CSVfileName <- "articles.csv"
K <- 5 # Number of topics
G <- 200 # Iterations
alpha <- 0.01 # Document to topic distribution, chance that a document belongs to a certain topic
eta <- 0.01 # Topic to term distribution, chance that a certain term belongs to a topic (eg. topic 1 has a 0.05 chance of containing term X while topic 2 has a 0.0001 chance)

# Load libraries
library(tm)
library(lda)
library(LDAvis)
library(SnowballC)
library(quanteda)
library(stringi)
library(topicmodels)
library(dplyr)

# Set workspace to folder where articles.csv is placed
setwd('~/workspace/R')

# Creates N-grams, counts term occurance, removes terms with low count
# Uses tm corpus as input
createNGrams <- function(originalCorpus) {
  # Convert back to document list (function uses Corpus as input for interoperability)
  originalText <- as.list(data.frame(text = unlist(sapply(originalCorpus, `[`, "content")), stringsAsFactors = F))[[1]]
  
  originalText <- gsub("-", "", originalText)
  
  # Create ngrams
  ngram <- dfm(originalText, ngrams = numberOfGrams, verbose = FALSE)
  # Get the term counts
  columnCounts <- colSums(ngram)
  
  # Create list we want to remove
  sparseTerms <- columnCounts < 15 | stri_length(names(columnCounts)) <= 6
  # Remove unwanted terms
  cleanedTerms <- columnCounts[!sparseTerms]
  
  # Create list of terms without dashes (for searching purposes)
  noDash <- gsub("_", " ", names(cleanedTerms))
  
  # Store the original text in a variable
  gramReadyText <- originalText
  # For each term (or compound term) check the original text and merge any occurences by adding a _
  # Stupid paste construction to concat a space to the beginning and end of the search string to avoid merging compound terms
  for(i in 1:length(noDash)) {
    gramReadyText <- gsub(paste("", paste(as.String(noDash[i]), "", sep = " "), sep = " "), paste("", paste(names(cleanedTerms[i]), "", sep = " "), sep = " "), gramReadyText, ignore.case = TRUE)
  }
  
  # Convert into corpus again
  gramReadyText <- Corpus(VectorSource(gramReadyText))
  
  # Returns the corpus with _ added to N-gram compound terms
  return(gramReadyText)
}

# Stem the text and stem complete the text (with most prevalent term)
# Uses tm corpus as input
stemText <- function(originalCorpus) {
  # Stem the document
  stemmedCorpus <- tm_map(originalCorpus, stemDocument)
  # Make sure there is no extra whitespace because of processing steps
  stemmedCorpus <- tm_map(stemmedCorpus, stripWhitespace)
  
  # Fix the stem completion (https://stackoverflow.com/questions/25206049/stemcompletion-is-not-working, answer by cdxsza)
  stemCompletion_mod <- function(stemmedSingleDocument, dict = originalCorpus) {
    PlainTextDocument(stripWhitespace(paste(stemCompletion(unlist(strsplit(as.character(stemmedSingleDocument), " ")), dictionary = dict, type = "prevalent"), sep = "", collapse = " ")))
  }
  
  # Apply the stem completion
  stemCompletedCorpus <- tm_map(stemmedCorpus, stemCompletion_mod)
  
  # Return the corpus
  return(stemCompletedCorpus)
}

# Remove stopwords and extra whitespaces
# Uses tm corpus as input
removeStopWords <- function(originalCorpus) {
  # Remove stopwords
  result <- tm_map(originalCorpus, removeWords, stopwords("SMART"))
  # Remove double spaces (side effect of removeWords)
  result <- tm_map(result, stripWhitespace)
  
  # Define whitespace trimmer (start/end of string)
  trim_mod <- function(x) {
    PlainTextDocument(trimws(x$content))
  }
  
  # Trim string and return
  return(tm_map(result, trim_mod))
}

cleanMyText <- function(originalText) {
  # Lowercase everything
  originalText <- toLower(originalText)
  
  # Convert to corpus
  originalCorpus <- Corpus(VectorSource(originalText))
  
  # Create compound terms
  grammedCorpus <- createNGrams(originalCorpus)
  
  # Stem the corpus
  stemmedCorpus <- stemText(grammedCorpus)
  
  # Remove stopwords and whitespaces
  cleanedCorpus <- removeStopWords(stemmedCorpus)
  
  # Return as corpus
  return(cleanedCorpus)
}

# Read the CSV file
text <- as.matrix(read.csv(file = CSVfileName, header = FALSE)[1]) 

# Start!
cleanCorpus <- cleanMyText(text)

lda <- LDASimulation(cleanCorpus)
visualise(lda$LDAData, lda$usedTerms, lda$termFrequency, lda$tokensPerDoc, lda$phi, lda$theta, "lda_vis")

#lda <- TmLDASimulation(cleanCorpus)
#visualise(lda$LDAData, lda$usedTerms, lda$termFrequency, lda$tokensPerDoc, lda$phi, lda$theta, "tm_vis")



