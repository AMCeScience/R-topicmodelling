if (!exists("configLoaded")) source("config.R")

# Load libraries
library(tm)
library(SnowballC)
library(quanteda)
library(stringi)
library(stringr)
library(methods)

###############################
# TM_MAP compatible functions #
###############################

# Removes the whitespace from beginning and end of document content
trimWhitespace <- function(document) {
  PlainTextDocument(trimws(document$content))
}

# Stem completes document
stemComplete <- function(document, dict) {
  # Fix the stem completion (https://stackoverflow.com/questions/25206049/stemcompletion-is-not-working, answer by cdxsza)
  PlainTextDocument(stripWhitespace(paste(stemCompletion(unlist(strsplit(as.character(document), " ")), dictionary = dict, type = "prevalent"), sep = "", collapse = " ")))
}

# Removes dashes between words
removeDash <- function(document) {
  PlainTextDocument(str_replace_all(document$content, "-", " "))
}

# Removes words longer than 26 characters from the string
removeLong <- function(document) {
  PlainTextDocument(stripWhitespace(str_replace_all(document$content, "[:alpha:]{26,}", "")))
}

###############################

# Creates N-grams, counts term occurance, removes terms with low count
# Uses tm corpus as input
createNGrams <- function(originalCorpus) {
  # Convert back to document list (function uses Corpus as input for interoperability)
  originalText <- as.list(data.frame(text = paste("", paste(unlist(sapply(originalCorpus, `[`, "content")), "", sep = " "), sep = " "), stringsAsFactors = F))[[1]]
  
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
  
  result <- tm_map(gramReadyText, trimWhitespace)
  
  # Returns the corpus with _ added to N-gram compound terms
  return(result)
}

# Stem the text and stem complete the text (with most prevalent term)
# Uses tm corpus as input
stemText <- function(originalCorpus) {
  # Stem the document
  stemmedCorpus <- tm_map(originalCorpus, stemDocument)
  # Make sure there is no extra whitespace because of processing steps
  stemmedCorpus <- tm_map(stemmedCorpus, stripWhitespace)
  
  # Apply the stem completion
  stemCompletedCorpus <- tm_map(stemmedCorpus, stemComplete, originalCorpus)
  
  # Return the corpus
  return(stemCompletedCorpus)
}

# Remove stopwords
# Uses tm corpus as input
removeStopWords <- function(originalCorpus, extra) {
  # Remove stopwords
  result <- tm_map(originalCorpus, removeWords, c(stopwords("SMART"), extra))
  
  result <- tm_map(result, stripWhitespace)
  result <- tm_map(result, trimWhitespace)
  
  return(result)
}

# Remove words that are weirdly long (e.g. > 30 letters, while max. size in English is approx. 26)
# Uses tm corpus as input
removeOverlyLongWords <- function(originalCorpus) {
  result <- tm_map(originalCorpus, removeLong)
  
  result <- tm_map(result, stripWhitespace)
  result <- tm_map(result, trimWhitespace)
  
  return(result)
}

# Remove special characters and extra whitespace at beginning and end of document
# Uses tm corpus as input
removeSpecialCharacters <- function(originalCorpus) {
  # Remove dash (-), punctuation, and numbers
  result <- tm_map(originalCorpus, removeDash)
  result <- tm_map(result, removePunctuation)
  result <- tm_map(result, removeNumbers)
  
  result <- tm_map(result, stripWhitespace)
  result <- tm_map(result, trimWhitespace)
  
  return(result)
}

cleanMyText <- function(originalText) {
  # Lowercase everything
  originalText <- toLower(originalText)
  
  # Convert to corpus
  result <- Corpus(VectorSource(originalText))
  
  # Remove special characters
  result <- removeSpecialCharacters(result)
  
  # Remove stopwords
  result <- removeStopWords(result, extraStopWords)
  
  # Create compound terms
  result <- createNGrams(result)
  
  # Stem the corpus
  result <- stemText(result)
  
  # Another round after N-grams
  result <- removeStopWords(result, extraStopWords)
  
  # If any weirdly long words are left, remove them
  result <- removeOverlyLongWords(result)
  
  # Return as corpus
  return(result)
}

addIDs <- function(corpus) {
  for (i in 1:length(corpus)) {
    corpus[[i]]$meta$id = i
  }
  
  return(corpus)
}

readCSV <- function(name) {
  text <- as.matrix(read.csv(file = name, header = FALSE)[1])
  
  return(text)
}

runPreprocessing <- function(csv_name, store = FALSE, name = "clean_corpus.rds") {
  documents <- readCSV(csv_name)
  
  # Start!
  cleanCorpus <- cleanMyText(documents)
  
  cleanCorpus <- addIDs(cleanCorpus)
  
  if (store == TRUE) {
    # https://stackoverflow.com/questions/19967478/how-to-save-data-file-into-rdata
    saveRDS(cleanCorpus, paste("data", name, sep = "/"))
  }
  
  return(cleanCorpus) 
}

# testCorpus <- runPreprocessing(CSVfileName)



