# Clear workspace
rm(list = ls())

# Variables
numberOfGrams <- 1:2
CSVfileName <- "articles.csv"
K <- 5 # Number of topics
G <- 10 # Iterations
alpha <- 0.01 # Document to topic distribution, chance that a document belongs to a certain topic
eta <- 0.01 # Topic to term distribution, chance that a certain term belongs to a topic (eg. topic 1 has a 0.05 chance of containing term X while topic 2 has a 0.0001 chance)

# Load libraries
library(tm)
library(lda)
library(LDAvis)
library(SnowballC)
library(quanteda)

# Creates N-grams, counts term occurance, removes terms with low count
createNGrams <- function(originalText) {
  # Create ngrams
  ngram <- dfm(originalText, ngrams = numberOfGrams, verbose = FALSE)
  # Get the term counts
  columnCounts <- colSums(ngram)
  
  # Create list we want to remove
  sparseTerms <- columnCounts < 2
  # Remove unwanted terms
  cleanedTerms <- columnCounts[!sparseTerms]
  
  # Create list of terms without dashes (for searching purposes)
  noDash <- gsub("_", " ", names(cleanedTerms))
  
  # Store the original text in a variable
  gramReadyText <- originalText
  # For each term (or compound term) check the original text and merge any occurences by adding a _
  for(i in 1:length(noDash)) {
    gramReadyText <- gsub(as.String(noDash[i]), names(cleanedTerms[i]), gramReadyText, ignore.case = TRUE)
  }
  
  # Returns the the text with _ added to N-gram compound terms
  return(gramReadyText)
}

# Stem the text and stem complete the text (with most prevalent term)
# Uses tm corpus as input
stemText <- function(originalCorpus) {
  # Stem the document
  stemmedCorpus <- tm_map(originalCorpus, stemDocument)
  
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
  
  # Create compound terms
  gramReadyText <- createNGrams(originalText)
  
  # Convert to corpus for stemming and stop word removal
  originalCorpus <- Corpus(VectorSource(gramReadyText))
  
  # Stem the corpus
  stemmedCorpus <- stemText(originalCorpus)
  
  # Remove stopwords and whitespaces
  cleanedCorpus <- removeStopWords(stemmedCorpus)
  
  # Return as corpus
  return(cleanedCorpus)
}

LDASimulation <- function(corpus) {
  # Convert tm corpus to document list, both LDA and topicmodels methods should use the TM to parse the original text
  # This line below can then convert for the LDA while topicmodels uses the TermDocumentMatrix
  LDADocuments <- as.list(data.frame(text = unlist(sapply(corpus, `[`, "content")), stringsAsFactors = F))[[1]]
  
  # Get terms by splitting the documents on whitespace
  usedTerms <- table(unlist(strsplit(LDADocuments, "[[:space:]]+")))
  
  # Lexicalize the documents
  LDADocuments <- lexicalize(LDADocuments, vocab = names(usedTerms))
  
  # Get the sum of terms per document
  tokensPerDoc <- sapply(LDADocuments, function(x) sum(x[2, ]))
  
  # Run LDA
  LDAData <- lda.collapsed.gibbs.sampler(documents = LDADocuments, K = K, vocab = names(usedTerms), num.iterations = G, alpha = alpha, eta = eta, compute.log.likelihood = TRUE)
  
  # Return a list of the LDA run, used terms, term frequency, and terms per document
  return(list(LDAData = LDAData, usedTerms = names(usedTerms), termFrequency = as.integer(usedTerms), tokensPerDoc = tokensPerDoc))
}

visualise <- function(LDAData, usedTerms, termFrequency, tokensPerDoc) {
  # Document to topic distribution estimate
  # Matrix where each column contains the probability distribution over topics for 1 document (1 cell is a probability of topic x for document y)
  theta <- t(apply(LDAData$document_sums + alpha, 2, function(x) x/sum(x)))
  # Topic to term distribution estimate
  # Matrix where each column contains the probability distribution over words for 1 topic (1 cell is a probability of word x for topic y)
  phi <- t(apply(t(LDAData$topics) + eta, 2, function(x) x/sum(x)))
  
  # Put all data into a single list
  json.raw <- list(phi = phi, theta = theta, doc.length = tokensPerDoc, vocab = usedTerms, term.frequency = termFrequency)
  
  # Create JSON format data
  json.data <- createJSON(phi = json.raw$phi, theta = json.raw$theta, doc.length = json.raw$doc.length, vocab = json.raw$vocab, term.frequency = json.raw$term.frequency)
  
  # Remove the directory (throws error otherwise)
  unlink('vis', recursive = TRUE)
  
  # Create browser scripts
  serVis(json.data, out.dir = 'vis', open.browser = FALSE)
}

#### TODO: fix the text input
articles.data <- read.csv(file = CSVfileName, header = FALSE)[1]
text <- as.matrix(c("In the process message"
                    , "Sorry i am had to step away for a moment message"
                    , "i am getting an error page that says QB is currently step unavailable"
                    , "That link gives me the same error page messages step"))

# Start!
cleanText <- cleanMyText(text)

lda <- LDASimulation(cleanText)

visualise(lda$LDAData, lda$usedTerms, lda$termFrequency, lda$tokensPerDoc)



