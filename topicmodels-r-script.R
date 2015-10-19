# Clear workspace
rm(list = ls())

# Variables
CSVfileName <- "articles.csv"
K <- 5 # Number of topics
G <- 10 # Iterations
alpha <- 0.01 # Document to topic distribution, chance that a document belongs to a certain topic
eta <- 0.01 # Topic to term distribution, chance that a certain term belongs to a topic (eg. topic 1 has a 0.05 chance of containing term X while topic 2 has a 0.0001 chance)
# Add?: introduction, abstract, conclusion, etc.
extra_stop_words <- c("big", "references")

# Load libraries
library(tm)
library(LDAvis)

# Set workspace to folder where articles.csv is placed
setwd('~/workspace/R')

##### Read csv file #####
# Creates an empty column for some reason, only select first column
articles.data <- read.csv(file = CSVfileName, header = FALSE)[1]
# Transfor to lower case
articles.data <- tolower(as.matrix(articles.data))

# Tokenize on space and output as a list
articles.list <- strsplit(articles.data, "[[:space:]]+")

# Compute the table of terms
term.table.full <- table(unlist(articles.list))
# Sort by count
term.table.full <- sort(term.table.full, decreasing = TRUE)

##### Remove stop words #####
# Added empty ("") stopword otherwise ldavis took a stumble
stop_words <- c(stopwords("SMART"), "", extra_stop_words)

# Remove term if it is in the stop_words list or it occurs fewer than 5 times
term.del <- names(term.table.full) %in% stop_words | term.table.full < 5
term.table.clean <- term.table.full[!term.del]
# Select the used terms
term.used <- names(term.table.clean)
# Calculate frequency of each term
term.frequency <- as.integer(term.table.clean)

##### Create lexicon #####
documents.data <- lexicalize(articles.data, vocab = term.used)
# Get total tokens per document
documents.tokensPerDoc <- sapply(documents.data, function(x) sum(x[2, ]))

##### Run LDA #####
lda.data <- lda.collapsed.gibbs.sampler(documents = documents.data, K = K, vocab = term.used, num.iterations = G, alpha = alpha, eta = eta, compute.log.likelihood = TRUE)

##### Create JSON #####
# Document to topic distribution estimate
# Matrix where each column contains the probability distribution over topics for 1 document (1 cell is a probability of topic x for document y)
theta <- t(apply(lda.data$document_sums + alpha, 2, function(x) x/sum(x)))
# Topic to term distribution estimate
# Matrix where each column contains the probability distribution over words for 1 topic (1 cell is a probability of word x for topic y)
phi <- t(apply(t(lda.data$topics) + eta, 2, function(x) x/sum(x)))

# Put all data into a single list
json.raw <- list(phi = phi, theta = theta, doc.length = documents.tokensPerDoc, vocab = term.used, term.frequency = term.frequency)

# Create JSON format data
json.data <- createJSON(phi = json.raw$phi, theta = json.raw$theta, doc.length = json.raw$doc.length, vocab = json.raw$vocab, term.frequency = json.raw$term.frequency)

# Remove the directory (throws error otherwise)
unlink('vis', recursive = TRUE)

# Create browser scripts
serVis(json.data, out.dir = 'vis', open.browser = FALSE)




