library(lda)
library(LDAvis)
library(topicmodels)
library(dplyr)
library(stringi)
library(tm)

LDASimulation <- function(corpus) {
  # Convert tm corpus to document list, both LDA and topicmodels methods should use the TM to parse the original text
  # This line below can then convert for the LDA while topicmodels uses the TermDocumentMatrix
  # https://stackoverflow.com/questions/21148049/r-topic-modeling-lda-command-lexicalize-giving-unexpected-results
  LDADocuments <- as.list(data.frame(text = unlist(sapply(corpus, `[`, "content")), stringsAsFactors = F))[[1]]
  
  # Get terms by splitting the documents on whitespace
  usedTerms <- table(unlist(strsplit(LDADocuments, "[[:space:]]+")))
  
  # Lexicalize the documents
  LDADocuments <- lexicalize(LDADocuments, vocab = names(usedTerms))
  
  # Get the sum of terms per document
  tokensPerDoc <- sapply(LDADocuments, function(x) sum(x[2, ]))
  
  # Run LDA
  LDAData <- lda.collapsed.gibbs.sampler(documents = LDADocuments, K = K, vocab = names(usedTerms), num.iterations = G, alpha = alpha, eta = eta, compute.log.likelihood = TRUE)
  
  # Document to topic distribution estimate
  # Matrix where each column contains the probability distribution over topics for 1 document (1 cell is a probability of topic x for document y)
  theta <- t(apply(LDAData$document_sums + alpha, 2, function(x) x/sum(x)))
  # Topic to term distribution estimate
  # Matrix where each column contains the probability distribution over words for 1 topic (1 cell is a probability of word x for topic y)
  phi <- t(apply(t(LDAData$topics) + eta, 2, function(x) x/sum(x)))
  
  saveRDS(list(LDAData = LDAData, usedTerms = names(usedTerms), termFrequency = as.integer(usedTerms), tokensPerDoc = tokensPerDoc, phi = phi, theta = theta), gsub("__", K, "data/perplexity__.rds"))
  
  # Return a list of the LDA run, used terms, term frequency, and terms per document
  #return()
}

TmLDASimulation <- function(corpus) {
  dtm <- DocumentTermMatrix(corpus)
  
  LDAData <- LDA(dtm, k = K, method = "Gibbs")
  
  # Solution to translate between topicmodels and LDAVis: http://www.r-bloggers.com/a-link-between-topicmodels-lda-and-ldavis/
  # Document to topic distribution estimate
  # Matrix where each column contains the probability distribution over topics for 1 document (1 cell is a probability of topic x for document y)
  theta <- posterior(LDAData)$topics %>% as.matrix
  # Topic to term distribution estimate
  # Matrix where each column contains the probability distribution over words for 1 topic (1 cell is a probability of word x for topic y)
  phi <- posterior(LDAData)$terms %>% as.matrix
  
  usedTerms <- colnames(phi)
  
  tokensPerDoc <- vector()
  
  for (i in 1:length(corpus)) {
    tokensPerDoc <- c(tokensPerDoc, stri_count(paste(corpus[[i]]$content, collapse = ' '), regex = '\\S+'))
  }
  
  #tempFrequency <- inspect(dtm)
  #termFrequency <- data.frame(ST = colnames(dtm), Freq = colSums(dtm))
  termFrequency <- colSums(inspect(dtm))
  
  saveRDS(list(LDAData = LDAData, usedTerms = usedTerms, termFrequency = termFrequency, tokensPerDoc = tokensPerDoc, phi = phi, theta = theta), gsub("__", K, "data/perplexity__.rds"))
}

visualise <- function(LDAData, usedTerms, termFrequency, tokensPerDoc, phi, theta, outputFolder) {
  # Put all data into a single list
  json.raw <- list(phi = phi, theta = theta, doc.length = tokensPerDoc, vocab = usedTerms, term.frequency = termFrequency)
  
  # Create JSON format data
  json.data <- createJSON(phi = json.raw$phi, theta = json.raw$theta, doc.length = json.raw$doc.length, vocab = json.raw$vocab, term.frequency = json.raw$term.frequency)
  
  # Remove the directory (throws error otherwise)
  unlink(outputFolder, recursive = TRUE)
  
  # Create browser scripts
  serVis(json.data, out.dir = outputFolder, open.browser = FALSE)
}

LDASimulation(cleanCorpus)
#visualise(lda$LDAData, lda$usedTerms, lda$termFrequency, lda$tokensPerDoc, lda$phi, lda$theta, "lda_vis")

#TmLDASimulation(cleanCorpus)
#saveRDS(lda, gsub("__", K, "data/perplexity__.rds"))
#visualise(lda$LDAData, lda$usedTerms, lda$termFrequency, lda$tokensPerDoc, lda$phi, lda$theta, "tm_vis")