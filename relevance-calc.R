relevanceCalculation <- function(id, numberOfTerms = 30) {
  # Can also get the raw relevancy instead of saliency
  # as.matrix(terms(LDAData, numberOfTerms))
  
  source("relevance.R")
  library(tm)
    
  data <- readFileId(id)
  
  # Term frequency gave memory issues on cloud machine, calculate it here
  # Remove a bunch of sparse terms
  sparseMatrix = removeSparseTerms(data$dtm, sparse = 0.99)
  data$termFrequency = colSums(as.matrix(sparseMatrix))
  
  # Get correct data after removal of sparse terms
  # Terms
  usedTerms = colnames(sparseMatrix)
  # Phi frame
  phi = data$posterior$phi[][, colnames(data$posterior$phi) %in% colnames(sparseMatrix)]
  
  termSaliencyData <- salientTerms(phi = phi, theta = data$posterior$theta, 
                                   vocab = usedTerms, doc.length = data$tokensPerDoc, 
                                   term.frequency = data$termFrequency, R = numberOfTerms)
  
  return(termSaliencyData)
}