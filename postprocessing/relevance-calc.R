relevanceCalculation <- function(id, numberOfTerms = 30, raw = FALSE) {
  source("postprocessing/relevance.R")
  library(tm)
    
  data <- readFileId(id)
  
  # Term frequency gave memory issues on cloud machine, calculate it here
  # Remove a bunch of sparse terms, avoid memory problems
  sparseMatrix = removeSparseTerms(data$dtm, sparse = 0.99)
  data$termFrequency = colSums(as.matrix(sparseMatrix))
  
  # Get correct data after removal of sparse terms
  # Terms
  usedTerms = colnames(sparseMatrix)
  
  # Phi frame
  phi = data$posterior$phi[][, colnames(data$posterior$phi) %in% colnames(sparseMatrix)]
  
  termSaliencyData <- salientTerms(phi = phi, theta = data$posterior$theta, 
                                   vocab = usedTerms, doc.length = data$tokensPerDoc, 
                                   R = numberOfTerms, raw = raw)
  
  return(termSaliencyData)
}