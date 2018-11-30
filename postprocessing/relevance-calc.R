relevanceCalculation <- function(data, numberOfTerms = 30, raw = FALSE, reorder = TRUE) {
  source("postprocessing/relevance.R")
  library(tm)

  # Remove a bunch of sparse terms to avoid memory problems
  # Sparse 0.99 removes terms which only occur in VERY few documents (probably only 1)
  sparseMatrix = removeSparseTerms(data$dtm, sparse = 0.99)
  data$termFrequency = colSums(as.matrix(sparseMatrix))

  # Get correct data after removal of sparse terms
  # Terms
  usedTerms = colnames(sparseMatrix)

  # Phi frame
  phi = data$posterior$phi[][, colnames(data$posterior$phi) %in% colnames(sparseMatrix)]

  termSaliencyData <- salientTerms(phi = phi, theta = data$posterior$theta,
                                   vocab = usedTerms, doc.length = data$dtm$nrow,
                                   R = numberOfTerms, raw = raw, reorder.topics = reorder)

  return(termSaliencyData)
}