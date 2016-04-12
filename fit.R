################
# RETURN VALUE #
################

# LDAData
# usedTerms
# termFrequency
# tokensPerDoc
# posterior
#   phi
#   theta
# control
#   alpha
#   delta
#   burnin
#   iter
#   keep
# numberOfTopics

library(lda)
library(LDAvis)
library(topicmodels)
library(tm)
library(dplyr)
library(stringi)

LDASimulation <- function(corpus, K, alpha, beta, burnin, iter, keep) {
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
  
  control = list(alpha = alpha, delta = beta, burnin = burnin, iter = iter)
  
  print("LDASimulation setup done.")
  print(proc.time() - timer)
  
  # Run LDA
  LDAData <- lda.collapsed.gibbs.sampler(documents = LDADocuments, 
                                         K = K, 
                                         vocab = names(usedTerms), 
                                         num.iterations = control$iter, 
                                         alpha = control$alpha, 
                                         eta = control$delta, 
                                         burnin = control$burnin,
                                         compute.log.likelihood = TRUE)
  
  print("LDASimulation run done.")
  print(proc.time() - timer)
  
  # Document to topic distribution estimate
  # Matrix where each column contains the probability distribution over topics for 1 document (1 cell is a probability of topic x for document y)
  theta <- t(apply(LDAData$document_sums + alpha, 2, function(x) x/sum(x)))
  # Topic to term distribution estimate
  # Matrix where each column contains the probability distribution over words for 1 topic (1 cell is a probability of word x for topic y)
  phi <- t(apply(t(LDAData$topics) + eta, 2, function(x) x/sum(x)))
  
  print("LDASimulation posteriors done.")
  print(proc.time() - timer)
  
  runData = list(LDAData = LDAData, 
                 usedTerms = names(usedTerms), 
                 termFrequency = as.integer(usedTerms), 
                 tokensPerDoc = tokensPerDoc, 
                 phi = phi, 
                 theta = theta, 
                 control = control, 
                 numberOfTopics = K)
  
  saveRDS(runData, gsub("__", Sys.time(), "data/LDA_modelfit__.rds"))
  
  print("LDASimulation data stored.")
  print(proc.time() - timer)
  
  return(runData)
}

TmLDASimulation <- function(corpus, folder, K, alpha, beta, burnin, iter, keep) {
  dtm = DocumentTermMatrix(corpus)
  
  control = list(alpha = alpha, delta = beta, burnin = burnin, iter = iter, keep = keep)
  #control = list(alpha = alpha, delta = beta, burnin = burnin, iter = iter, nstart = nstart, best = TRUE, seed = seed)
  
  print("TmLDASimulation setup done.")
  print(proc.time() - timer)
  
  LDAData = LDA(dtm, k = K, method = "Gibbs", control = control)
  
  print("TmLDASimulation run done.")
  print(proc.time() - timer)
  
  # Solution to translate between topicmodels and LDAVis: http://www.r-bloggers.com/a-link-between-topicmodels-lda-and-ldavis/
  # Document to topic distribution estimate
  # Matrix where each column contains the probability distribution over topics for 1 document (1 cell is a probability of topic x for document y)
  theta <- posterior(LDAData)$topics %>% as.matrix
  # Topic to term distribution estimate
  # Matrix where each column contains the probability distribution over words for 1 topic (1 cell is a probability of word x for topic y)
  phi <- posterior(LDAData)$terms %>% as.matrix
  
  print("TmLDASimulation posteriors done.")
  print(proc.time() - timer)
  
  usedTerms <- colnames(phi)
  
  tokensPerDoc <- vector()
  
  for (i in 1:length(corpus)) {
    tokensPerDoc <- c(tokensPerDoc, stri_count(paste(corpus[[i]]$content, collapse = ' '), regex = '\\S+'))
  }
  
  print("TmLDASimulation tokensPerDoc done.")
  print(proc.time() - timer)
  
  # Memory issue on cloud machine, do this locally
  #termFrequency <- colSums(as.matrix(dtm))
  
  print("TmLDASimulation termFrequency done.")
  print(proc.time() - timer)
  
  runData = list(LDAData = LDAData,
                 usedTerms = usedTerms,
                 dtm = dtm,
                 tokensPerDoc = tokensPerDoc,
                 posterior = list(phi = phi, theta = theta),
                 #termFrequency = termFrequency,
                 control = control,
                 numberOfTopics = K)
  
  saveRDS(runData, gsub("__", paste(Sys.time(), "alpha:", control$alpha, "beta:", control$delta, "topics:", K), paste("data", paste(folder, "TM_LDA_modelfit__.rds", sep = "/"), sep = "/")))
   
  print("TmLDASimulation data stored.")
  print(proc.time() - timer)
  
  return(runData)
}

visualise <- function(runData, outputFolder) {
  # Create JSON format data
  json.data <- createJSON(phi = runData$posterior$phi, 
                          theta = runData$posterior$theta, 
                          doc.length = runData$tokensPerDoc, 
                          vocab = runData$usedTerms, 
                          term.frequency = runData$termFrequency)
  
  # Remove the directory (throws error otherwise)
  unlink(outputFolder, recursive = TRUE)
  
  # Create browser scripts
  serVis(json.data, out.dir = outputFolder, open.browser = FALSE)
}