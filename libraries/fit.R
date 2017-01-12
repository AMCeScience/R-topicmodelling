################
# RETURN VALUE #
################

# System date
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

LDASimulation <- function(corpus, folder, k, alpha, beta, burnin, iter, thin, keep, store = TRUE) {
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
  
  control = list(alpha = alpha, delta = beta, burnin = burnin, iter = iter, thin = thin)
  
  print("LDASimulation setup done.")
  
  # Run LDA
  LDAData <- lda.collapsed.gibbs.sampler(documents = LDADocuments, 
                                         k = k, 
                                         vocab = names(usedTerms), 
                                         num.iterations = control$iter, 
                                         alpha = control$alpha, 
                                         eta = control$delta, 
                                         burnin = control$burnin,
                                         compute.log.likelihood = TRUE)
  
  print("LDASimulation run done.")
  
  # Document to topic distribution estimate
  # Matrix where each column contains the probability distribution over topics for 1 document (1 cell is a probability of topic x for document y)
  theta <- t(apply(LDAData$document_sums + alpha, 2, function(x) x/sum(x)))
  # Topic to term distribution estimate
  # Matrix where each column contains the probability distribution over words for 1 topic (1 cell is a probability of word x for topic y)
  phi <- t(apply(t(LDAData$topics) + beta, 2, function(x) x/sum(x)))
  
  print("LDASimulation posteriors done.")
  
  runData = list(LDAData = LDAData, 
                 usedTerms = names(usedTerms), 
                 termFrequency = as.integer(usedTerms), 
                 tokensPerDoc = tokensPerDoc, 
                 phi = phi, 
                 theta = theta, 
                 control = control, 
                 numberOfTopics = k)
  
  if (store == TRUE) {
    saveRDS(runData, gsub("__", paste(Sys.time(), "alpha:", control$alpha, "beta:", control$delta, "topics:", k), paste("data", folder, "TM_LDA_modelfit__.rds", sep = "/")))
    
    print("LDASimulation data stored.")
  }
  
  return(runData)
}

TmLDASimulation <- function(corpus, project_name, file_version, k, alpha, beta, burnin, iter, thin, keep, multiple = FALSE) {
  dtm = DocumentTermMatrix(corpus)
  
  if (multiple) {
    control = list(alpha = alpha, delta = beta, iter = iter, keep = 1, nstart = 3, best = FALSE, seed = list(123234, 890, 112))
  } else {
    control = list(alpha = alpha, delta = beta, burnin = burnin, iter = iter, keep = keep, thin = thin)
  }
  
  print("TmLDASimulation setup done.")
  
  LDAData = LDA(dtm, k = k, method = "Gibbs", control = control)
  
  print("TmLDASimulation run done.")
  
  if (multiple) {
    return(LDAData)
  }
  
  # Solution to translate between topicmodels and LDAVis: http://www.r-bloggers.com/a-link-between-topicmodels-lda-and-ldavis/
  # Document to topic distribution estimate
  # Matrix where each column contains the probability distribution over topics for 1 document (1 cell is a probability of topic x for document y)
  theta <- posterior(LDAData)$topics %>% as.matrix
  # Topic to term distribution estimate
  # Matrix where each column contains the probability distribution over words for 1 topic (1 cell is a probability of word x for topic y)
  phi <- posterior(LDAData)$terms %>% as.matrix
  
  print("TmLDASimulation posteriors done.")
  
  usedTerms <- colnames(phi)
  
  tokensPerDoc <- vector()
  
  for (i in 1:length(corpus)) {
    tokensPerDoc <- c(tokensPerDoc, stri_count(paste(corpus[[i]]$content, collapse = ' '), regex = '\\S+'))
  }
  
  print("TmLDASimulation tokensPerDoc done.")
  
  # Memory issue on cloud machine, do this locally
  #termFrequency <- colSums(as.matrix(dtm))
  
  print("TmLDASimulation termFrequency done.")
  
  runData = list(
    date = Sys.time(),
    LDAData = LDAData,
    usedTerms = usedTerms,
    dtm = dtm,
    tokensPerDoc = tokensPerDoc,
    posterior = list(phi = phi, theta = theta),
    #termFrequency = termFrequency,
    control = control,
    numberOfTopics = k
  )
  
  if (fit_store == TRUE) {
    filename <- paste(data_folder, "/", project_name, "/LDA_fit_k", k, "_a", round(alpha, 2), "_b", round(beta, 2), "_", file_version, ".rds", sep = "")
    
    saveRDS(runData, filename)
    
    print("TmLDASimulation data stored.")
  }
  
  return(runData)
}

visualise <- function(runData, outputFolder) {
  runData$termFrequency = colSums(as.matrix(runData$dtm))
  
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