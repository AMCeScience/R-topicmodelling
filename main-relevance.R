source("relevance.R")

files <- list.files("data", "TM_LDA_modelfit2015-11-17*")

for (i in 1:length(files)) {
  data <- readRDS(paste("data", files[i], sep = "/"))
  
  # Term frequency gave memory issues on cloud machine, calculate it here
  data$termFrequency = colSums(as.matrix(data$dtm))
  
  json <- salientTerms(phi = data$posterior$phi, theta = data$posterior$theta, vocab = data$usedTerms, doc.length = data$tokensPerDoc, term.frequency = data$termFrequency)
  
  saveRDS(json, gsub("__", paste("alpha:", data$control$alpha, "beta:", data$control$delta, "topics:", data$numberOfTopics), "data/salient_terms__.rds"))
}