readFileId <- function(id, saliencyFile = FALSE) {
  patt = "TM_LDA*"
  
  if(saliencyFile == TRUE) {
    patt = "saliency_terms*"
  }
  
  file <- list.files("data", paste(id, patt, sep = ""))
  
  return(readRDS(paste("data", file, sep = "/")))
}

KLdists <- function(ids) {
  source("KL-distance.R")
  
  ids <- paste(ids, collapse = ",")
  
  files <- list.files("data", paste("[", ids, "]TM_LDA*"))
  
  unorderedContainer <- list()
  container <- list()
  
  for (i in 1:length(files)) {
    file1 <- readRDS(paste("data", files[i], sep = "/"))
    
    orderedRes <- list()
    unorderedRes <- list()
    
    for (j in i:length(files)) {
      file2 <- readRDS(paste("data", files[j], sep = "/"))
      
      res <- KLdistFromRunResults(file1, file2, minimialise = FALSE)
      
      unorderedRes[[j]] <- res
      orderedRes[[j]] <- KLorder(res)
    }
    
    #unorderedContainer[[i]] <- unorderedRes
    container[[i]] <- orderedRes
  }
  
  return(container)
}

relevance <- function(ids, numberOfTerms = 30) {
  # Can also get the raw relevancy instead of saliency
  # as.matrix(terms(LDAData, numberOfTerms))
  
  source("relevance.R")
  timer <- proc.time()
  
  ids <- paste(ids, collapse = ",")
  
  files <- list.files("data", paste("[", ids, "]TM_LDA*"))
  
  for (i in 1:length(files)) {
    print("Reading file.")
    print(proc.time() - timer)
    
    data <- readRDS(paste("data", files[i], sep = "/"))
    id <- substr(files[i], 1, 1)
    
    if(is.null(data$termFrequency)) {
      print("Calculating term frequencies.")
      print(proc.time() - timer)
      
      # Term frequency gave memory issues on cloud machine, calculate it here
      data$termFrequency = colSums(as.matrix(data$dtm))
      
      saveRDS(data, paste("data", files[i], sep = "/"))
    }
    
    print("Calculating relevances.")
    print(proc.time() - timer)
    
    termSaliencyData <- salientTerms(phi = data$posterior$phi, theta = data$posterior$theta, 
                                     vocab = data$usedTerms, doc.length = data$tokensPerDoc, 
                                     term.frequency = data$termFrequency, R = numberOfTerms)
    
    print("Storing data.")
    print(proc.time() - timer)
    
    saveRDS(termSaliencyData, gsub("XX", id, "data/XXsaliency_terms.rds"))
  }
}

calcImpression <- function(id) {
  file <- list.files("data", paste(id, "saliency_terms*", sep = ""))
  
  data <- readRDS(paste("data", file, sep = "/"))
  
  size <- length(data[,1]) # number of words
  topics <- max(data[,2]) # number of topics
  chunkSize <- size/topics
  
  # Take only the text, they are already sorted on relevance/saliency
  impData <- data[,1]
  
  # Split the data into chunks and put into frame
  impression <- data.frame(split(impData, ceiling(seq_along(impData) / chunkSize)))
  
  return(impression)
}

docsToTopics <- function(id) {
  file <- list.files("data", paste(id, "saliency_terms*", sep = ""))
  
  data <- readRDS(paste("data", file, sep = "/"))
  
  return(as.matrix(topics(data$LDAData)))
}

getOverview <- function(ids) {
  ids <- paste(ids, collapse = ",")
  
  files <- list.files("data", paste("[", ids, "]TM_LDA*"))
  
  print("############################################")
  print(paste("# Printing overview of ids: ", paste(ids, collapse = ",")))
  print("############################################")
  
  for (i in 1:length(files)) {
    file <- files[i]
    id <- substr(files[i], 1, 1)
    data <- readRDS(paste("data", files[i], sep = "/"))
    
    #alpha <- str_match(files[i], "alpha:\ ([0-9.]+)")[,2]
    #beta <- str_match(files[i], "beta:\ ([0-9.]+)")[,2]
    #topics <- str_match(files[i], "topics:\ ([0-9]+)")[,2]
    
    print(paste("id: ", id, ", alpha: ", data$control$alpha, " = (1/k?: ", eval((1/as.integer(data$numberOfTopics)) == data$control$alpha),
                "), beta: ", data$control$delta, ", topics: ", data$numberOfTopics, sep = ""))
  }
}