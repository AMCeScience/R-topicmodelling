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
  
  ids <- paste(ids, collapse = "|")
  
  files <- list.files("data", paste("(", ids, ")TM_LDA*", sep = ""))
  
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

KLColourMatrix <- function(id1, id2) {
  source("KL-distance.R")
  
  KLData <- as.matrix(KLorder(KLdistFromIds(id1, id2)))
  
  library(gplots)
  
  heatmap.2(x = KLData, cellnote = round(KLData, 2), col = colorRampPalette(c("white", "black"), bias = 10),
            Rowv = FALSE, Colv = FALSE, dendrogram = "none", notecol = "red", notecex = 1,
            trace = "none", key = TRUE, margins = c(7, 7))
}

relevance <- function(ids, numberOfTerms = 30) {
  # Can also get the raw relevancy instead of saliency
  # as.matrix(terms(LDAData, numberOfTerms))
  
  source("relevance.R")
  timer <- proc.time()
  
  ids <- paste(ids, collapse = "|")
  print(ids)
  files <- list.files("data", paste("(", ids, ")TM_LDA*", sep = ""))
  
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

topicSplitMatrix <- function(id1, id2) {
  getTopicIntersect <- function(model1, model2, topicId) {
    match <- intersect(model1[model1$Category == topicId,]$Term, model2$Term)
    
    # Get only the most relevant?
    # model1[model1$Category == topicId,]$Term
    # model1[model1$Category == 1 & model1[,4 + topicId] == apply(model1[grep("relevance*", names(model1))], 1, max),]$Term
    
    return(as.integer(model2$Term %in% match) * topicId)
  }
  
  getTopicDifference <- function(model1, model2) {
    return(as.integer(!model2$Term %in% intersect(model1$Term, model2$Term)) * -1)
  }
  
  getModelIntersect <- function(model1, model2) {
    loopa <- model1
    loopb <- model2
    
    if(length(unique(model1$Category)) > length(unique(model2$Category))) {
      loopa <- model2
      loopb <- model1
    }
    
    container <- seq(from = 0, to = 0, length.out = length(loopb$Term))
    
    for(i in 1:length(unique(loopa$Category))) {
      container <- container + getTopicIntersect(loopa, loopb, i)
      
      # Give shared items a different colour
      container[container > i] = length(unique(loopa$Category)) + 1
    }
    
    #container <- as.integer(loopb$Term %in% intersect(loopa$Term, loopb$Term)) * loopa$Category
    
    container <- container + getTopicDifference(loopa, loopb)
    
    return(container)
  }
  
  model1 <- readFileId(id1, TRUE)
  model2 <- readFileId(id2, TRUE)
  
  biggest <- model2
  smallest <- model1
  
  if(length(unique(model1$Category)) > length(unique(model2$Category))) {
    biggest <- model1
    smallest <- model2
  }
  
  intersection <- getModelIntersect(smallest, biggest)
  
  size <- length(biggest$Term) # number of words
  topics <- length(unique(biggest$Category)) # number of topics
  chunkSize <- size/topics
  
  # Split intersection into 1 topic per column based on chunkSize
  splitIntersection <- as.matrix(data.frame(split(intersection, ceiling(seq_along(intersection) / chunkSize))))
  # Split terms into 1 topic per column based on chunkSize
  splitTerms <- as.matrix(data.frame(split(biggest$Term, ceiling(seq_along(biggest$Term) / chunkSize))))
  
  library(gplots)
  
  heatmap.2(x = splitIntersection, cellnote = splitTerms,
            col = c("burlywood", 0, 2:(length(unique(smallest$Category)) + 2)), breaks = -2:(length(unique(smallest$Category)) + 1),
            Rowv = FALSE, Colv = FALSE, dendrogram = "none", notecol = "black", notecex = 1,
            trace = "none", key = TRUE, margins = c(7, 7))
}

getOverview <- function(ids) {
  ids <- paste(ids, collapse = "|")
  
  files <- list.files("data", paste("(", ids, ")TM_LDA*", sep=""))
  
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