folder = "data/ovid"

readFileId <- function(id, saliencyFile = FALSE) {
  patt = "TM_LDA*"
  
  if(saliencyFile == TRUE) {
    patt = "saliency_terms*"
  }
  
  file <- list.files(folder, paste(id, patt, sep = ""))
  
  return(readRDS(paste(folder, file, sep = "/")))
}

KLdists <- function(ids) {
  source("KL-distance.R")
  
  container <- list()
  
  for (i in 1:length(ids)) {
    file1 <- readFileId(ids[i])
    
    orderedRes <- list()
    
    for (j in i:length(ids)) {
      file2 <- readFileId(ids[j])
      
      res <- KLdistFromRunResults(file1, file2, minimialise = FALSE)
      
      orderedRes[[j]] <- KLorder(res)
    }
    
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
  
  print(ids)
  
  for (i in 1:length(ids)) {
    print("Reading file.")
    print(proc.time() - timer)
    
    id <- ids[i]
    data <- readFileId(id)
    
    # Remove a bunch of sparse terms
    sparseMatrix = removeSparseTerms(data$dtm, sparse = 0.99)
    
    print("Calculating term frequencies.")
    print(proc.time() - timer)
    
    # Term frequency gave memory issues on cloud machine, calculate it here
    data$termFrequency = colSums(as.matrix(sparseMatrix))
    
    usedTerms = colnames(sparseMatrix)
    phi = data$posterior$phi[][, colnames(data$posterior$phi) %in% colnames(sparseMatrix)]
    
    print("Calculating relevances.")
    print(proc.time() - timer)
    
    termSaliencyData <- salientTerms(phi = phi, theta = data$posterior$theta, 
                                     vocab = usedTerms, doc.length = data$tokensPerDoc, 
                                     term.frequency = data$termFrequency, R = numberOfTerms)
    
    print("Storing data.")
    print(proc.time() - timer)
    
    saveRDS(termSaliencyData, gsub("XX", id, paste(folder, "XXsaliency_terms.rds", sep = "/")))
  }
}

calcImpression <- function(id) {
  data <- readFileId(id, saliencyFile = TRUE)
  
  size <- length(data[,1]) # number of words
  topics <- max(data[,2]) # number of topics
  chunkSize <- size/topics
  
  # Take only the text, they are already sorted on relevance/saliency
  impData <- data[,1]
  
  # Split the data into chunks and put into frame
  impression <- data.frame(split(impData, ceiling(seq_along(impData) / chunkSize)))
  
  return(impression)
}

# docsToTopics <- function(id) {
#   file <- list.files("data", paste(id, "saliency_terms*", sep = ""))
#   
#   data <- readRDS(paste("data", file, sep = "/"))
#   
#   return(as.matrix(topics(data$LDAData)))
# }

topicSplitMatrix <- function(id1, id2) {
  getTopicIntersect <- function(model1, model2, topicId) {
    match <- intersect(model1[model1$Category == topicId,]$Term, model2$Term)
    
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
  
  intersection[intersection > length(unique(smallest$Category))] = -2
  
  size <- length(biggest$Term) # number of words
  topics <- length(unique(biggest$Category)) # number of topics
  chunkSize <- size/topics
  
  # Split intersection into 1 topic per column based on chunkSize
  splitIntersection <- as.matrix(data.frame(split(intersection, ceiling(seq_along(intersection) / chunkSize))))
  # Split terms into 1 topic per column based on chunkSize
  splitTerms <- as.matrix(data.frame(split(biggest$Term, ceiling(seq_along(biggest$Term) / chunkSize))))
  
  library(gplots)
  
  colorMap = c("gold", "white", "aquamarine", "azure3", "bisque3", "blue", "blueviolet", "brown2", "cadetblue", "chartreuse3", "chocolate1", "darkgoldenrod4")
  
  nCats = length(unique(smallest$Category))
  nTopics = length(unique(biggest$Category))
  
  heatmap.2(x = splitIntersection, cellnote = splitTerms,                                                               # Intersection and term data
            labCol = seq(1, nTopics, 1), xlab = "Topics", ylab = "Words", cexRow = 1, cexCol = 1, srtCol = 0,           # Adjust label text, size, and positioning
            col = colorMap[1:(nCats + 3)], breaks = -3:(nCats),                                                         # Cell colors and color break values (when colors swap)
            Rowv = FALSE, Colv = FALSE, dendrogram = "none", notecol = "black", notecex = 1, trace = "none", key = TRUE # Cleanup of plot
           )
}

getOverview <- function(ids) {
  print("############################################")
  print(paste("# Printing overview of ids: ", paste(ids, collapse = ",")))
  print("############################################")
  
  for (i in 1:length(ids)) {
    id <- ids[i]
    data <- readFileId(id)
    
    print(paste("id: ", id, ", alpha: ", round(data$control$alpha, 2), " = (1/k?: ", eval((1/as.integer(data$numberOfTopics)) == data$control$alpha),
                "), beta: ", data$control$delta, ", topics: ", data$numberOfTopics, sep = ""))
  }
}