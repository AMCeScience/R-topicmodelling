folder = "data/ovid/19/ovid_latest"

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
  
  source("relevance-calc.R")
  timer <- proc.time()
  
  print(ids)
  
  for (i in 1:length(ids)) {
    termSaliencyData <- relevanceCalculation(id, numberOfTerms)
    
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

storeImpressions <- function(ids) {
  for (i in 1:length(ids)) {
    id <- ids[i]
    
    data <- calcImpression(id)
    
    write.csv(data, file = paste(folder, paste(id, "impression.csv", sep = ""), sep = "/"))
  }
}

plotSplitMatrix <- function(id1, id2, notes = TRUE) {
  source("topic-split-matrix.R")
  
  model1 <- readFileId(id1, TRUE)
  model2 <- readFileId(id2, TRUE)
  
  models <- findBiggestModel(model1, model2)
  intersection <- topicSplitMatrix(model1, model2)
  
  plotMatrix(intersection, models, notes)
}

plotOptimalSplitMatrix <- function(id1, id2, notes = TRUE) {
  source("topic-split-matrix.R")
  
  model1 <- readFileId(id1, TRUE)
  model2 <- readFileId(id2, TRUE)
  
  models <- findBiggestModel(model1, model2)
  intersection <- optimaliseTopicIntersection(id1, id2)
  
  plotMatrix(intersection, models, notes)
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