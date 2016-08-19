extractRelevancies <- function(id, numWords) {
  source("postprocessing/relevance-calc.R")
  
  rels <- relevanceCalculation(id, numWords, TRUE)
  
  size <- length(rels[,1]) # number of words
  topics <- max(rels[,2]) # number of topics
  chunkSize <- size/topics
  
  relevanceCols <- rels[,grepl("relevance", colnames(rels))]
  
  #recalcRelevance <- apply(relevanceCols, 2, function(x) { max(x) / x })
  
  chuncked_rels <- matrix(data = NA, nrow = length(relevanceCols[,1]), ncol = 1)
  colCount = 1
  
  for (i in 1:length(relevanceCols[,1])) {
    chuncked_rels[i] = relevanceCols[i, colCount]
    
    if(i %% chunkSize == 0) {
      colCount = colCount + 1
    }
  }
  
  impData <- cbind(rels[,c(1,2)], chuncked_rels)
  
  impression <- data.frame(split(impData, impData$Category))
  
  impressionFilter <- impression[, -grep("Category", colnames(impression))]
}

wordsPerTopic <- function(id, goal_surface) {
  data <- readFileId(id)
  
  words <- c()
  
  for (i in 1:data$numberOfTopics) {
    rels <- getSpecificTopic(id, i)
    words <- c(words, getWords(rels, goal_surface))
  }
  
  words_per_topic <- data.frame(topic = seq_len(data$numberOfTopics), numWords = words)
}

getSpecificTopic <- function(id, topic_num) {
  rels <- extractRelevancies(id, 50)
  
  return(rels[, (topic_num * 2)])
}

getWords <- function(rels, goal_surface) {
  total <- getSurface(rels, length(rels))
  
  curr_surface <- 0
  last_surface <- 0
  numWords <- 1
  
  while (curr_surface < goal_surface) {
    last_surface <- curr_surface
    
    curr_surface <- getSurface(rels, numWords)
    
    numWords <- numWords + 1
  }
  
  curr_diff <- abs(curr_surface - goal_surface)
  last_diff <- abs(last_surface - goal_surface)
  
  if (curr_diff < last_diff) {
    return(numWords - 1)
  } else {
    return(numWords - 2)
  }
}

getSurface <- function(rels, numWords) {
  normalised <- normalise(rels)
  
  total_area <- getArea(normalised, length(normalised))
  
  partial_area <- getArea(normalised, numWords)
  
  return((partial_area / total_area) * 100)
}

getArea <- function(normalised_rels, numWords) {
  used_rels <- normalised_rels[1:numWords]
  
  height <- (used_rels[-1] + used_rels[-length(used_rels)]) / 2
  width <- -diff(1:length(used_rels))
  return(sum(height * width))
}

plotRels <- function(rels) {
  normalised <- normalise(rels)
  
  plot(normalised, type = "l")
}

normalise <- function(rels) {
  normalised <- rels - min(rels)
  
  return(normalised[order(normalised, decreasing = TRUE)])
}