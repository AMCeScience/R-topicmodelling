folder = "data/ovid/19/ovid_latest"

readFileId <- function(id, saliencyFile = FALSE) {
  patt = "TM_LDA*"
  
  if(saliencyFile == TRUE) {
    patt = "saliency_terms*"
  }
  
  file <- list.files(folder, paste(id, patt, sep = ""))
  
  return(readRDS(paste(folder, file, sep = "/")))
}

wordDistribution <- function(id, numWords = 10, colorTop = TRUE) {
  data <- readFileId(id)
  
  tdm <- as.TermDocumentMatrix(data$dtm)
  
  m <- as.matrix(tdm)
  v <- sort(rowSums(m), decreasing = TRUE)
  
  plot(v[seq(1, numWords)], xlab = "", ylab = "")
  title(xlab = "Word Index", line = 2.1, cex.lab = 1)
  title(ylab = "Frequency", line = 2.2, cex.lab = 1)
  
  if (colorTop == TRUE) {
    points(x = 1, y = v[1], col = "red", pch = 19)
    points(x = 2, y = v[2], col = "red", pch = 19)
    abline(v=100)
  }
}

overlap <- function(ids) {
  perc = 0
  
  run = length(ids) - 1
  
  for (i in 1:run) {
    id1 <- ids[i]
  
    data1 <- readFileId(id1, TRUE)
      
    start = i + 1
    
    if (start > run) {
      next
    }
    
    for (j in start:run) {
      print(i)
      print(j)
      id2 <- ids[j]
      
      data2 <- readFileId(id2, TRUE)
      
      overlappingWords <- data2$Term[data2$Term %in% data1$Term]
      
      percentage = length(overlappingWords) / length(data2$Term)
      
      perc = (perc + percentage) / 2
    }
  }
  
  return(perc)
}

myWordcloud <- function(id) {
  data <- readFileId(id)

  dtm2list <- apply(data$dtm, 1, function(x) {
    paste(rep(names(x), x), collapse = " ")
  })
  
  myCorp <- VCorpus(VectorSource(dtm2list))
  
  library(stringr)
  
  clean <- tm_map(myCorp, function(document) {
    PlainTextDocument(str_replace_all(str_replace_all(document$content, "data", ""), "big", ""))
  })
    
  tdm <- TermDocumentMatrix(clean)
  
  m <- as.matrix(tdm)
  v <- sort(rowSums(m), decreasing=TRUE)
  d <- data.frame(word = names(v),freq=v)
  
  library(wordcloud)
  
  pal <- brewer.pal(8,"Dark2")
  png("wordcloud.png", width = 1280,height = 800)
  wordcloud(d$word, d$freq, scale = c(5,1), min.freq = 10, max.words = 100, random.order = F, rot.per = 0.1, colors = pal, vfont = c("sans serif","bold"))
  #wordcloud(d$word, d$freq, scale = c(5,1), min.freq = 10, max.words = 100, random.order = T, rot.per = 0.1, colors = pal, vfont = c("sans serif","bold"))
  dev.off()
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