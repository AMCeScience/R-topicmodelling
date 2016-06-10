workspace <- "~/workspace/R"

setwd(workspace)

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
  
  library(tm)
  
  tdm <- as.TermDocumentMatrix(data$dtm)
  
  m <- as.matrix(tdm)
  v <- sort(rowSums(m), decreasing = TRUE)
  
  plot(v[seq(1, numWords)], xlab = "", ylab = "")
  title(xlab = "Word Rank", line = 2.1, cex.lab = 1)
  title(ylab = "Frequency", line = 2.2, cex.lab = 1)
  
  if (colorTop == TRUE) {
    points(x = 1, y = v[1], col = "red", pch = 19)
    points(x = 2, y = v[2], col = "red", pch = 19)
    abline(v=100)
  }
}

overlap <- function(ids) {
  perc <- matrix(, nrow = length(ids), ncol = length(ids))
  expected <- matrix(, nrow = length(ids), ncol = length(ids))
  diff <- matrix(, nrow = length(ids), ncol = length(ids))
  
  run = length(ids)
  
  for (i in 1:(run - 1)) {
    id1 <- ids[i]
  
    data1 <- readFileId(id1, TRUE)
      
    start = i + 1
      
    perc[i, i] = 1
    
    if (start > run) {
      next
    }
    
    for (j in start:run) {
      perc[j, j] = 1
      
      id2 <- ids[j]
      
      data2 <- readFileId(id2, TRUE)
      
      inter = length(intersect(data1$Term, data2$Term))
      
      setSize = length(unique(data1$Term)) + length(unique(data2$Term))
      
      percentage =  (2 * inter) / setSize
      
      perc[i, j] = percentage
      perc[j, i] = percentage
      
      expected[i, j] = (2 * length(unique(data1$Term))) / setSize
      expected[j, i] = (2 * length(unique(data1$Term))) / setSize
      
      diff[i, j] = expected[i, j] - perc[i, j]
      diff[j, i] = expected[j, i] - perc[j, i]
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
  
  KLData <- as.matrix(KLorder(KLdistFromIds(id2, id1)))

  library(gplots)
  
  dev.off()
  heatmap.2(x = KLData, cellnote = round(KLData, 2), col = colorRampPalette(c("white", "black"), bias = 10), 
            srtCol = 0, labCol = seq(1,length(KLData[1,]),1), cexRow = 1, cexCol = 1, xlab = "Model 1, topics", ylab = "Model 2, topics",
            Rowv = FALSE, Colv = FALSE, dendrogram = "none", notecol = "red", notecex = 1, trace = "none", key = FALSE, 
            lhei = c(0.05, 0.95), lwid = c(0.05, 0.95), margins = c(3, 3.3))
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

impressionsToExcel <- function(ids) {
  library(xlsx)
  
  for (id in ids) {
    data <- calcImpression(id)
    
    write.xlsx(data, file="relevancies.xlsx", sheetName=paste("sheet", id), append=T)
  }
}

calcImpression <- function(id) {
  data <- readFileId(id, saliencyFile = TRUE)
  
  size <- length(data[,1]) # number of words
  topics <- max(data[,2]) # number of topics
  chunkSize <- size/topics
  
  relevanceCols <- data[,grepl("relevance", colnames(data))]
  
  recalcRelevance <- apply(relevanceCols, 2, function(x) { max(x) / x })
  
  rels <- matrix(, nrow = length(recalcRelevance[,1]), ncol = 1)
  colCount = 1
  
  for (i in 1:length(recalcRelevance[,1])) {
    rels[i] = recalcRelevance[i, colCount]
    
    if(i %% 30 == 0) {
      colCount = colCount + 1
    }
  }
  
  # Take only the text, they are already sorted on relevance/saliency
  impData <- cbind(data[,c(1,2)], rels)
  
  # Split the data into chunks and put into frame
  impression <- data.frame(split(impData, impData$Category))
  
  impressionFilter <- impression[, -grep("Category", colnames(impression))]
  
  return(impressionFilter)
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

perplexityPlot <- function(id) {
  perps <- readRDS(paste(folder, paste("perplexity", id, ".rds", sep = ""), sep = "/"))
  
  # Get perps somewhere
  meanPerps <- rowMeans(perps[2:length(perps)])
  
  plot(perps[,1], meanPerps)
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