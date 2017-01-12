### FUNCTIONS ###
# readFileID(id, saliencyFile = false): 
  # reads TM_LDA or saliency_terms file from 'folder' with specific id

# wordDistribution(id, numWords = 10, colorTop = true): 
  # calculates and plots word distribution of the top 'numWords' most frequent words from DocumentTermMatrix in file with specific id
# overlap(ids):
  # calculates expected and actual overlap between files with specific ids, input as: c(1,2,3,...)
# myWordcloud(id):
  # creates and plots word cloud for file with specific id

# Kullback Leibler distances (see KL-distance.R)
# KLdists(ids):
  # calculates and prints the KL distances between files with specific ids, input as: c(1,2,3,...)
# KLColourMatrix(id1, id2):
  # creates a heatmap plot of KL distances between two files with specific ids 'id1' and 'id2'

# relevance(ids, numberOfTerms = 30)
  # calculates relevance for the 'numberOfTerms' most relevant words according to Sievert et. al. (see relevance.R and relevance-calc.R)
  # stores output in saliency_terms file
# calcImpression(id)
  # gets the relevancies from saliency_terms file with specific id, normalises relevancies to a maximum of 1, and prints the words and relevancies per topic
# impressionsToExcel(ids)
  # invokes calcImpression on files with specific ids and stores output in excel file, input as: c(1,2,3,...)
# impressionsToCSV(ids)
  # invokes calcImpression on files with specific ids and stores output in CSV file, input as: c(1,2,3,...)

# plotSplitMatrix(id1, id2, notes = true)
  # generates cross-occurrence map for files with specific ids 'id1' and 'id2', see topic-split-matrix.R
# plotOptimalSplitMatrix(id1, id2, notes = true)
  # generates optimalised cross-occurrence map for files with specific ids 'id1' and 'id2', see topic-split-matrix.R

# getOverview(ids)
  # prints a list of information (i.e. id, alpha, beta, topics) for files with specific ids, input as: c(1,2,3,...)

workspace <- "~/workspace/R"

setwd(workspace)

folder = "data/contrast_all"

readFileId <- function(id, saliencyFile = FALSE) {
  patt = "TM_LDA*"
  
  if(saliencyFile == TRUE) {
    patt = "saliency_terms*"
  }
  
  #print(id)
  #print(folder)
  #print(patt)
  
  file <- list.files(path = folder, pattern = paste("^", id, patt, sep = ""))
  #print(file)
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
  perc <- matrix(data = NA, nrow = length(ids), ncol = length(ids))
  expected <- matrix(data = NA, nrow = length(ids), ncol = length(ids))
  diff <- matrix(data = NA, nrow = length(ids), ncol = length(ids))
  
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

wordcloudByTopics <- function(id, topics, name = "wordcloud.png") {
  data <- readFileId(id, TRUE)
  
  selected <- data[data$Category %in% topics,]
  
  deduplicated <- aggregate(selected['Freq'], by = selected['Term'], sum)
  
  word_list <- deduplicated$Term
  freqs <- deduplicated$Freq
  
  library(wordcloud)
  
  pal <- brewer.pal(8,"Dark2")
  png(name, width = 1280,height = 800)
  wordcloud(word_list, freqs, scale = c(5,1), max.words = 100, random.order = F, rot.per = 0.1, colors = pal, vfont = c("sans serif","bold"))
  #wordcloud(d$word, d$freq, scale = c(5,1), min.freq = 10, max.words = 100, random.order = T, rot.per = 0.1, colors = pal, vfont = c("sans serif","bold"))
  dev.off()
}

myWordcloud <- function(id, name = "wordcloud.png") {
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
  png(name, width = 1280,height = 800)
  wordcloud(d$word, d$freq, scale = c(5,1), min.freq = 15, max.words = 100, random.order = F, rot.per = 0.1, colors = pal, vfont = c("sans serif","bold"))
  #wordcloud(d$word, d$freq, scale = c(5,1), min.freq = 10, max.words = 100, random.order = T, rot.per = 0.1, colors = pal, vfont = c("sans serif","bold"))
  dev.off()
}

KLdists <- function(ids) {
  source("postprocessing/KL-distance.R")
  
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
  source("postprocessing/KL-distance.R")
  
  KLData <- as.matrix(KLorder(KLdistFromIds(id2, id1)))

  library(gplots)
  
#   heatmap.2(x = KLData, cellnote = round(KLData, 2), col = colorRampPalette(c("white", "black"), bias = 10), 
#             srtCol = 0, labCol = seq(1,length(KLData[1,]),1), cexRow = 1, cexCol = 1, 
#             xlab = gsub("__", id1, "Model __, topics"), ylab = gsub("__", id2, "Model __, topics"),
#             Rowv = FALSE, Colv = FALSE, dendrogram = "none", notecol = "red", notecex = 1, trace = "none", key = FALSE, 
#             lhei = c(0.10, 0.90), lwid = c(0.10, 0.90), margins = c(3, 3.3))
  
  heatmap.2(x = KLData, col = colorRampPalette(c("white", "black"), bias = 10), 
            srtCol = 0, labCol = seq(1,length(KLData[1,]),1), cexRow = 1, cexCol = 1, 
            xlab = gsub("__", id1, "Model __, topics"), ylab = gsub("__", id2, "Model __, topics"),
            Rowv = FALSE, Colv = FALSE, dendrogram = "none", trace = "none", key = FALSE, 
            lhei = c(0.10, 0.90), lwid = c(0.10, 0.90), margins = c(3, 3.3))
}

relevance <- function(ids, numberOfTerms = 10) {
  source("postprocessing/relevance-calc.R")
  
  for (id in ids) {
    termRelevancyData <- relevanceCalculation(id, numberOfTerms, TRUE)
    
    #saveRDS(termSaliencyData, gsub("XX", id, paste(folder, "XXsaliency_terms.rds", sep = "/")))
  }
}

saliency <- function(ids, numberOfTerms = 10) {
  source("postprocessing/relevance-calc.R")
  timer <- proc.time()
  
  for (id in ids) {
    termSaliencyData <- relevanceCalculation(id, numberOfTerms)
    
    print("Storing data.")
    print(proc.time() - timer)
    
    saveRDS(termSaliencyData, gsub("XX", id, paste(folder, "XXsaliency_terms.rds", sep = "/")))
  }
}

impressionsToExcel <- function(ids) {
 # library(xlsx)
  
  for (id in ids) {
    data <- calcImpression(id)
    
    filename <- gsub("XX", id, paste(folder, "XXrelevancies.csv", sep = "/"))
    
    if (file.exists(filename)) {
      file.remove(filename)
    }
    
    #write.xlsx(data, file = filename, sheetName = paste("sheet", id), append = TRUE)
    write.table(data, file = filename, row.names = FALSE)
  }
}

calcImpression <- function(id) {
  source("postprocessing/phi-calculations.R")
  source("postprocessing/theta-calculations.R")
  
  data <- readFileId(id, saliencyFile = TRUE)
  
  size <- length(data[,1]) # number of words
  topics <- max(data[,2]) # number of topics
  chunkSize <- size/topics
  
  relevanceCols <- data[,grepl("relevance", colnames(data))]
  
  recalcRelevance <- apply(relevanceCols, 2, function(x) { max(x) / x })
  
  rels <- matrix(data = NA, nrow = length(recalcRelevance[,1]), ncol = 1)
  colCount = 1
  
  for (i in 1:length(recalcRelevance[,1])) {
    rels[i] = recalcRelevance[i, colCount]
    
    if(i %% chunkSize == 0) {
      colCount = colCount + 1
    }
  }
  
  # Take the text, bind the relevancies
  #impData <- data[,c(1,2)]
  impData <- cbind(data[,c(1,2)], rels)
  
  # Split the data into chunks and put into frame
  impression <- data.frame(split(impData, impData$Category))
  
  impressionFilter <- impression[, -grep("Category", colnames(impression))]
  
  return(impressionFilter)
}

impressionsToCSV <- function(ids) {
  for (i in 1:length(ids)) {
    id <- ids[i]
    
    data <- calcImpression(id)
    
    write.csv(data, file = paste(folder, paste(id, "impression.csv", sep = ""), sep = "/"))
  }
}

plotSplitMatrix <- function(id1, id2, notes = TRUE) {
  source("postprocessing/topic-split-matrix.R")
  
  model1 <- readFileId(id1, TRUE)
  model2 <- readFileId(id2, TRUE)
  
  models <- findBiggestModel(model1, model2)
  intersection <- topicSplitMatrix(model1, model2)
  
  plotMatrix(intersection, models, notes)
}

plotOptimalSplitMatrix <- function(id1, id2, notes = TRUE) {
  source("postprocessing/topic-split-matrix.R")
  
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