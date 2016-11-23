data <- readFileId(3)

docs <- data$posterior$theta

ordered_docs <- docs[order(docs[,1], decreasing = TRUE)]

plot(ordered_docs, type = "l", xlab = "", ylab = "")
title(ylab = "Topic to document probability", line = 2.2, cex.lab = 1)
title(xlab = "Ordered document index", line = 2.1, cex.lab = 1)

for (i in 2:ncol(docs)) {
  this_color = "black"
  
  if (i == 2 || i == 17) {
    this_color = "blue"
  }
  
  if (i == 3 || i == 18) {
    this_color = "red"
  }
  
  col <- docs[,i]
  
  ordered_doc <- col[order(col, decreasing = TRUE)]
  lines(ordered_doc, col = this_color)
}


topicSurface <- 100
wordSurface <- 75
wordCount <- 20

filename <- "relevance_all.xlsx"

id <- 3

################################
# Remove topics
################################

source("postprocessing/theta-calculations.R")

data <- readFileId(id)

theta_rels <- getRawTheta(id)
num_topics <- getTopics(theta_rels$theta, topicSurface)

topic_ids <- theta_rels$category[1:num_topics]

source("postprocessing/postprocessing.R")

saliency(id, 30)

data <- readFileId(id, saliencyFile = TRUE)

size <- length(data[,1]) # number of words
topics <- length(unique(data[,2])) # number of topics
chunkSize <- size/topics

relevance <- data[,grepl("relevance", colnames(data))]

rels <- matrix(data = NA, nrow = length(relevance[,1]), ncol = 1)
colCount = 1

for (i in 1:length(relevance[,1])) {
  rels[i] = relevance[i, colCount]
  
  if(i %% chunkSize == 0) {
    colCount = colCount + 1
  }
}

# Take the text, bind the relevancies
relevance_data <- cbind(data[,c(1,2)], rels)

topics_removed <- relevance_data[relevance_data$Category %in% topic_ids,]

# all_topics <- 1:max(relevance_data$Category)
# 
# remove_topics <- setdiff(all_topics, topic_ids)
# 
# patts <- c()
# 
# for(i in remove_topics) {
#   patts <- c(patts, paste("relevance.", i, sep = ""))
# }
# 
# topics_removed <- cut_horz[ , !(names(cut_horz) %in% patts)]

################################
# Remove words
################################

source("postprocessing/phi-calculations.R")

#num_words <- wordsPerTopic(id, wordSurface)
num_words <- rep(wordCount, num_topics)

begin <- 1
end <- 0

cut_vert <- topics_removed

for (i in 1:length(num_words)) {
  begin <- begin + num_words[i]
  end <- begin + (nrow(topics_removed[topics_removed$Category == topic_ids[i],]) - (num_words[i] + 1))
    
  print(paste(begin, end))
  
  cut_vert <- cut_vert[-(begin:end),]
}

words_removed <- cut_vert

################################
# Build impression
################################

data <- words_removed

# size <- length(data[,1]) # number of words
# topics <- length(unique(data[,2])) # number of topics
# chunkSize <- size/topics
# 
# relevanceCols <- data[,grepl("relevance", colnames(data))]
# 
# recalcRelevance <- apply(relevanceCols, 2, function(x) { max(x) / x })
# 
# rels <- matrix(data = NA, nrow = length(recalcRelevance[,1]), ncol = 1)
# colCount = 1
# 
# for (i in 1:length(recalcRelevance[,1])) {
#   rels[i] = recalcRelevance[i, colCount]
#   
#   if(i %% chunkSize == 0) {
#     colCount = colCount + 1
#   }
# }
# 
# # Take the text, bind the relevancies
# #impData <- data[,c(1,2)]
# impData <- cbind(data[,c(1,2)], rels)

# Split the data into chunks and put into frame
impression <- data.frame(split(data, data$Category))

impressionFilter <- impression[, -grep("Category", colnames(impression))]
impressionFilter <- impressionFilter[, -grep("rels", colnames(impressionFilter))]

if (file.exists(filename)) {
  file.remove(filename)
}

write.xlsx(impressionFilter, file = filename, sheetName = "sheet", append = TRUE)