topicSplitMatrix <- function(model1, model2) {
  # Get a vector of integers denoting the intersection of a certain topic in model1 as compared to model2
  # The returned integer is equal to the topicId
  getTopicIntersect <- function(model1, model2, topicId) {
    match <- intersect(model1[model1$Category == topicId,]$Term, model2$Term)
    
    return(as.integer(model2$Term %in% match) * topicId)
  }
  
  # Get a vector of integers denoting the difference between all topics in model2 as compared to model1
  getTopicDifference <- function(model1, model2) {
    return(as.integer(!model2$Term %in% intersect(model1$Term, model2$Term)) * -1)
  }
  
  # Calculate vector of integers
  # Representing topics from model1 that appear in model2
  getModelIntersect <- function(loopa, loopb) {
    # Initialise container with zeroes
    container <- seq(from = 0, to = 0, length.out = length(loopb$Term))
    
    # Loop the topics
    for(i in 1:length(unique(loopa$Category))) {
      # Give items in model2 the topic number of model1 based on intersection
      container <- container + getTopicIntersect(loopa, loopb, i)
      
      # Give shared items a different colour
      container[container > i] = -2
    }
    
    # Give new items a different colour
    container <- container + getTopicDifference(loopa, loopb)
    
    return(container)
  }
  
  models <- findBiggestModel(model1, model2)
  
  intersection <- getModelIntersect(models$smallest, models$biggest)
  
  return(intersection)
}

findBiggestModel <- function(model1, model2) {
  biggest <- model2
  smallest <- model1
  
  if(length(unique(model1$Category)) > length(unique(model2$Category))) {
    biggest <- model1
    smallest <- model2
  }
  
  return(list("biggest" = biggest, "smallest" = smallest))
}

optimaliseTopicIntersection <- function(id1, id2) {
  source("relevance-calc.R")
  
  model1 <- readFileId(id1, TRUE)
  model2 <- readFileId(id2, TRUE)
  
  old_model <- model2
  train_id <- id1
  
  if(length(unique(model1$Category)) > length(unique(model2$Category))) {
    old_model <- model1
    train_id <- id2
  }
  
  optimum <- optimumSearch(10, 100, train_id, old_model)
  optimum <- optimumSearch((optimum - 100), 10, train_id, old_model)
  optimum <- optimumSearch((optimum - 10), 1, train_id, old_model)
  
  new_model <- relevanceCalculation(train_id, optimum)
  
  return(topicSplitMatrix(new_model, old_model))
}

optimumSearch <- function(bound, stepping, train_id, old_model) {
  error_data <- seq(from = 0, to = 0, length.out = length(100))
  bound_data <- seq(from = 0, to = 0, length.out = length(100))
  
  count = 1
  execute = TRUE
  
  while(execute == TRUE && count < 40) {
    new_model <- relevanceCalculation(train_id, bound)
    
    # Calculate intersect
    intersection <- topicSplitMatrix(new_model, old_model)
    # Setting errors
    intersection[intersection > 0] = 0
    intersection[intersection < 0] = 1
    
    # Count total error
    error_data[count] = sum(intersection)
    bound_data[count] = bound
    
    if(count > 6) {
      if(all(error_data[count] > error_data[(count - 5):(count - 1)])) {
        execute = FALSE
      }
    }
    
    bound = bound + stepping
    count = count + 1
  }
  
  print(paste("Error found: ", min(error_data)))
  print(paste("Bound found: ", bound_data[which.min(error_data)]))
  
  return(bound_data[which.min(error_data)])
}

plotMatrix <- function(intersection, models, notes = TRUE) {
  size <- length(models$biggest$Term) # number of words
  topics <- length(unique(models$biggest$Category)) # number of topics
  chunkSize <- size/topics
  
  # Split intersection into 1 topic per column based on chunkSize
  splitIntersection <- as.matrix(data.frame(split(intersection, ceiling(seq_along(intersection) / chunkSize))))
  # Split terms into 1 topic per column based on chunkSize
  splitTerms <- as.matrix(data.frame(split(models$biggest$Term, ceiling(seq_along(models$biggest$Term) / chunkSize))))
  
  library(gplots)
  
  colorMap = c("gold", "white", "aquamarine", "azure3", "bisque3", "blue", "blueviolet", "brown2", "cadetblue", "chartreuse3", "chocolate1", "darkgoldenrod4")
  
  nCats = length(unique(models$smallest$Category))
  nTopics = length(unique(models$biggest$Category))
  
  dev.off()
  if (notes == TRUE) {
    heatmap.2(x = splitIntersection, cellnote = splitTerms,                                                                # Intersection and term data
              labCol = seq(1, nTopics, 1), xlab = "Topics", ylab = "Words", cexRow = 1, cexCol = 1, srtCol = 0,            # Adjust label text, size, and positioning
              col = colorMap[1:(nCats + 3)], breaks = -3:(nCats),                                                          # Cell colors and color break values (when colors swap)
              Rowv = FALSE, Colv = FALSE, dendrogram = "none", notecol = "black", notecex = 1, trace = "none", key = FALSE # Cleanup of plot
              #, lhei = c(0.01, 0.99), lwid = c(0.01, 0.99), margins = c(2.5, 3.5)
    )
  } else {
    heatmap.2(x = splitIntersection,                                                                             # Intersection data
              labCol = seq(1, nTopics, 1), xlab = "Topics", ylab = "Words", cexRow = 1, cexCol = 1, srtCol = 0,  # Adjust label text, size, and positioning
              col = colorMap[1:(nCats + 3)], breaks = -3:(nCats),                                                # Cell colors and color break values (when colors swap)
              Rowv = FALSE, Colv = FALSE, dendrogram = "none", trace = "none", key = FALSE,                      # Cleanup of plot
              sepcolor = "black", sepwidth = c(0.005, 0.005),                                                    # Seperation color and size
              rowsep = 1:nrow(splitIntersection), colsep = 1:ncol(splitIntersection)                             # Seperation locations
    )
  }
}