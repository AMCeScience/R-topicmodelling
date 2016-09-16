getRawTheta <- function(id) {
  source("postprocessing/postprocessing.R")
  
  data <- readFileId(id)
  
  theta <- colSums(data$posterior$theta)
  
  topic_frame <- data.frame(category = seq_along(theta))
  topic_frame <- cbind(topic_frame, theta = theta)
  
  ordered_frame <- topic_frame[order(topic_frame["theta"], decreasing = TRUE),]
  
  return(ordered_frame)
}

getProportions <- function(id) {
  data <- readFileId(id)
  
  topic.frequency <- colSums(data$posterior$theta * data$tokensPerDoc)
  topic.proportion <- topic.frequency/sum(topic.frequency)
  
  topic_frame <- data.frame(category = seq_along(topic.proportion))
  topic_frame <- cbind(topic_frame, proportion = topic.proportion)
  
  ordered_frame <- topic_frame[order(topic_frame["proportion"], decreasing = TRUE),]
  
  return(ordered_frame)
}

getTopics <- function(rels, goal_surface) {
  total <- getSurface(rels, length(rels))
  
  curr_surface <- 0
  last_surface <- 0
  numTopics <- 1
  
  while (curr_surface < goal_surface) {
    last_surface <- curr_surface
    
    curr_surface <- getSurface(rels, numTopics)
    
    numTopics <- numTopics + 1
  }
  
  curr_diff <- abs(curr_surface - goal_surface)
  last_diff <- abs(last_surface - goal_surface)
  
  if (curr_diff < last_diff) {
    return(numTopics - 1)
  } else {
    return(numTopics - 2)
  }
}

getSurface <- function(rels, numTopics) {
  normalised <- normalise(rels)
  
  total_area <- getArea(normalised, length(normalised))
  
  partial_area <- getArea(normalised, numTopics)
  
  return((partial_area / total_area) * 100)
}

getArea <- function(normalised_rels, numTopics) {
  used_rels <- normalised_rels[1:numTopics]
  
  height <- (used_rels[-1] + used_rels[-length(used_rels)]) / 2
  width <- -diff(1:length(used_rels))
  return(sum(height * width))
}

plotRels <- function(rels) {
  normalised <- normalise(rels)
  
  plot(normalised, xlab = "Ordered Topics", ylab = "Normalised Theta", type = "l")
}

normalise <- function(rels) {
  normalised <- rels - min(rels)
  
  return(normalised[order(normalised, decreasing = TRUE)])
}