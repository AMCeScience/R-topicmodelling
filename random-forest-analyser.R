datasets <- c(5, 10, 15, 20, 25, 30, 35, 40, 45, 50, 55, 60, 65, 70, 75, 80, 85, 90, 95, 100, 150, 200, 250, 300, 350, 400, 450, 500)

FMeasure <- function(precision, recall, beta = 1) {
  return((1 + beta^2) * ((precision * recall) / ((beta^2 * precision) + recall)))
}

getData <- function(setName) {
  x <- c()
  y <- c()
  sd <- c()
  
  precision <- c()
  F1 <- c()
  reduction <- c()
  
  sizeTrue <- function(x) {
    return(length(x[x == TRUE]))
  }
  
  # Loop all the data sets
  for (i in 1:length(datasets)) {
    topic <- datasets[[i]]
    
    topicData <- readRDS(paste('data/', setName, '/', topic, '_set.rds', sep = ''))
    
    recallList <- c()
    precisionList <- c()
    F1List <- c()
    reductionList <- c()
    
    # Loop all the folds
    for (j in 1:10) {
      recallList <- c(recallList, topicData[[j]]$recall)
      precisionList <- c(precisionList, topicData[[j]]$precision)
      # F1List <- c(F1List, topicData[[j]]$F1)
      F1List <- c(F1List, FMeasure(topicData[[j]]$precision, topicData[[j]]$recall, 2))
      reductionList <- c(reductionList, (topicData[[j]]$TP + topicData[[j]]$FP) / (sizeTrue(topicData[[j]]$base_positives) + sizeTrue(topicData[[j]]$base_negatives)))
    }
    
    x <- c(x, length(topicData[[1]]$rf$coefnames))
    y <- c(y, mean(recallList))
    sd <- c(sd, sd(recallList))
    precision <- c(precision, mean(precisionList))
    F1 <- c(F1, mean(F1List))
    reduction <- c(reduction, 1 - mean(reductionList))
  }
  
  return(data.frame(x = x, y = y, sd = sd, precision = precision, F1 = F1, reduction = reduction))
}

dataStemmed <- getData('sysrev')
dataNotStemmed <- getData('sysrev-nostemming')
dataTermine <- getData('sysrev-termine')
dataNotStemmedOrGrammed <- getData('sysrev-nostemminggrams')
dataTermineStemmed <- getData('sysrev-terminestemmed')

plot_this <- function(x, y, sd, F1, reduction) {
  plotFrame <- data.frame(x = x, y = y, sd = sd)
  
  library("Hmisc")
  
  with (
    data = plotFrame,
    expr = errbar(x, y, y + sd, y - sd, add = F, pch = 1, cap = .015, log = "x", xlab = '', ylab = '', ylim = c(0,1.1))
  )
  title(ylab = "Recall", line = 2.2, cex.lab = 1)
  title(xlab = "# Topics", line = 2.1, cex.lab = 1)
  lines(plotFrame$x, F1, col='red')
  lines(plotFrame$x, reduction, col='blue')
}

plot_this(dataStemmed$x, dataStemmed$y, dataStemmed$sd, dataStemmed$F1, dataStemmed$reduction)
plot_this(dataNotStemmed$x, dataNotStemmed$y, dataNotStemmed$sd, dataNotStemmed$F1, dataNotStemmed$reduction)
plot_this(dataNotStemmedOrGrammed$x, dataNotStemmedOrGrammed$y, dataNotStemmedOrGrammed$sd, dataNotStemmedOrGrammed$F1, dataNotStemmedOrGrammed$reduction)
plot_this(dataTermineStemmed$x, dataTermineStemmed$y, dataTermineStemmed$sd, dataTermineStemmed$F1, dataTermineStemmed$reduction)
plot_this(dataTermine$x, dataTermine$y, dataTermine$sd, dataTermine$F1, dataTermine$reduction)