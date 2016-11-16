#!/usr/bin/env Rscript
# https://stackoverflow.com/questions/18306362/run-r-script-from-command-line

# http://appliedpredictivemodeling.com/blog/2013/12/8/28rmc2lv96h8fw8700zm4nl50busep

# https://cran.r-project.org/web/packages/caret/caret.pdf
library(caret)
library(pROC)
library(randomForest)
library(parallel)

sizeTrue <- function(x) {
  return(length(x[x == TRUE]))
}

runDataSet <- function(setNum) {
  # Prepare dataset
  data <- readRDS(paste(folder, '/LDA_', setNum, '.rds', sep = ''))
  
  includes <- c(2076, 2554, 2072, 1922, 2954, 3025, 3336, 1524, 2698, 2895, 1926, 1596, 2624, 2307, 1828, 
                2205, 2647, 1924, 1539, 2863, 2044, 1314, 1967, 1846, 3341, 2995, 2130, 2907, 2329, 2252,
                2791, 2952, 2877, 2766, 1057, 2651, 2464, 1132, 2311, 2061, 2053, 2336, 3141, 2768, 2802,
                3183, 905, 1343, 2250, 1083, 2817, 481, 1056)
  
  thetas <- data$posterior$theta  
  
  # Create include/exclude factor
  y <- vector(length = length(thetas[,1]))
  y[] <- 'exclude'
  y[includes] <- 'include'
  
  y <- factor(y)
  
  # Calls to select top X most important variables
  # selection <- order(-importance(rf1))[0:10]
  # input <- data.frame(X = thetas)[selection]
  
  # Create data frame out of thetas
  input <- data.frame(X = thetas)
  # Append the factor
  input$Class <- y
  
  #   inTrain <- createDataPartition(y = input$Class, p = .75, list = FALSE)
  #   
  #   training <- input[inTrain,]
  #   testing <- input[-inTrain,]
  
  trainFolds <- createFolds(y = input$Class, k = 10, list = FALSE)
  
  valList <- list()
  
  for (i in folds) {
    testing <- input[trainFolds == i,]
    training <- input[trainFolds != i,]
    
    table(training$Class)
    table(testing$Class)
    
    # Count the number of includes in the training portion
    nmin <- sum(training$Class == "include")
    
    # Create control variable, set to cross validate
    ctrl <- trainControl(method = "cv",
                         classProbs = TRUE,
                         summaryFunction = twoClassSummary)
    
    if (setNum < 50) {
      mtry <- seq(1, setNum, 1)
    } else {
      mtry <- seq(10, setNum, 10)
      
      # Append setNum if not divisible by 10
      if (setNum %% 10 != 0) {
        mtry <- c(mtry, setNum)
      }
    }
    
    tunegrid <- expand.grid(.mtry = mtry)
    
    rf <- train(Class ~ ., data = training,
                method = "rf",
                ntree = 1500,
                tuneGrid = tunegrid,
                metric = "ROC",
                trControl = ctrl,
                strata = training$Class,
                sampsize = rep(nmin, 2))
    
    rfProbs <- predict(rf, testing, type = "prob")
    ROC <- roc(response = testing$Class, 
               predictor = rfProbs[,1],
               levels = rev(levels(testing$Class)))
    
    base_positives <- testing$Class == 'include'
    base_negatives <- testing$Class == 'exclude'
    
    test_positives <- rfProbs$include >= 0.5
    test_negatives <- rfProbs$exclude > 0.5
    
    TP <- sizeTrue(test_positives & base_positives)
    TN <- sizeTrue(test_negatives & base_negatives)
    
    FP <- sizeTrue(test_positives & base_negatives)
    FN <- sizeTrue(test_negatives & base_positives)
    
    recall <- TP / sizeTrue(base_positives) # most important, has to be close to 1
    
    accuracy <- (TP + TN) / (TP + TN + FP + FN) # not interesting
    precision <- TP / (TP + FP) # not interesting
    
    sensitivity <- TP / sizeTrue(base_positives) # most important, has to be close to 1 == recall
    specificity <- TN / sizeTrue(base_negatives) # not interesting
    
    F1 <- 2 * ((precision * recall) / (precision + recall)) # not interesting, will be terrible
    
    #plot(downsampledROC, col = rgb(1, 0, 0, .5), lwd = 2)
    
    set <- list(rf = rf, rfProbs = rfProbs, ROC = ROC, base_positives = base_positives, base_negatives = base_negatives,
                test_positives = test_positives, test_negatives = test_negatives, TP = TP, TN = TN, FP = FP, FN = FN, recall = recall,
                accuracy = accuracy, precision = precision, sensitivity = sensitivity, specificity = specificity, F1 = F1)
    
    valList[[i]] <- set
  }
  
  if (store) {
    saveRDS(valList, paste(folder, '/', setNum, '_set.rds', sep = ''))
    
    rm(valList)
  } else {
    return(valList)
  }
}

############ INPUT ############

args <- commandArgs(trailingOnly = TRUE)

if (length(args) > 0) {
  print("Taking cli arguments.")
  
  workspace = args[1]
  folder = args[2]
  cores <- 7
  datasets <- c(5, 10, 15, 20, 25, 30, 35, 40, 45, 50, 55, 60, 65, 70, 75, 80, 85, 90, 95, 100, 150, 200, 250, 300, 350, 400, 450, 500)
  folds <- 1:10
  store <- TRUE
  
  print(paste("Changing to workspace:", workspace))
  
  setwd(workspace)
  
  mclapply(datasets, runDataSet, mc.cores = cores, mc.silent = TRUE)
} else {
  print("Taking preset arguments.")
  
  workspace <- "~/workspace/R"
  folder = 'test'
  cores <- 2
  datasets <- c(10)
  folds <- 1:1
  store <- FALSE
  
  setwd(workspace)
  
  data <- runDataSet(500)
}