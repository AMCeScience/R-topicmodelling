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

runFullSet <- function(setNum, folder, store) {
  data <- readRDS(paste(folder, '/LDA_', setNum, '.rds', sep = ''))
  
  thetas <- data$posterior$theta
  
  # Create include/exclude factor
  y <- vector(length = length(thetas[,1]))
  y[] <- 'exclude'
  y[includes] <- 'include'
  
  y <- factor(y)
  
  # Create data frame out of thetas
  input <- data.frame(X = thetas)
  # Append the factor
  input$Class <- y
  
  inTrain <- rep(0, length(thetas[,1]))
  inTrain[1:3515] <- 1
  
  training <- input[inTrain == 1,]
  testing <- input[inTrain == 0,]
  
  valList <- list()
  
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
  
  valList <- list(rf = rf, rfProbs = rfProbs, ROC = ROC, base_positives = base_positives, base_negatives = base_negatives,
              test_positives = test_positives, test_negatives = test_negatives, TP = TP, TN = TN, FP = FP, FN = FN, recall = recall,
              accuracy = accuracy, precision = precision, sensitivity = sensitivity, specificity = specificity, F1 = F1)
  
  if (store) {
    saveRDS(valList, paste(folder, '/', setNum, '_set.rds', sep = ''))
  } else {
    return(valList)
  }
}

runDataSet <- function(setNum, folder, store) {
  # Prepare dataset
  data <- readRDS(paste(folder, '/LDA_', setNum, '.rds', sep = ''))
  
  thetas <- data$posterior$theta  
  
  # Create include/exclude factor
  y <- vector(length = length(thetas[,1]))
  y[] <- 'exclude'
  y[includes] <- 'include'
  
  y <- factor(y)
  
  # Create data frame out of thetas
  input <- data.frame(X = thetas)
  # Append the factor
  input$Class <- y
  
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
    
    if (setNum < 40) {
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