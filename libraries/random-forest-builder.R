# http://appliedpredictivemodeling.com/blog/2013/12/8/28rmc2lv96h8fw8700zm4nl50busep

# https://cran.r-project.org/web/packages/caret/caret.pdf
library(caret)
library(pROC)
library(randomForest)
source("libraries/utils.R")

singleFoldForest <- function(data, includes, training_selection, datasets_location, file_version, set_num) {
  thetas <- data$posterior$theta
  
  # Create include/exclude list
  input <- formatInput(thetas, includes)
  
  # Determine train and test portion of data
  inTrain <- rep(0, length(thetas[,1]))
  inTrain[training_selection] <- 1
  
  # Split dataset into train and test set
  training <- input[inTrain == 1,]
  testing <- input[inTrain == 0,]
  
  # Train forest
  rf_probs <- trainForest(training, testing, set_num)
  
  # Get output metrics
  results <- getMetrics(rf_probs, testing)
  
  if (rf_store) {
    saveRDS(results, paste(datasets_location, "/RF_SF_fit_k", set_num, "_", file_version, ".rds", sep = ""))
    
    rm(results)
  } else {
    return(results)
  }
}

crossFoldForest <- function(data, includes, datasets_location, file_version, set_num) {
  thetas <- data$posterior$theta  
  
  # Create include/exclude list
  input <- formatInput(thetas, includes)
  
  # Create training folds
  trainFolds <- createFolds(y = input$Class, k = length(rf_folds), list = FALSE)
  
  val_list <- list()
  
  # Fit folds
  for (i in rf_folds) {
    # Create test and train set for this fold
    testing <- input[trainFolds == i,]
    training <- input[trainFolds != i,]
    
    # Train forest
    rf_probs <- trainForest(training, testing, set_num)
    
    # Store output metrics into list
    val_list[[i]] <- getMetrics(rf_probs, testing)
  }
  
  if (rf_store) {
    saveRDS(val_list, paste(datasets_location, "/RF_CF_fit_k", set_num, "_", file_version, ".rds", sep = ""))
    
    rm(val_list)
  } else {
    return(val_list)
  }
}

trainForest <- function(training, testing, set_num) {
  # Count the number of includes in the training portion
  nmin <- sum(training$Class == "include")
  
  # Create control variable, set to cross validate
  ctrl <- trainControl(method = "cv",
                       classProbs = TRUE,
                       summaryFunction = twoClassSummary)
  
  mtry <- getMtry(set_num)
  
  tunegrid <- expand.grid(.mtry = mtry)
  
  rf <- train(Class ~ ., data = training,
              method = "rf",
              ntree = 1500,
              tuneGrid = tunegrid,
              metric = "ROC",
              trControl = ctrl,
              strata = training$Class,
              sampsize = rep(nmin, 2))
  
  return(predict(rf, testing, type = "prob"))
}

formatInput <- function(thetas, includes) {
  # Create include/exclude factor
  y <- vector(length = length(thetas[,1]))
  y[] <- 'exclude'
  y[includes] <- 'include'
  
  y <- factor(y)
  
  # Create data frame out of thetas
  input <- data.frame(X = thetas)
  # Append the factor
  input$Class <- y
  
  return(input)
}

getMtry <- function(set_num) {
  if (set_num < 50) {
    mtry <- seq(1, set_num, 1)
  } else {
    mtry <- seq(10, set_num, 10)
    
    # Append set_num if not divisible by 10
    if (set_num %% 10 != 0) {
      mtry <- c(mtry, set_num)
    }
  }
  
  return(mtry)
}

getMetricsForFile <- function(project_name, file_version, set_num, fold_type = "SF") {
  project_folder <- getProjectFolder(project_name)
  pattern <- paste("RF_", fold_type, "_fit_k", set_num, sep = "")
  file <- getAllOfVersion(pattern, project_folder, file_version)
  
  return(readRDS(paste(project_folder, file, sep = "/")))
}

getMetrics <- function(rf_probs, test_set) {
  ROC <- roc(response = test_set$Class, 
             predictor = rf_probs[,1],
             levels = rev(levels(test_set$Class)))
  
  base_positives <- test_set$Class == 'include'
  base_negatives <- test_set$Class == 'exclude'
  
  test_positives <- rf_probs$include >= 0.5
  test_negatives <- rf_probs$exclude > 0.5
  
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
  
  return(list(rf = rf, rf_probs = rf_probs, ROC = ROC, base_positives = base_positives, base_negatives = base_negatives,
             test_positives = test_positives, test_negatives = test_negatives, TP = TP, TN = TN, FP = FP, FN = FN, recall = recall,
             accuracy = accuracy, precision = precision, sensitivity = sensitivity, specificity = specificity, F1 = F1))
}