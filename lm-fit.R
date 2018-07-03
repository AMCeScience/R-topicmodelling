suppressMessages(library(tm))
suppressMessages(library(glmnet))
suppressMessages(library(pROC))
suppressMessages(library(caret))
suppressMessages(library(methods))
require(doMC)
registerDoMC(cores = 4)

fitCorpus <- function(dtm_matrix, y, i) {
  train_folds <- createFolds(y = y, k = 10, list = TRUE)

  # Get first fold as test
  fold <- train_folds[[1]]

  train <- dtm_matrix[-fold,]
  train_y <- y[-fold]
  test <- dtm_matrix[fold,]
  test_y <- y[fold]
  
  c <- cv.glmnet(train, train_y, parallel = TRUE, family = "binomial")

  prob <- predict(c, type = "response", newx = test, s = "lambda.1se")

  coeffs <- coef(c, s = "lambda.1se")
  cut_coeffs <- coeffs[2:length(coeffs[,1]),]
  filtered_coeffs <- cut_coeffs[cut_coeffs > 0]
  words <- names(filtered_coeffs)

  roc <- roc(test_y, as.vector(prob))

  boolean_prob <- factor(prob > 0.5)

  cfm <- confusionMatrix(boolean_prob, test_y)

  saveRDS(cfm, paste(project_location, "/results/confusion_matrix_", i ,".rds", sep = ""))
  saveRDS(filtered_coeffs, paste(project_location, "/results/coefficients_", i, ".rds", sep = ""))
  saveRDS(c, paste(project_location, "/results/fit_", i, ".rds", sep = ""))
  saveRDS(words, paste(project_location, "/results/words_", i, ".rds", sep = ""))
  saveRDS(roc, paste(project_location, "/results/roc_", i, ".rds", sep = ""))
}

start <- function(project_name) {
  data_folder <- getProjectFolder(project_name)

  files = list.files(path = paste(data_folder, "/corpora", sep = ""), full.names = TRUE)

  count = 0

  includes <- unlist(includes)

  for (corpus_filename in files) {
    print(corpus_filename)

    corpus <- readRDS(corpus_filename)

    dtm <- DocumentTermMatrix(corpus)

    # minimized_dtm <- removeSparseTerms(dtm, sparse = 0.99)

    dtm_matrix <- as.matrix(minimized_dtm)

    y <- vector(length = length(dtm_matrix[,1]))
    y[] <- FALSE
    y[includes] <- TRUE

    y <- factor(y)

    fitCorpus(dtm_matrix, y, count)

    count = count + 1
  }
}