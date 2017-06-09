library(tm)
library(glmnet)
library(pROC)
library(caret)
library(methods)

# corpus <- readRDS("data/contrast-2011/clean_corpus_1.rds")

print("Getting DTM")
dtm <- DocumentTermMatrix(corpus)

minimized_dtm <- removeSparseTerms(dtm, sparse = 0.99)

includes <- vector("list")

for (i in 1:length(corpus$content)) {
  if (corpus$content[[i]]$meta$included) {
    includes <- c(includes, i)
  }
}

includes <- unlist(includes)

dtm_matrix <- as.matrix(minimized_dtm)

y <- vector(length = length(dtm_matrix[,1]))
y[] <- FALSE
y[includes] <- TRUE

y <- factor(y)

dtm_df <- as.data.frame(dtm_matrix)

# Append the factor
#dtm_df$Class <- y

print("Getting folds")
#train_folds <- createDataPartition(y = dtm_df$Class, times = 5, list = TRUE)
train_folds <- createFolds(y = y, k = 10, list = TRUE)

print("Starting folds")

i <- seq(1,10)

words <- vector("list")
fits <- vector("list")
rocs <- vector("list")

runFold <- function(i, train_folds, project_location) {
  fold <- train_folds[[i]]

  train <- dtm_matrix[-fold,]
  train_y <- y[-fold]
  test <- dtm_matrix[fold,]
  test_y <- y[fold]

  c <- cv.glmnet(train, train_y, family = "binomial")

  prob <- predict(c, type = "response", newx = test, s = "lambda.1se")

  coeffs <- coef(c, s = "lambda.1se")
  cut_coeffs <- coeffs[2:length(coeffs[,1]),]
  filtered_coeffs <- cut_coeffs[cut_coeffs > 0]
  #ordered <- filtered_coeffs[order(filtered_coeffs, decreasing = TRUE)]
  words <- names(filtered_coeffs)

  roc <- roc(test_y, as.vector(prob))

  saveRDS(c, paste(project_location, "/fit_", i, ".rds", sep = ""))
  saveRDS(words, paste(project_location, "/words_", i, ".rds", sep = ""))
  saveRDS(roc, paste(project_location, "/roc_", i, ".rds", sep = ""))
}

data <- mclapply(
  i,
  function(k) runFold(k, train_folds, project_location),
  mc.cores = parallel_cores,
  mc.silent = FALSE
)
#
# saveRDS(data, paste(project_location, "data.rds", sep = "/"))

# for (k in i) {
#   runFold(k, train_folds, project_location)
# }

# all_fit <- list("vector")
# all_words <- list("vector")
# all_roc <- list("vector")
#
# for (k in i) {
#   all_fit[i] <- c(readRDS(paste(project_location, "/fit_", k, ".rds", sep = "")))
#   all_words[i] <- c(readRDS(paste(project_location, "/words_", k, ".rds", sep = "")))
#   all_roc[i] <- c(readRDS(paste(project_location, "/roc_", k, ".rds", sep = "")))
# }
#
# saveRDS(all_fit, paste(project_location, "fits.rds", sep = "/"))
# saveRDS(all_words, paste(project_location, "words.rds", sep = "/"))
# saveRDS(all_roc, paste(project_location, "rocs.rds", sep = "/"))