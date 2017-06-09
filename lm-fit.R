library(tm)
library(glmnet)
library(pROC)
library(caret)
library(methods)

#corpus <- readRDS("data/contrast-tdm/clean_corpus_1.rds")

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
i <- 0
words <- vector("list")
fits <- vector("list")
rocs <- vector("list")

runFold <- function(fold) {
  train <- dtm_matrix[-fold,]
  train_y <- y[-fold]
  test <- dtm_matrix[fold,]
  test_y <- y[fold]

  c <- cv.glmnet(train, train_y, family = "binomial")
  fits[i] <- c

  prob <- predict(c, type = "response", newx = test, s = "lambda.1se")

  coeffs <- coef(c, s = "lambda.1se")
  cut_coeffs <- coeffs[2:length(coeffs[,1]),]
  filtered_coeffs <- cut_coeffs[cut_coeffs > 0]
  #ordered <- filtered_coeffs[order(filtered_coeffs, decreasing = TRUE)]
  words[i] <- names(filtered_coeffs)

  rocs[i] <- roc(test_y, as.vector(prob))

  # if (i == 0) {
  #   plot(g)
  # } else {
  #   lines(g)
  # }

  i <- i + 1

  print(paste("Ran fold", i, sep = " "))
}

mclapply(
  train_folds,
  function(fold) runFold(fold),
  mc.cores = parallel_cores,
  mc.silent = parallel_silent
)

# for (fold in train_folds) {
#   runFold(fold)
# }

saveRDS(fits, paste(project_location, "fits.rds", sep = "/"))
saveRDS(words, paste(project_location, "words.rds", sep = "/"))
saveRDS(rocs, paste(project_location, "rocs.rds", sep = "/"))