setwd("~/workspace/R")

library("tm")
library("stringr")

source("config.R")
source("cli/cli-input.R")
source("libraries/utils.R")
source("libraries/fit.R")

#project_name <- args[2]
project_name <- "clef-redo"

project_location <- getProjectFolder(project_name)

# # Get review IDs
# reviews <- read.csv(paste(project_location, "review_ids.csv", sep = "/"), header = FALSE, stringsAsFactors = FALSE)
# review_ids <- trimws(reviews[,1])
#
# Get corpus
# docs <- readRDS(paste(project_location, "clean_corpus_1.rds", sep = "/"))
#
# # Add in metadata
# for (i in 1:length(docs)) {
#   docs[[i]]$meta$review_id <- review_ids[[i]]
# }
#
# saveRDS(docs, paste(project_location, "clean_corpus_metadata.rds", sep = "/"))
#
# docs_cleaned <- docs
#
# # Remove empty documents
# for (i in 1:length(docs_cleaned)) {
#   if (i > length(docs_cleaned)) {
#     break
#   }
#
#   if (str_length(docs_cleaned[[i]]$content) < 1) {
#     docs_cleaned[[i]] <- NULL
#   }
# }
#
# # Removed two docs not picked up by the method above
# docs_cleaned[[65392]] <- NULL
# docs_cleaned[[140435]] <- NULL
#
# saveRDS(docs_cleaned, paste(project_location, "clean_corpus_empties_removed.rds", sep = "/"))
#
# dtm <- DocumentTermMatrix(docs_cleaned)
#
# saveRDS(dtm, paste(project_location, "dtm.rds", sep = "/"))
#
# dtm <- readRDS(paste(project_location, "dtm.rds", sep = "/"))
#
# k <- 75
#
# fit <- setupFitting(dtm, project_name, 1, k, fit_divider/k, fit_beta, fit_burnin, fit_iter, fit_thin, fit_keep)
#
# review_ids_clean <- list()
#
# for (i in 1:length(docs_cleaned)) {
#   review_ids_clean[[i]] <- docs_cleaned[[i]]$meta$review_id
# }
#
# saveRDS(review_ids_clean, paste(project_location, "review_ids_clean.rds", sep = "/"))
#
review_ids <- readRDS(paste(project_location, "review_ids_clean.rds", sep = "/"))
unique_ids <- unique(review_ids)
#
# topics <- readRDS(paste(project_location, "LDA_fit_k75_a0.67_b0.01_1.rds", sep = "/"))
# source(paste(project_location, "rf_selection.R", sep = "/"))
#
# data_matrix <- topics$posterior$theta
#
# get_train_corpus <- function(exclude) {
#   to_include <- unique_ids[unique_ids != exclude]
#
#   doc_nums <- which(review_ids %in% to_include)
#
#   return(doc_nums)
# }
#
# get_test_corpus <- function(include) {
#   doc_nums <- which(review_ids %in% include)
#
#   return(doc_nums)
# }
#
# source("libraries/random-forest-builder.R")
#
# for (i in 3:length(unique_ids)) {
#   test_id <- unique_ids[[i]]
#
#   train_corpus <- get_train_corpus(test_id)
#   # test_corpus <- get_test_corpus(test_id)
#
#   result <- setupForest(data_matrix, includes, project_location, i, FALSE, train_corpus)
# }

tm_data <- readRDS(paste(project_location, "LDA_fit_k75_a0.67_b0.01_1.rds", sep = "/"))

store <- list()
store_b <- list()
# forests <- list()

for (i in 1:length(unique_ids)) {
  # forests[[i]] <- readRDS(paste(project_location, "/RF_SF_", i, ".rds", sep = ""))
  forest_data <- forests[[i]]
  # store[[i]] <- forest_data$recall

  # store_b[[i]] <- sum(forest_data$base_positives) / (sum(forest_data$base_positives) + sum(forest_data$base_negatives))

  tree <- forest_data$rf

  varImp <- importance(tree$finalModel)
  #
  # # interesting_topics <- order(-varImp)[1:4]
  interesting_topics <- which(varImp > 10)
  #
  store[[i]] <- interesting_topics
}

cor(unlist(store_b), unlist(store))

cors <- list()

for (i in 1:75) {
  pos <- list()

  for (j in 1:length(unique_ids)) {
    forest_data <- importance(forests[[j]]$rf$finalModel)

    pos[[j]] <- which(names(forest_data[order(forest_data),]) == paste("`", i, "`", sep = ""))
  }

  cors[[i]] <- cor(unlist(pos), unlist(store))
}

# Join lists
concat_list <- do.call(c, store)

topic_table <- table(concat_list)

topic_lists <- terms(tm_data$LDAData, 30)

# interesting_topics <- topic_lists[, topics]

words <- topic_lists[, as.integer(names(topic_table[order(topic_table)]))]
