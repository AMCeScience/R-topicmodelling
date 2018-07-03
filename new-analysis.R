#!/usr/bin/env Rscript

source("cli/cli-input.R")
source("libraries/utils.R")

project_name <- args[2]

fold_cutoff <- 50

project_location <- getProjectFolder(project_name)

library("tm")
library("stats")
library("reticulate")
library("wordcloud")
library("slam")

store_folder <- paste(project_location, "analysis", sep = "/")

files <- list.files(paste(project_location, "results", sep = "/"), pattern = "fit_[[:digit:]]{1,2}\\.rds")

file_loop <- c()

for (file in files) {
  file_number <- as.numeric(gsub("fit_", "", gsub(".rds", "", file)))

  file_loop = c(file_number, file_loop)
}

# ######################### CONFUSION MATRICES #########################

confusion_matrix <- function() {
  all_fdrs <- list()
  all_fors <- list()

  count <- 1

  for (k in file_loop) {
    this_cm <- readRDS(paste(project_location, "/results/confusion_matrix_", k, ".rds", sep = ""))

    TP <- this_cm$table[1,1]
    FP <- this_cm$table[1,2]
    FN <- this_cm$table[2,1]
    TN <- this_cm$table[2,2]

    FDR <- FP / (TP + FP)
    FOR <- FN / (FN + TN)

    all_fdrs[count] <- FDR
    all_fors[count] <- FOR

    count <- count + 1
  }

  py_save_object(list('fdrs' = all_fdrs, 'fors' = all_fors), paste(store_folder, "fdrs.pickle", sep = "/"))
}

# ######################### PLOT ROCS #########################

roc_data <- function() {
  all_words <- data.frame(ZplaceholderZ = c(1))
  tprs <- list()
  fprs <- list()
  aucs <- list()

  count <- 1

  for (k in file_loop) {
    this_roc <- readRDS(paste(project_location, "/results/roc_", k, ".rds", sep = ""))
    
    tprs[count] <- list(rev(this_roc$sensitivities))
    fprs[count] <- list(1 - rev(this_roc$specificities))
    aucs[count] <- as.numeric(this_roc$auc)
    
    count <- count + 1
  }

  py_save_object(list('tpr' = tprs, 'fpr' = fprs, 'aucs' = aucs), paste(store_folder, "roc.pickle", sep = "/"))
}

# # ######################### GET PARAMETER CHOSEN FOR EACH DATASET #########################

coefficient_data <- function() {
  all_words <- data.frame(ZplaceholderZ = c(1))

  for (k in file_loop) {
    this_words <- readRDS(paste(project_location, "/results/words_", k, ".rds", sep = ""))

    for (word in this_words) {
      if (is.null(all_words[[word]])) {
        all_words[[word]] <- 1
      } else {
        all_words[[word]] <- all_words[[word]] + 1
      }
    }
  }

  # Write to CSV
  all_words <- all_words[ , !(names(all_words) %in% c("ZplaceholderZ"))]
  all_words <- all_words[order(all_words, decreasing = TRUE)]
  all_words <- all_words[,apply(all_words, 2, function(x) any(x > fold_cutoff))]

  saveRDS(all_words, paste(store_folder, "coefficients.rds", sep = "/"))
  write.csv(all_words, paste(store_folder, "contrast_sets_combined.csv", sep = "/"))
}

# ######################### GET BD AND NBD WORD COUNTS #########################

corpus_counts <- function() {
  getIncluded <- function(doc) {
    return(doc$meta$included)
  }

  for (k in file_loop) {
    corpus <- readRDS(paste(project_location, "/corpora/clean_corpus_", k, ".rds", sep = ""))
    dtm_tfidf <- DocumentTermMatrix(corpus, control = list(weighting = weightTfIdf))
    dtm <- DocumentTermMatrix(corpus)

    bd_docs <- unlist(lapply(corpus, getIncluded))

    bd_dtm <- dtm[c(bd_docs),]

    bd_tfidf_sum <- colSums(as.matrix(dtm_tfidf))
    bd_counts <- colSums(as.matrix(bd_dtm))

    break
  }

  dtm_list <- list()

  count <- 1

  for (k in file_loop) {
    corpus <- readRDS(paste(project_location, "/corpora/clean_corpus_", k, ".rds", sep = ""))
    dtm <- DocumentTermMatrix(corpus)

    nbd_docs <- !bd_docs

    dtm2list <- apply(dtm[c(nbd_docs),], 1, function(x) {
      paste(rep(names(x), x), collapse=" ")
    })

    dtm_list[[count]] <- dtm2list

    print(count)

    count <- count + 1

    # if (count > 3) {
    #   break
    # }
  }

  saveRDS(dtm_list, paste(store_folder, "nbd_dtm_lists.rds", sep = "/"))

  dtm_list <- readRDS(paste(store_folder, "nbd_dtm_lists.rds", sep = "/"))

  corp <- VCorpus(VectorSource(unlist(dtm_list)))

  nbd_tfidf <- DocumentTermMatrix(corp, control = list(weighting = weightTfIdf))
  print('after')
  
  nbd_tfidf_sum <- slam::col_sums(nbd_tfidf)
  print('summing')

  saveRDS(bd_tfidf_sum, paste(store_folder, "bd_tfidf.rds", sep = "/"))
  saveRDS(nbd_tfidf_sum, paste(store_folder, "nbd_tfidf.rds", sep = "/"))

  # saveRDS(bd_counts, paste(store_folder, "bd_counts.rds", sep = "/"))
  # saveRDS(nbd_counts, paste(store_folder, "nbd_counts.rds", sep = "/"))
  print('stored')
}

# ######################### CREATE WORDCLOUDS FOR BD AND NBD #########################

make_wordcloud <- function() {
  bd_counts <- readRDS(paste(store_folder, "bd_tfidf.rds", sep = "/"))
  nbd_counts <- readRDS(paste(store_folder, "nbd_tfidf.rds", sep = "/"))

  png(paste(store_folder, "BD_wordcloud.png", sep = "/"), width = 1200, height = 1200, res = 200)
  
  wordcloud(names(bd_counts), bd_counts, colors=brewer.pal(8, "Dark2"), scale = c(3,0.5), min.freq = 10, max.words = 100, random.order = FALSE)

  dev.off()

  png(paste(store_folder, "NBD_wordcloud.png", sep = "/"), width = 1200, height = 1200, res = 200)

  wordcloud(names(nbd_counts), nbd_counts, colors=brewer.pal(8, "Dark2"), scale = c(3,0.5), min.freq = 10, max.words = 100, random.order = FALSE)

  dev.off()

  coefs <- readRDS(paste(store_folder, "coefficients.rds", sep = "/"))

  png(paste(store_folder, "coefs.png", sep = "/"), width = 1200, height = 1200, res = 200)

  wordcloud(names(coefs), coefs ^ 4, colors=brewer.pal(8, "Dark2"), scale = c(2.5,0.5), random.order = FALSE, max.words = 200, rot.per = 0.35)

  dev.off()
}

confusion_matrix()
roc_data()
# coefficient_data()
# corpus_counts()
# make_wordcloud()