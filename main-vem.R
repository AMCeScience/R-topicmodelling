#!/usr/bin/env Rscript
# https://stackoverflow.com/questions/18306362/run-r-script-from-command-line

# Clear workspace
rm(list = ls())

options(echo = TRUE)
args <- commandArgs(trailingOnly = TRUE)

is <- args[1] : args[2]

library(tm)
library(datasets)
library(stats)
library(utils)
library(methods)
library(grDevices)
library(graphics)

workspace <- "~/workspace/R"

# Set workspace to folder where articles.csv is placed
setwd(workspace)

# Load the config
if (!exists("ks")) source("config.R")

if (file.exists("data/clean_corpus.rds")) cleanCorpus <- readRDS("data/clean_corpus.rds")
if (!exists("cleanCorpus")) source("preprocessing.R")

library(topicmodels)

# https://github.com/cpsievert/xkcd/tree/master/code
# https://github.com/cpsievert/xkcd/blob/master/code/03-fitLDA.R 
# -------------------------------------------------------------------------
# General idea: 
#  (1) Split documents into training/test sets.
#  (2) Fit models (varying by the number of topics) to the training set.
#  (3) Compute the perplexity (log-likelihood of the held-out test set) for
#   each fitted model.
# -----------------------------------------------------------------------

#dtm <- DocumentTermMatrix(cleanCorpus)

source("dtm_handlers.R")

if (file.exists("data/split_corpus.rds")) {
  split <- readRDS("data/split_corpus.rds")
} else {
  split <- split_corpus(cleanCorpus)
  saveRDS(split, "data/split_corpus.rds")
}

perps <- data.frame(ks = ks)
count <- 1
for (i in is) {
  merge <- merge_corpus(split, i)
  
  dtm_train <- DocumentTermMatrix(merge$train)
  dtm_test <- DocumentTermMatrix(merge$test)
  
  # Make ten subsets of approx 10% of the whole set
  #dtms <- split_dtm(dtm)
  
  #dtm_merged <- merge_dtms(dtms, 1)
  #dtm_merged <- split_dtm_once(dtm)
  #dtm_train <- dtm_merged$train
  #dtm_test <- dtm_merged$test
  
  # save some summary statistics that we'll need for LDAvis
  #termFreqs <- colSums(as.matrix(dtm_train))
  #stopifnot(!any(termFreqs == 0))
  #saveRDS(termFreqs, "data/termFreqs.rds")
  
  #docLens <- rowSums(as.matrix(dtm_train))
  #stopifnot(!any(docLens == 0))
  #saveRDS(docLens, "data/docLens.rds")
  
  # fit a bunch of models -- varying the number of topics
  # section 2.4 of http://www.jstatsoft.org/v40/i13/paper
  # has a nice, concise overview of model selection for LDA
  #models <- lapply(ks, function(k) LDA(dtm_train, k, method = "Gibbs", control = list(alpha = alpha/k, delta = delta, burnin = G, iter = G, keep = 50)))
  #saveRDS(models, "data/models.rds")
  
  perps[,count] <- sapply(ks, function(k) {
    train <- LDA(dtm_train, k = k)
    
    test <- LDA(dtm_test, model = train, control = list(estimate.beta = FALSE))
    
    perplexity(test)
  })
  
  # Plot the perplexity
  # perps[,count] <- sapply(models, perplexity, dtm_test)
  
  saveRDS(perps, gsub("__", i, "data/perplexity_incremental__.rds"))
  #png(filename = "data/perplexity.png")
  #plot(ks, perps, xlab = "Number of topics", ylab = "Perplexity")
  #dev.off()
  
  count <- count + 1
}

saveRDS(perps, gsub("__", i, "data/perplexity__.rds"))