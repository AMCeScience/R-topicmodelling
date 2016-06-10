#!/usr/bin/env Rscript
# https://stackoverflow.com/questions/18306362/run-r-script-from-command-line

# Clear workspace
rm(list = ls())

options(echo = TRUE)
args <- commandArgs(trailingOnly = TRUE)

if (length(args) > 0) {
  print("Taking cli arguments.")
  
  workspace = args[1]
  is <- 1 : 10
  ks = c(3,4,5,6,7,8,20)
  
  # Load the config
  if (!exists("configLoaded")) source("config.R")
  
  print(paste("Changing to workspace:", workspace))
  
  setwd(workspace)
  
  corpus_name = args[2]
} else {
  print("Taking preset arguments.")
  
  workspace <- "~/workspace/R"
  is <- 1 : 10
  ks = c(4,5)
  
  setwd(workspace)
  
  # Load the config
  if (!exists("configLoaded")) source("config.R")
  
  corpus_name = "clean_corpus.rds"
}

library(tm)
library(parallel)

if (file.exists(paste("data", corpus_name, sep = "/"))) cleanCorpus <- readRDS(paste("data", corpus_name, sep = "/"))
if (!exists("cleanCorpus")) return

library(topicmodels)

# https://github.com/cpsievert/xkcd/tree/master/code
# https://github.com/cpsievert/xkcd/blob/master/code/03-fitLDA.R 
# -----------------------------------------------------------------------
# General idea: 
#  (1) Split documents into training/test sets.
#  (2) Fit models (varying by the number of topics) to the training set.
#  (3) Compute the perplexity (log-likelihood of the held-out test set) for
#   each fitted model.
# -----------------------------------------------------------------------

source("dtm_handlers.R")

if (file.exists("data/split_corpus.rds")) {
  split <- readRDS("data/split_corpus.rds")
} else {
  split <- split_corpus(cleanCorpus)
  saveRDS(split, "data/split_corpus.rds")
}

source("fit.R")

perps <- data.frame(ks = ks)
count <- 2

timer <- proc.time()

pb <- txtProgressBar(min=0, max=length(is), style=3)

for (i in is) {
  merge <- merge_corpus(split, i)
  
  # Make ten subsets of approx 10% of the whole set
  dtm_train <- merge$train
  dtm_test <- DocumentTermMatrix(merge$test)
  
  # fit a bunch of models -- varying the number of topics
  # section 2.4 of http://www.jstatsoft.org/v40/i13/paper
  # has a nice, concise overview of model selection for LDA
  models <- mclapply(ks, function(k) TmLDASimulation(dtm_train, "", k, alpha, beta, burnin = burnin, iter = iter, keep = keep, store = FALSE))
  
  # Plot the perplexity
  perps[,count] <- sapply(models, perplexity, dtm_test)
  
  saveRDS(perps, gsub("__", i, "data/perplexity_incremental__.rds"))
  
  count <- count + 1
  setTxtProgressBar(pb, i)
}
close(pb)
print(proc.time() - timer)

saveRDS(perps, gsub("__", i, "data/perplexity__.rds"))