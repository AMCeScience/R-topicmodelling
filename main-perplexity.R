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
  ks = c(5, seq(10, 100, 10), seq(150, 500, 50))
  cores = 7
  
  # Load the config
  if (!exists("configLoaded")) source("config.R")
  
  print(paste("Changing to workspace:", workspace))
  
  setwd(workspace)
  
  corpus_name = args[2]
  
  if (file.exists(paste("originals", corpus_name, sep = "/"))) cleanCorpus <- readRDS(paste("originals", corpus_name, sep = "/"))
} else {
  print("Taking preset arguments.")
  
  workspace <- "~/workspace/R"
  is <- 1 : 2
  ks = c(2)
  cores = 2
  
  setwd(workspace)
  
  # Load the config
  if (!exists("configLoaded")) source("config.R")
  
  burnin = 800
  iter = 1000
  corpus_name = "clean_corpus.rds"
  
  if (file.exists(paste("data", corpus_name, sep = "/"))) cleanCorpus <- readRDS(paste("data", corpus_name, sep = "/"))
}

library(tm)
library(parallel)

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

source("dtm-handlers.R")

if (file.exists("data/split_corpus.rds")) {
  split <- readRDS("data/split_corpus.rds")
} else {
  split <- split_corpus(cleanCorpus)
  saveRDS(split, "data/split_corpus.rds")
}

source("fit.R")

perps <- data.frame(ks = ks)
count <- 1

timer <- proc.time()

for (i in is) {
  # Make ten subsets of approx 10% of the whole set
  merge <- merge_corpus(split, i)
  
  dtm_train <- merge$train
  dtm_test <- DocumentTermMatrix(merge$test)
  
  # fit a bunch of models -- varying the number of topics
  # section 2.4 of http://www.jstatsoft.org/v40/i13/paper
  # has a nice, concise overview of model selection for LDA
  models <- mclapply(ks, function(k) TmLDASimulation(dtm_train, "", k, alpha, beta, burnin = burnin, iter = iter, thin = thin, keep = keep, store = FALSE), mc.cores = cores, mc.silent = TRUE)
  
  # Plot the perplexity
  perps[,count] <- unlist(mclapply(models, perplexity, dtm_test, mc.cores = cores, mc.silent = TRUE))
  
  saveRDS(perps, gsub("__", i, "data/perplexity_incremental__.rds"))
  
  count <- count + 1
}

print(proc.time() - timer)

saveRDS(perps, "data/perplexity_complete.rds")

perplexityPlot <- function() {
  data <- readRDS("data/perplexity_complete.rds")
  
  dataMeans <- data.frame("T" = data[,1])
  dataMeans[,"Perplexity (x1000)"] <- rowMeans(data[,2:id]) / 1000
  
  plot(dataMeans, xlab = "", ylab = "")
  title(xlab = "T", line = 2.3, cex.lab = 1)
  title(ylab = "Perplexity (x1000)", line = 2.4, cex.lab = 1)
}