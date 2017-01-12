#!/usr/bin/env Rscript
# https://stackoverflow.com/questions/18306362/run-r-script-from-command-line

# Clear workspace
rm(list = ls())

args <- commandArgs(trailingOnly = TRUE)

if (length(args) > 0) {
  print("Taking cli arguments.")
  
  workspace = args[1]
  
  print(paste("Changing to workspace:", workspace))
  
  setwd(workspace)
  
  # Load the config
  if (!exists("configLoaded")) source("config.R")
  
  # Overwrite the defaults
  folder = args[2]
  filename_separate = substr(args[3], 0, nchar(args[3]) - 4)
  corpus_filename = paste("originals", args[3], sep = "/")
  corpus_csv_name = paste("originals", args[4], sep = "/")
  #ks = c(seq(5, 100, 5), seq(150, 500, 50))
  ks = c(20, 30, 40, 50, 75, 100)
  cores <- 7
  
  versions <- list(c('stemmed_grammed', TRUE, TRUE), c('stemmed_notgrammed', TRUE, FALSE), c('notstemmed_grammed', FALSE, TRUE))
  
  #datasets <- c(5, 10, 15, 20, 25, 30, 35, 40, 45, 50, 55, 60, 65, 70, 75, 80, 85, 90, 95, 100, 150, 200, 250, 300, 350, 400, 450, 500)
  datasets <- c(20, 25, 30, 40, 50, 75, 100)
  folds <- 1:10
  store <- TRUE
} else {
  print("Taking preset arguments.")
  
  workspace <- "~/workspace/R"
  
  setwd(workspace)
  
  # Load the config
  #if (!exists("configLoaded")) source("config.R")
  source("config.R")
  
  ks = c(2)
  cores = 2
  
  versions <- list(c('stemmed_grammed', TRUE, TRUE), c('stemmed_notgrammed', TRUE, FALSE), c('notstemmed_grammed', FALSE, TRUE))
  
  folder <- "data/tests"
  filename_separate = "articles_sysrev_test"
  corpus_filename = "data/tests/articles_sysrev_test.rds"
  corpus_csv_name = "data/tests/lyme.csv"
  
  datasets <- c(2)
  folds <- 1:1
  store <- TRUE
}

print("Corpus loaded.")

library(coda)
library(parallel)
source("preprocessing.R")
source("fit.R")
source("random-forest-builder.R")

# Start timer
print("Starting run.")
timer <- proc.time()

for (version in versions) {
  corpus_filename = paste(filename_separate, '_', version[1], '.rds', sep = '')
  folder = paste('data/lyme_', version[1], sep = '')
  stem = (version[2] == 'TRUE')
  gram = (version[3] == 'TRUE')
  
  if (!file.exists(paste(folder, corpus_filename, sep = '/'))) {
    cleanCorpus <- runPreprocessing(corpus_csv_name, store = store, stem = stem, gram = gram, corpus_filename)
  }
  
  cleanCorpus <- readRDS(corpus_filename)
  
  mclapply(ks, function(k) TmLDASimulation(cleanCorpus, folder, k, 50/k, 0.01, burnin, iter, thin, keep, store, name_template = "LDA_KK.rds"), mc.cores = cores, mc.silent = TRUE)
  
  includes <- c(141, 175, 279, 281, 306, 336, 338, 343, 382, 384, 531, 601, 607, 640, 665,
                     680, 718, 729, 743, 757, 832, 860, 887, 923, 958, 1017, 1076, 1142, 1230, 1258, 1337,
                     1380, 1399, 1400, 1472, 1532, 1589, 1603, 1668, 1699, 1709, 1710, 1765, 1768, 1828,
                     1854, 1859, 1872, 1960, 1961, 1977, 1978, 1980, 2012, 2014, 2183, 2219, 2291, 2293,
                     2341, 2389, 2410, 2483, 2509, 2550, 2568, 2595, 2615, 2640, 2663, 2690, 2691, 2738,
                     2765, 2798, 2837, 2852, 2988, 2990, 2996, 3003, 3095, 3166, 3174, 3215, 3321, 3324,
                     3457, 3571, 3583, 3605, 3608, 3614, 3616, 3635, 3647, 3648, 3655, 3657, 3684, 3786,
                     3841, 3856, 3965)
  
  mclapply(datasets, function(set) runDataSet(set, folder, store), mc.cores = cores, mc.silent = TRUE)
}