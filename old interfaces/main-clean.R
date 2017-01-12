#!/usr/bin/env Rscript
# https://stackoverflow.com/questions/18306362/run-r-script-from-command-line

# Clear workspace
rm(list = ls())

args <- commandArgs(trailingOnly = TRUE)

corpus_folder <- "originals"
data_folder <- "data"
force <- TRUE

if (length(args) > 0) {
  print("Taking cli arguments.")
  
  workspace <- args[1]
  
  print(paste("Changing to workspace:", workspace))
  
  setwd(workspace)
  
  # Load the config
  if (!exists("configLoaded")) source("config.R")
  
  corpus_name <- args[2]
  csv_name <- args[3]
  
  store <- TRUE
} else {
  print("Taking preset arguments.")
  
  workspace <- "~/workspace/R"
  
  setwd(workspace)
  
  # Load the config
  if (!exists("configLoaded")) source("config.R")
  
  corpus_name <- "complete_2"
  csv_name <- "complete.csv"
  
  store <- TRUE
}

if (!dir.exists(corpus_folder)) {
  stop("Corpus directory does not exist")
}

csv_location <- paste(corpus_folder, csv_name, sep = "/")

if (!file.exists(csv_location)) {
  stop("CSV input file not found")
}

corpus_location <- paste(corpus_folder, corpus_name, ".rds", sep = "/")

if (!force && file.exists(corpus_location)) {
  cleanCorpus <- readRDS(corpus_location)
}

if (!exists("cleanCorpus")) {
  source("libraries/preprocessing.R")
  clean_corpus <- runPreprocessing(csv_location, FALSE, FALSE)
  
  if (store == TRUE) {
    if (!dir.exists(data_folder)) {
      stop("Data directory does not exist")
    }
    
    data_location <- paste(data_folder, corpus_name, sep = "/")
    
    dir.create(data_location)
    
    clean_corpus_location = paste(data_location, "clean_corpus.rds", sep = "/")
    
    print("Cleaning: store into rds file.")
    saveRDS(clean_corpus, clean_corpus_location)
  }
}