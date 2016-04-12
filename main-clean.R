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
  
  corpus_name = args[2]
  csv_name = args[3]
} else {
  print("Taking preset arguments.")
  
  workspace <- "~/workspace/R"
  
  setwd(workspace)
  
  corpus_name = "clean_corpus.rds"
  csv_name = "articles.csv"
}

# Load the config
if (!exists("configLoaded")) source("config.R")

if (file.exists(paste("data", corpus_name, sep = "/"))) cleanCorpus <- readRDS(paste("data", corpus_name, sep = "/"))
if (!exists("cleanCorpus")) {
  source("preprocessing.R")
  cleanCorpus <- runPreprocessing(csv_name, store = TRUE, corpus_name)
}