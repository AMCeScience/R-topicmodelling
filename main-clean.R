#!/usr/bin/env Rscript
# https://stackoverflow.com/questions/18306362/run-r-script-from-command-line

# Clear workspace
rm(list = ls())

args <- commandArgs(trailingOnly = TRUE)

if (length(args) > 0) {
  print("Taking cli arguments.")
  
  workspace = args[1]
  
  # Load the config
  if (!exists("configLoaded")) source("config.R")
  
  print(paste("Changing to workspace:", workspace))
  
  setwd(workspace)
  
  corpus_name = args[2]
  csv_name = args[3]
  
  store = TRUE
} else {
  print("Taking preset arguments.")
  
  workspace <- "~/workspace/R"
  
  setwd(workspace)
  
  # Load the config
  if (!exists("configLoaded")) source("config.R")
  
  corpus_name = "clean_sysrev_extended.rds"
  csv_name = "articles_sysrev_test.csv"
  
  store = FALSE
}

#if (file.exists(paste(folder, corpus_name, sep = "/"))) cleanCorpus <- readRDS(paste(folder, corpus_name, sep = "/"))
# if (!exists("cleanCorpus")) {
  source("preprocessing.R")
  cleanCorpus <- runPreprocessing(csv_name, store = store, corpus_name)
# }