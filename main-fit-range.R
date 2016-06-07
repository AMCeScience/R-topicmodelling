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
  storeFolder = args[2]
  CSVfileName = args[3]
  k = args[4]
  divider = args[5]
  rangeAlpha = args[6]
  
  cleanCorpus <- readRDS(paste("data", CSVfileName, sep = "/"))
} else {
  print("Taking preset arguments.")
  
  workspace <- "~/workspace/R"
  
  setwd(workspace)
  
  # Load the config
  if (!exists("configLoaded")) source("config.R")
  
  #source("preprocessing.R")
  #cleanCorpus <- runPreprocessing(CSVfileName, store = TRUE)
  cleanCorpus <- readRDS("data/clean_corpus.rds")
}

print("Corpus loaded.")

source("fit.R")

# Start timer
print("Starting run.")
timer <- proc.time()

if (rangeAlpha == "true") {
  beta = 0.01
  
  for (alpha in (0.001 * 10^(1:4)) ) {
    data <- TmLDASimulation(cleanCorpus, storeFolder, k, alpha, beta, iter, iter, keep)
  }
} else {
  alpha = as.integer(divider)/as.integer(k)
  
  for (beta in (0.001 * 10^(1:4)) ) {
    data <- TmLDASimulation(cleanCorpus, storeFolder, k, alpha, beta, iter, iter, keep)
  }
}

print("Ending run.")
print(proc.time() - timer)