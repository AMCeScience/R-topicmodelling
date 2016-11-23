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
  alpha = as.integer(divider)/as.integer(k)
  beta = args[6]
  
  cleanCorpus <- readRDS(paste("data", CSVfileName, sep = "/"))
} else {
  print("Taking preset arguments.")
  
  workspace <- "~/workspace/R"
  
  setwd(workspace)
  
  # Load the config
  if (!exists("configLoaded")) source("config.R")
  
  storeFolder = "complete_2"
  
  cleanCorpus <- readRDS("data/complete_2/complete_2.rds")
#   source("preprocessing.R")
#   cleanCorpus <- runPreprocessing(CSVfileName, store = TRUE)
}

print("Corpus loaded.")

source("fit.R")

# Start timer
print("Starting run.")
timer <- proc.time()

data <- TmLDASimulation(cleanCorpus, storeFolder, k, alpha, beta, burnin, iter, thin, keep)
#data <- TmLDASimulation(cleanCorpus, folder k, alpha, beta, iter, iter, keep, thin, nstart)

print("Ending run.")
print(proc.time() - timer)