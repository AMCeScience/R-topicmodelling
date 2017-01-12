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
  store_folder = args[2]
  csv_filename = args[3]
  k = args[4]
  divider = args[5]
  alpha = as.integer(divider)/as.integer(k)
  beta = args[6]
  
  clean_corpus <- readRDS(paste("data", csv_filename, sep = "/"))
} else {
  print("Taking preset arguments.")
  
  workspace <- "~/workspace/R"
  
  setwd(workspace)
  
  # Load the config
  if (!exists("configLoaded")) source("config.R")
  
  store_folder = "complete_2"
  
  clean_corpus <- readRDS("data/complete_2/complete_2.rds")
}

print("Corpus loaded.")

source("fit.R")

# Start timer
print("Starting run.")
timer <- proc.time()

data <- TmLDASimulation(clean_corpus, store_folder, k, alpha, beta, burnin, iter, thin, keep)
#data <- TmLDASimulation(clean_corpus, folder k, alpha, beta, iter, iter, keep, thin, nstart)

print("Ending run.")
print(proc.time() - timer)