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
  RDSfilename = args[3]
  
  cores = 7
  
  cleanCorpus <- readRDS(paste("data", RDSfilename, sep = "/"))
} else {
  print("Taking preset arguments.")
  
  workspace <- "~/workspace/R"
  
  setwd(workspace)
  
  # Load the config
  source("config.R")
  
  cores = 2
  
  storeFolder <- "tests"
  
  cleanCorpus <- readRDS("data/clean_corpus.rds")
}

library(coda)
library(parallel)
source("fit.R")

k = 250
beta = 0.01
alphas = c(0.001, 0.005, 0.01, 0.05, 0.1, 0.2, 0.3)

data <- mclapply(alphas, function(alpha) TmLDASimulation(cleanCorpus, "", k, alpha, beta, burnin, iter, thin, keep, store = FALSE, multiple = TRUE), mc.cores = cores, mc.silent = TRUE)  

saveRDS(data, paste(storeFolder, "TM_LDA_LL_ALPHA_FIT.rds", sep = "/"))