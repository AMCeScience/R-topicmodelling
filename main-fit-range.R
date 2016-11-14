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
  ks = c(seq(5, 100, 5), seq(150, 500, 50))
  cores = 7
  
  cleanCorpus <- readRDS(paste("originals", RDSfilename, sep = "/"))
} else {
  print("Taking preset arguments.")
  
  workspace <- "~/workspace/R"
  
  setwd(workspace)
  
  # Load the config
  #if (!exists("configLoaded")) source("config.R")
  source("config.R")
  
  ks = seq(5,30,1)
  cores = 2
  
  storeFolder <- "tests"
  
  cleanCorpus <- readRDS("data/clean_corpus.rds")
}

print("Corpus loaded.")

library(coda)
library(parallel)
source("fit.R")

# Start timer
print("Starting run.")
timer <- proc.time()

mclapply(ks, function(k) TmLDASimulation(cleanCorpus, storeFolder, k, 50/k, 0.01, burnin, iter, thin, keep, store = TRUE), mc.cores = cores, mc.silent = TRUE)