#!/usr/bin/env Rscript
# https://stackoverflow.com/questions/18306362/run-r-script-from-command-line

# Clear workspace
rm(list = ls())

workspace <- "~/workspace/R"

# Set workspace to folder where articles.csv is placed
setwd(workspace)

# Load the config
if (!exists("configLoaded")) source("config.R")

if (file.exists("data/clean_corpus.rds")) cleanCorpus <- readRDS("data/clean_corpus.rds")
if (!exists("cleanCorpus")) {
  source("preprocessing.R")
  cleanCorpus <- runPreprocessing(CSVfileName, store = TRUE)
}