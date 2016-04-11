#!/usr/bin/env Rscript
# https://stackoverflow.com/questions/18306362/run-r-script-from-command-line

# Clear workspace
rm(list = ls())

args <- commandArgs(trailingOnly = TRUE)

corpus_name = args[1]
csv_name = args[2]

workspace <- "~/workspace/R"

# Set workspace to folder where articles.csv is placed
setwd(workspace)

# Load the config
if (!exists("configLoaded")) source("config.R")

if (file.exists(paste("data", corpus_name, sep = "/"))) cleanCorpus <- readRDS(paste("data", corpus_name, sep = "/"))
if (!exists("cleanCorpus")) {
  source("preprocessing.R")
  cleanCorpus <- runPreprocessing(csv_name, store = TRUE)
}