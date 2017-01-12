#!/usr/bin/env Rscript
# https://stackoverflow.com/questions/18306362/run-r-script-from-command-line

# Run from CLI or with source("ui/ui-clean.R")

setwd("~/workspace/R")

source("config.R")
source("libraries/utils.R")

corpus_name <- ask("Corpus filename?: ")
csv_name <- ask("CSV filename?: ")

source("libraries/preprocessing.R")

setupPreprocessing(corpus_name, csv_name)