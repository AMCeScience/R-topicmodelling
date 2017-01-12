#!/usr/bin/env Rscript
# https://stackoverflow.com/questions/18306362/run-r-script-from-command-line

# CALL
# ./cli-clean.R ~/workspace/R test_corp articles_sysrev_test

source("cli-input.R")
source("libraries/utils.R")

if (length(args) < 3) {
  stop("Corpus name or CSV name not provided")
}

corpus_name <- args[2]
csv_name <- args[3]

source("libraries/preprocessing.R")

setupPreprocessing(corpus_name, csv_name)