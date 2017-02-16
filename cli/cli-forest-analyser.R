#!/usr/bin/env Rscript
# https://stackoverflow.com/questions/18306362/run-r-script-from-command-line

# CALL
# ./cli-forest-analyser.R ~/workspace/R test_corp TRUE

source("cli-input.R")
source("interfaces/forest-analyser.R")

if (length(args) < 3) {
  stop("Not all arguments provided.")
}

project_name <- args[2]
folds <- as.logical(args[3])

results <- execute(project_name, folds)