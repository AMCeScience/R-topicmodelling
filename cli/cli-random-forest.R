#!/usr/bin/env Rscript
# https://stackoverflow.com/questions/18306362/run-r-script-from-command-line

# CALL
# ./cli-random-forest.R ~/workspace/R test_corp

source("cli-input.R")

if (length(args) < 2) {
  stop("Not all arguments provided.")
}

project_name <- args[2]

source("interfaces/random-forest.R")

execute(project_name, "rf_selection.R")