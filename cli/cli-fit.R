#!/usr/bin/env Rscript
# https://stackoverflow.com/questions/18306362/run-r-script-from-command-line

# CALL
# ./cli-fit.R ~/workspace/R test_corp 2,3,4 50 0.01

source("cli-input.R")

if (length(args) < 5) {
  stop("Not all arguments provided.")
}

# Overwrite the defaults
project_name <- args[2]
ks <- as.numeric(unlist(strsplit(args[3], ",")))
divider <- as.numeric(args[4])
beta <- as.numeric(args[5])

source("interfaces/fit.R")

execute(project_name, ks, divider, beta)