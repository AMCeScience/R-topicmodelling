#!/usr/bin/env Rscript
# https://stackoverflow.com/questions/18306362/run-r-script-from-command-line

# Run from CLI or with source("ui/ui-fit.R")

# Overwrite the defaults
project_name <- ask("Project name?: ")
ks <- as.numeric(unlist(strsplit(ask("Number of topics (in CSV)?: "), ",")))
divider <- as.numeric(ask("Alpha divider (alpha = K/divider)?: "))
beta <- as.numeric(ask("Beta?: "))

source("interfaces/fit.R")

execute(project_name, ks, divider, beta)