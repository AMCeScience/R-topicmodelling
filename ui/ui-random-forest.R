#!/usr/bin/env Rscript
# https://stackoverflow.com/questions/18306362/run-r-script-from-command-line

# Run from CLI or with source("ui/ui-random-forest.R")

project_name <- ask("Project name?: ")

source("interfaces/random-forest.R")

execute(project_name, "rf_selection.R")