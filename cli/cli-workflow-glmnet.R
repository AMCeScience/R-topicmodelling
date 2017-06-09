#!/usr/bin/env Rscript
# https://stackoverflow.com/questions/18306362/run-r-script-from-command-line

# CALL
# ./cli-workflow.R ~/workspace/R test_corp articles_sysrev_test

source("cli-input.R")
source("libraries/utils.R")

if (length(args) < 3) {
  stop("Corpus name or CSV name not provided")
}

# CLEANING ----------------------------------------------

project_name <- args[2]
csv_name <- args[3]

# OVERWRITE CONFIG DEFAULTS
clean_force <- FALSE

source("libraries/preprocessing.R")

selection_file <- "rf_selection.R"

project_location <- getProjectFolder(project_name)
file_version <- getLastVersion("clean_corpus", project_location)

# Load 'includes' variable
source(paste(project_location, selection_file, sep = "/"))

# Pre-process the corpus
corpus <- setupPreprocessing(project_name, csv_name, includes)

if (workflow_run_to == "cleaning") {
  stop()
}

# FITTING ----------------------------------------------

source("lm-fit.R")
