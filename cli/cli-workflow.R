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

clean_corpus <- setupPreprocessing(project_name, csv_name)

if (workflow_run_to == "cleaning") {
  stop()
}

# FITTING -----------------------------------------------

# Overwrite the defaults
#fit_ks <- seq(2, 4, 1)
fit_divider <- 50
fit_beta <- 0.01

project_location <- getProjectFolder(project_name)
file_version <- getLastVersion("clean_corpus", project_location)

source("interfaces/fit.R")

library(parallel)

datasets <- mclapply(
  fit_ks,
  function(k) setupFitting(clean_corpus, project_name, file_version, k, fit_divider/k, fit_beta, fit_burnin, fit_iter, fit_thin, fit_keep),
  mc.cores = parallel_cores,
  mc.silent = parallel_silent
)

if (workflow_run_to == "fitting") {
  stop()
}

# RANDOM FOREST -----------------------------------------

source("libraries/random-forest-builder.R")

selection_file <- "rf_selection.R"
#training_selection <- c(24, 10, 26, 11, 1, 22, 18, 15, 12, 25, 7, 2, 19)

# Load 'includes' variable
source(paste(project_location, selection_file, sep = "/"))

runSet <- function(dataset) {
  results <- setupForest(dataset, includes, project_location, file_version, rf_fold, training_selection)

  return(results)
}

results <- mclapply(datasets, function(set) runSet(set), mc.cores = parallel_cores, mc.silent = parallel_silent)

# RANDOM FOREST ANALYSER --------------------------------

source("libraries/random-forest-analyser.R")

results <- setupForestAnalysis(project_location, file_version, rf_fold)