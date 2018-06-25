#!/usr/bin/env Rscript
# https://stackoverflow.com/questions/18306362/run-r-script-from-command-line

# CALL
# ./cli-workflow-glmnet-years.R ~/workspace/R

locations <- c("2011_2014", "2015", "2016", "2017", "2018")

source("cli-input.R")
source("libraries/utils.R")

# CLEANING ----------------------------------------------

# OVERWRITE CONFIG DEFAULTS
clean_force <- FALSE

source("libraries/preprocessing.R")

for (loc in locations) {
  selection_file <- "rf_selection.R"

  project_name <- paste("new_data_", loc, sep = "")

  project_location <- paste("data", project_name, sep = "/")

  # Load 'includes' variable
  source(paste(project_location, selection_file, sep = "/"))

  # preprocessFolder(project_name, paste("originals/datasets/", loc, sep = ""), includes)

  # FITTING ----------------------------------------------

  source("lm-fit.R")

  start(project_name)
}