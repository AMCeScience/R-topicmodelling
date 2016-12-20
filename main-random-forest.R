#!/usr/bin/env Rscript
# https://stackoverflow.com/questions/18306362/run-r-script-from-command-line

# Clear workspace
rm(list = ls())

args <- commandArgs(trailingOnly = TRUE)

source("includes/contrast.R")

source("random-forest-builder.R")

if (length(args) > 0) {
  print("Taking cli arguments.")
  
  workspace = args[1]
  folder = args[2]
  cores <- 7
  #datasets <- c(5, 10, 15, 20, 25, 30, 35, 40, 45, 50, 55, 60, 65, 70, 75, 80, 85, 90, 95, 100, 150, 200, 250, 300, 350, 400, 450, 500)
  #datasets <- c(20, 25, 30, 40, 50, 75, 100)
  folds <- 1:10
  store <- TRUE
  
  print(paste("Changing to workspace:", workspace))
  
  setwd(workspace)
  
  mclapply(datasets, function(set) runDataSet(set, folder, store), mc.cores = cores, mc.silent = TRUE)
} else {
  print("Taking preset arguments.")
  
  workspace <- "~/workspace/R"
  folder = 'data/contrast_all'
  cores <- 2
  datasets <- c(10)
  folds <- 1:1
  store <- FALSE
  
  setwd(workspace)
  
  data <- runDataSet(25, folder, store)
}