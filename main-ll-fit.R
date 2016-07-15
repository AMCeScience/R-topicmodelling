#!/usr/bin/env Rscript
# https://stackoverflow.com/questions/18306362/run-r-script-from-command-line

# Clear workspace
rm(list = ls())

args <- commandArgs(trailingOnly = TRUE)

if (length(args) > 0) {
  print("Taking cli arguments.")
  
  workspace = args[1]
  
  print(paste("Changing to workspace:", workspace))
  
  setwd(workspace)
  
  # Load the config
  if (!exists("configLoaded")) source("config.R")
  
  # Overwrite the defaults
  storeFolder = args[2]
  CSVfileName = args[3]
  #k = args[4]
  #divider = args[5]
  #alpha = as.integer(divider)/as.integer(k)
  #beta = args[6]
  
  cleanCorpus <- readRDS(paste("data", CSVfileName, sep = "/"))
} else {
  print("Taking preset arguments.")
  
  workspace <- "~/workspace/R"
  
  setwd(workspace)
  
  # Load the config
  if (!exists("configLoaded")) source("config.R")
  
  k = 100
  alpha = 50/k
  
  storeFolder <- "tests"
  
  cleanCorpus <- readRDS("data/clean_corpus.rds")
  #source("preprocessing.R")
  #cleanCorpus <- runPreprocessing(CSVfileName, store = TRUE)
}

print("Corpus loaded.")

source("fit.R")

# Start timer
print("Starting run.")
timer <- proc.time()

store_list <- list()

for (k in seq(5, 150, 5)) {
  alpha = 50/k
  beta = 0.01
  
  data <- TmLDASimulation(cleanCorpus, storeFolder, k, alpha, beta, burnin, iter, thin, keep, store = FALSE, multiple = TRUE)
  
  holder <- list()
  
  for(item in data) {
    q <- item@fitted[[1]]@logLiks
    holder[[length(holder) + 1]] <- q
  }
  
  store_list[[length(store_list) + 1]] <- holder
}

saveRDS(store_list, paste("data", storeFolder, "TM_LDA_MCMC_LL.rds", sep = "/"))

# mcmc_list <- list()
# 
# for(item in data) {
#   q <- item@fitted[[1]]@logLiks
#   mcmc_list[[length(mcmc_list) + 1]] <- mcmc(q[floor(1/2 * length(q)):length(q)])
# }
#
# data_list <- list()
# 
# for(item in data) {
#   q <- item@fitted[[1]]@logLiks
#   data_list[[length(data_list) + 1]] <- matrix(q[floor(1/2 * length(q)):length(q)], nrow=1)
# }
# 
# plot_list <- list()
# 
# for(item in data) {
#   q <- item@fitted[[1]]@logLiks
#   plot_list[[length(plot_list) + 1]] <- q
# }
# 
# plot(unlist(plot_list), type="n", xlim=c(1,length(data[[1]]@fitted[[1]]@logLiks)))
# mapply(lines, plot_list, col = seq_along(plot_list),lty=2)

print("Ending run.")
print(proc.time() - timer)