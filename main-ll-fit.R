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
vals <- list()

alphas = c(1, 0.9, 0.9, 0.85, 0.77, 0.73, 0.71, 0.69, 0.65, 0.6, 0.54, 0.51, 0.45, 0.41, 0.38, 0.31, 0.25, 0.21, 0.17, 0.13)

for (k in seq(5, 100, 5)) {
  a <- alphas[[count]]
  
  data <- TmLDASimulation(cleanCorpus, storeFolder, k, a, 0.01, burnin, iter, thin, keep, store = FALSE, multiple = TRUE)
  
#   holder <- list()
#   
#   for(item in data) {
#     q <- item@fitted[[1]]@logLiks
#     holder[[length(holder) + 1]] <- q
#   }
  
  store_list[[length(store_list) + 1]] <- data
  
  saveRDS(holder, gsub("__", k, paste("data", storeFolder, "TM_LDA_MCMC_LL_num__.rds", sep = "/")))
}

saveRDS(store_list, paste("data", storeFolder, "TM_LDA_MCMC_LL.rds", sep = "/"))

# for (data in store_list) {
#   mcmc_list <- list()
#   
#   for(item in data) {
#     mcmc_list[[length(mcmc_list) + 1]] <- mcmc(item[floor(1/2 * length(item)):length(item)])
#   }
#   
#   data_list <- list()
#   
#   for(item in data) {
#     data_list[[length(data_list) + 1]] <- matrix(item[floor(1/2 * length(item)):length(item)], nrow=1)
#   }
#   
#   plot_list <- list()
#   
#   for(item in data) {
#     plot_list[[length(plot_list) + 1]] <- item
#   }
#   
#   plot(unlist(plot_list), type="n", xlim=c(1,length(plot_list[[1]])))
#   mapply(lines, plot_list, col = seq_along(plot_list), lty=2)
#   
#   print(gelman.diag(mcmc_list))
#   
#   quit
# }

highest_lls <- list()

for (ll_lists in store_list) {
  all_lls <- list()
  
  for (ll_list in ll_lists) {
    all_lls <- c(all_lls, tail(ll_list, n = 1))
  }
  
  highest_lls <- c(highest_lls, max(ll_list))
}

d<-t(rbind(c(seq(5,100,5)), highest_lls))
plot(d, type="o", xlab="", ylab="", xlim=c(0,100))
title(xlab = "Number of Topics", line = 2.2, cex.lab = 1)
title(ylab = "Estimated Log Likelihood", line = 2.3, cex.lab = 1)

print("Ending run.")
print(proc.time() - timer)
