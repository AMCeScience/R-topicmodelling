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
  RDSfilename = args[3]
  ks = seq(5, 100, 5)
  cores = 7
  #k = args[4]
  #divider = args[5]
  #alpha = as.integer(divider)/as.integer(k)
  #beta = args[6]
  
  cleanCorpus <- readRDS(RDSfilename)
} else {
  print("Taking preset arguments.")
  
  workspace <- "~/workspace/R"
  
  setwd(workspace)
  
  # Load the config
  #if (!exists("configLoaded")) source("config.R")
  source("config.R")
  
  ks = seq(5, 10, 5)
  cores = 2
  
  storeFolder <- "tests"
  
  cleanCorpus <- readRDS("data/clean_corpus.rds")
  #cleanCorpus <- readRDS("data/clean_pubmed_ovid_pmc.rds")
  #source("preprocessing.R")
  #cleanCorpus <- runPreprocessing(CSVfileName, store = TRUE)
}

print("Corpus loaded.")

library(coda)
library(parallel)
source("fit.R")

# Start timer
print("Starting run.")
timer <- proc.time()

#store_list <- list()
#vals <- list()

#for (k in seq(5, 5, 5)) {

data <- mclapply(ks, function(k) TmLDASimulation(cleanCorpus, "", k, 50/k, 0.01, burnin, iter, thin, keep, store = FALSE, multiple = TRUE), mc.cores = cores, mc.silent = TRUE)  
  
saveRDS(data, paste("data", storeFolder, "TM_LDA_LL_FULL_FIT.rds", sep = "/"))
#   holder <- list()
#   
#   for(item in data) {
#     q <- item@fitted[[1]]@logLiks
#     holder[[length(holder) + 1]] <- q
#   }
#   
#   mcmc_list <- list()
#   
#   for(item in data) {
#     q <- item@fitted[[1]]@logLiks
#     mcmc_list[[length(mcmc_list) + 1]] <- mcmc(q[floor(1/2 * length(q)):length(q)])
#   }
#   
#   gelman.diag(mcmc_list)
#   
#   store_list[[length(store_list) + 1]] <- data
# 
#   saveRDS(data, gsub("__", k, paste("data", storeFolder, "TM_LDA_MCMC_LL_num__.rds", sep = "/")))
#}
#
#saveRDS(store_list, paste("data", storeFolder, "TM_LDA_MCMC_LL.rds", sep = "/"))
#
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
#     q <- item@fitted[[1]]@logLiks
#     plot_list[[length(plot_list) + 1]] <- q
#   }
#   
#   plot(unlist(plot_list), type="n", xlim=c(1,length(plot_list[[1]])))
#   mapply(lines, plot_list, col = seq_along(plot_list), lty=2)
#   
#   print(gelman.diag(mcmc_list))
#   
#   quit
# }
#
highest_lls <- list()

for (ll_lists in data) {
  all_lls <- list()
  
  for (ll_list in ll_lists) {
    ll_list <- ll_list@fitted[[1]]@logLiks
    
    all_lls <- c(all_lls, tail(ll_list, n = 1))
  }
  
  highest_lls <- c(highest_lls, max(ll_list))
}

saveRDS(highest_lls, paste("data", storeFolder, "TM_LDA_MCMC_LL.rds", sep = "/"))

# d<-t(rbind(c(seq(5,100,5)), highest_lls))
# plot(d, type="o", xlab="", ylab="", xlim=c(0,100))
# title(xlab = "Number of Topics", line = 2.2, cex.lab = 1)
# title(ylab = "Estimated Log Likelihood", line = 2.3, cex.lab = 1)

print("Ending run.")
print(proc.time() - timer)
