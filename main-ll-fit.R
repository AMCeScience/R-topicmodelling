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
  ks = c(seq(5, 100, 5), seq(150, 500, 50))
  cores = 7
  #k = args[4]
  #divider = args[5]
  #alpha = as.integer(divider)/as.integer(k)
  #beta = args[6]
  
  cleanCorpus <- readRDS(paste("originals", RDSfilename, sep = "/"))
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
}

print("Corpus loaded.")

library(parallel)
source("fit.R")

# Start timer
print("Starting run.")
timer <- proc.time()

data <- mclapply(ks, function(k) TmLDASimulation(cleanCorpus, "", k, 50/k, 0.01, burnin, iter, thin, keep, store = FALSE, multiple = TRUE), mc.cores = cores, mc.silent = TRUE)

store_list <- list()
highest_lls <- c()

for (models in data) {
  mcmc_list <- list()
  plot_list <- list()
  highest_ll <- 0
  
  for(model in models) {
    q <- model@fitted[[1]]@logLiks
    
    mcmc_list[[length(mcmc_list) + 1]] <- mcmc(q[floor(1/2 * length(q)):length(q)])
    plot_list[[length(plot_list) + 1]] <- q
    
    last_ll <- tail(q, n = 1)
    
    if (last_ll * -1 > highest_ll) {
      highest_ll <- last_ll
    }
  }
  
  #plot(unlist(plot_list), type="n", xlim=c(1,length(plot_list[[1]])))
  #mapply(lines, plot_list, col = seq_along(plot_list), lty=2)
  
  #print(data[[1]]@fitted[[1]]@k)
  #print(gelman.diag(mcmc_list))
  
  store_list[[length(store_list) + 1]] <- plot_list
  highest_lls <- c(highest_lls, highest_ll)
}

saveRDS(store_list, "data/TM_LDA_LL.rds")
saveRDS(highest_lls, "data/TM_LDA_HIGHEST_LL.rds")

#data1 <- unlist(readRDS("data/tests/1TM_LDA_MCMC_LL.rds")) # 5 - 100, 5
#data2 <- unlist(readRDS("data/tests/2TM_LDA_MCMC_LL.rds")) # 150 - 500, 50
#data3 <- unlist(readRDS("data/tests/3TM_LDA_MCMC_LL.rds")) # 600 - 1000, 100

#data <- c(data1,data2,data3)

# y <- c(seq(5,100,5), seq(110,180,10), seq(200,400,50), seq(500,700,100))
#y <- c(seq(5,100,5), seq(150,500,50), seq(600,1000,100))

# bla <- data - ((y/2) * 1308)
# bla <- (2 * y) - (2 * data)
# 
# d<-t(rbind(y, bla))
# plot(d, type="o", xlab="", ylab="", xlim=c(0,1005))
# title(xlab = "Number of Topics", line = 2.2, cex.lab = 1)
# title(ylab = "BIC", line = 2.3, cex.lab = 1)
# 
# data <- c(data4)
# 
# y <- c(seq(200,300,5))
# 
# d<-t(rbind(y, data))
# plot(d, type="o", xlab="", ylab="", xlim=c(195,305))
# title(xlab = "Number of Topics", line = 2.2, cex.lab = 1)
# title(ylab = "Estimated Log Likelihood", line = 2.3, cex.lab = 1)

print("Ending run.")
print(proc.time() - timer)
