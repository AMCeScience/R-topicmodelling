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
  
  ks = seq(5,30,1)
  cores = 2
  
  storeFolder <- "tests"
  
  cleanCorpus <- readRDS("data/clean_corpus.rds")
}

print("Corpus loaded.")

library(coda)
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

# plot(c(1:500),store_list[[1]][[1]], type = "l", col = "red", xlab = "", ylab = "", ylim=c(-800000, -695000))
# title(ylab = "Log-Likelihood", line = 2.2, cex.lab = 1)
# title(xlab = "Iteration", line = 2.1, cex.lab = 1)
# lines(store_list[[1]][[2]], col = "green")
# lines(store_list[[1]][[3]], col = "blue")

highest_frame <- data.frame("T" = ks, "ll" = highest_lls)

saveRDS(store_list, "data/TM_LDA_LL.rds")
saveRDS(highest_frame, "data/TM_LDA_HIGHEST_LL.rds")



###################################################
# PLOTTING
###################################################

par(mfrow=c(1,2))

number_of_words <- 17644

data <- readRDS("data/sysrev/TM_LDA_HIGHEST_LL.rds")

y <- data["T"]

#BIC <- (-2 * data["ll"]) + (log(1300) * (y + (y * number_of_words)))
AIC <- -2 * data["ll"] + 2 * ((y - 1) + (y * (number_of_words - 1)))
#AIC <- (-2*data["ll"]) + (2*((y-1) + (y * (number_of_words - 1))))

cbind(data, AIC)

plot(cbind(y, AIC), type="o", xlab="", ylab="")
abline(v = 14, col = "red", lty = 2)
title(xlab = "Number of Topics", line = 2.2, cex.lab = 1)
title(ylab = "AIC", line = 2.3, cex.lab = 1)

data <- readRDS("data/TM_LDA_HIGHEST_LL.rds")

y <- data["T"]

#BIC <- (-2 * data["ll"]) + (log(1300) * (y + (y * number_of_words)))
AIC <- -2 * data["ll"] + 2 * ((y - 1) + (y * (number_of_words - 1)))
#AIC <- (-2*data["ll"]) + (2*((y-1) + (y * (number_of_words - 1))))

plot(cbind(y, AIC), type="o", xlab="", ylab="")
abline(v = 14, col = "red", lty = 2)
title(xlab = "Number of Topics", line = 2.2, cex.lab = 1)
title(ylab = "AIC", line = 2.3, cex.lab = 1)