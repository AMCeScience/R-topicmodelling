#!/usr/bin/env Rscript
# https://stackoverflow.com/questions/18306362/run-r-script-from-command-line

# Clear workspace
rm(list = ls())

options(echo = TRUE)
args <- commandArgs(trailingOnly = TRUE)

if (length(args) > 0) {
  print("Taking cli arguments.")
  
  workspace = args[1]
  
  # Load the config
  if (!exists("configLoaded")) source("config.R")
  
  print(paste("Changing to workspace:", workspace))
  
  setwd(workspace)
  
  cores = 7
  ks = c(3, 4, 5, 6, 7, 8, seq(10, 100, 5))
  keep = 50
  corpus_name = args[2]
} else {
  print("Taking preset arguments.")
  
  workspace <- "~/workspace/R"
  
  setwd(workspace)
  
  # Load the config
  if (!exists("configLoaded")) source("config.R")
  
  burnin = 800
  iter = 4000
  cores = 2
  ks = c(4,5,6,7,8,9,10)
  keep = 50
  corpus_name = "clean_corpus.rds"
}

library(tm)
library(parallel)

#if (file.exists(paste("data", corpus_name, sep = "/"))) cleanCorpus <- readRDS(paste("data", corpus_name, sep = "/"))
#if (!exists("cleanCorpus")) return

print(paste("Using corpus: ", corpus_name))

library(topicmodels)

library(Rmpfr)
harmonicMean <- function(logLikelihoods, precision=2000L) {
  llMed <- median(logLikelihoods)
  as.double(llMed - log(mean(exp(-mpfr(logLikelihoods, prec = precision) + llMed))))
}

source("fit.R")

timer <- proc.time()

data("AssociatedPress")

cleanCorpus = AssociatedPress

#print(cleanCorpus[[1]]$content)

# Solution: https://stackoverflow.com/questions/21355156/topic-models-cross-validation-with-loglikelihood-or-perplexity
all_fitted <- mclapply(ks, function(k) TmLDASimulation(cleanCorpus, "", k, alpha, beta, burnin = burnin, iter = iter, keep = keep, store = FALSE), mc.cores = cores, mc.silent = TRUE)
# all_fitted <- lapply(ks, function(k) { train <- LDA(dtm_train, k = k, method = "Gibbs", control = list(burnin = burnin, iter = iter, keep = keep)) })

all_logLiks <- mclapply(all_fitted, function(L) L@logLiks[-c(1:(burnin/keep))], mc.cores = cores, mc.silent = TRUE)

all_hm <- unlist(mclapply(all_logLiks, harmonicMean, mc.cores = cores, mc.silent = TRUE))

print(proc.time() - timer)

saveRDS(all_hm, gsub("__", gsub(".rds", "", corpus_name), "data/harmonic-means-__.rds"))

harmonicMeansPlot <- function(corpus_name) {
  data <- readRDS(gsub("__", corpus_name, "data/sysrev/harmonic-means-__.rds"))
  
  ks = c(3, 4, 5, 6, 7, 8, seq(10, 100, 5))
  
  dataMeans <- data.frame("T" = ks)
  dataMeans[,"Harmonic Mean"] <- data / 1000
  
  plot(dataMeans, xlab = "", ylab = "")
  title(xlab = "T", line = 2.3, cex.lab = 1)
  title(ylab = "Harmonic Mean (x1000)", line = 2.4, cex.lab = 1)
}