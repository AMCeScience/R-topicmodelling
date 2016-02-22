#!/usr/bin/env Rscript
# https://stackoverflow.com/questions/18306362/run-r-script-from-command-line

source("fit.R")

run1 <- TmLDASimulation(cleanCorpus, 4, alpha, beta, iter, iter, keep)
run2 <- TmLDASimulation(cleanCorpus, 4, alpha, beta, iter, iter, keep)

source("KL-distance.R")

res <- KLdistFromRunResults(run1, run2, minimialise = FALSE)

orderedRes <- KLorder(res)

source("doc-generator.R")
