# Clear workspace
rm(list = ls())

workspace <- "~/workspace/R"

# Set workspace to folder where articles.csv is placed
setwd(workspace)

# Load the config
if (!exists("configLoaded")) source("config.R")

# if (file.exists("data/clean_corpus.rds")) cleanCorpus <- readRDS("data/clean_corpus.rds")
if (!exists("cleanCorpus")) {
  source("preprocessing.R")
  cleanCorpus <- runPreprocessing(CSVfileName, store = TRUE)
}

source("fit.R")

run1 <- TmLDASimulation(cleanCorpus, 4, alpha, beta, iter, iter, keep)
run2 <- TmLDASimulation(cleanCorpus, 4, alpha, beta, iter, iter, keep)

source("KL-distance.R")

res <- KLdistFromRunResults(run1, run2, minimialise = FALSE)

orderedRes <- KLorder(res)

source("doc-generator.R")
