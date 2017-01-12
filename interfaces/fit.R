source("libraries/utils.R")
source("libraries/fit.R")

execute <- function(project_name, ks, divider = fit_divider, beta = fit_beta) {
  corpus_location <- getProjectFolder(project_name)
  
  file_version <- getLastVersion("clean_corpus", corpus_location)
  corpus_filename <- paste("clean_corpus_", file_version, sep = "")
  
  clean_corpus <- readRDS(paste(corpus_location, "/", corpus_filename, ".rds", sep = ""))
  
  library(parallel)
  
  model_fit_data <- mclapply(
    ks,
    function(k) TmLDASimulation(clean_corpus, project_name, file_version, k, divider/k, beta, fit_burnin, fit_iter, fit_thin, fit_keep),
    mc.cores = parallel_cores,
    mc.silent = parallel_silent
  )
}