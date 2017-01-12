source("libraries/utils.R")
source("libraries/random-forest-builder.R")

execute <- function(project_name, selection_file, folds = TRUE, file_version = FALSE) {
  datasets_location <- getProjectFolder(project_name)
  
  if (file_version == FALSE) {
    file_version <- getLastVersion("LDA_fit", datasets_location)
  }
  
  datasets <- getAllOfVersion("LDA_fit", datasets_location, file_version)
  
  source(paste(datasets_location, selection_file, sep = "/"))
  
  runSet <- function(set_filename) {
    dataset <- readRDS(paste(datasets_location, "/", set_filename, sep = ""))
    
    if (folds == TRUE) {
      results <- crossFoldForest(dataset, includes, datasets_location, file_version, dataset$numberOfTopics)
    } else {
      results <- singleFoldForest(dataset, includes, training_selection, datasets_location, file_version, dataset$numberOfTopics)
    }
    
    return(results)
  }
  
  library(parallel)
  
  results <- mclapply(datasets, function(set) runSet(set), mc.cores = parallel_cores, mc.silent = parallel_silent)
  
  return(results)
}