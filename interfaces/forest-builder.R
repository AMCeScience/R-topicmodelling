source("libraries/utils.R")
source("libraries/random-forest-builder.R")

executeForest <- function(project_name, corpus) {
  suppressMessages(library(tm))

  project_location <- getProjectFolder(project_name)
  file_version <- getLastVersion("clean_corpus", project_location)

  dtm <- DocumentTermMatrix(corpus)

  minimized_dtm <- removeSparseTerms(dtm, sparse = 0.99)

  includes <- vector("list")

  for (i in 1:length(corpus$content)) {
    if (corpus$content[[i]]$meta$included) {
      includes <- c(includes, i)
    }
  }

  includes <- unlist(includes)

  result <- setupForest(minimized_dtm, includes, project_location, file_version, rf_fold, training_selection)

  return(result)
}