#!/usr/bin/env Rscript
# https://stackoverflow.com/questions/18306362/run-r-script-from-command-line

# CALL
# ./cli-analysis.R ~/workspace/R test_corp

source("cli-input.R")
source("libraries/utils.R")
library("caret")
library("pracma")

if (length(args) < 2) {
  stop("Project name not provided")
}

number_topics <- 2
auc_proportion <- 0.5

project_folder <- getProjectFolder(args[2])
version <- getLastVersion("RF_CF", project_folder)
files <- getAllOfVersion("RF_CF", project_folder, version)

indiceList <- function() {
  complete_indices_list <- c()

  for (file in files) {
    data <- readRDS(paste(project_folder, file, sep = "/"))

    k <- gsub("RF_CF_fit_k([0-9]*)_[0-9]*.rds", "\\1", file)

    top_indices_list <- list()
    # var_imp_list <- c()

    for (fold in 1:length(data)) {
      imp <- varImp(data[[fold]]$rf)$importance

      imp_values <- imp[order(imp, decreasing = TRUE),]

      total_auc <- trapz(1:length(imp_values), imp_values)
      number_of_indices <- 0

      for (i in 2:10) {
        partial_auc <- trapz(1:i, imp_values[1:i])

        if (partial_auc <= total_auc * auc_proportion) {
          number_of_indices <- i
        }
      }

      top_indices <- which(imp >= sort(imp[1:nrow(imp),], decreasing = TRUE)[number_topics])
      # top_indices <- which(imp >= sort(imp[1:nrow(imp),], decreasing = TRUE)[number_of_indices])

      if (fold > 1) {
        lines(imp_values)
      } else {
        png(paste(project_folder, "/var_imp_runoff_k", k, ".png", sep = ""))
        plot(imp_values, type = "l")
      }

      top_indices_list <- c(top_indices_list, list(top_indices))
    }

    complete_indices_list <- c(complete_indices_list, list(top_indices_list))

    saveRDS(top_indices_list, paste(project_folder, "/top_indices_k", k, ".rds", sep = ""))
  }

  return(complete_indices_list)
}

writeToExcel <- function(indices_list) {
  library(openxlsx)
  source("postprocessing/relevance-calc.R")

  files <- getAllOfVersion("LDA_fit", project_folder, version)

  count <- 1

  for (file in files) {
    indices <- indices_list[[count]][[1]]

    topic_data <- readRDS(paste(project_folder, file, sep = "/"))

    relevance_data <- relevanceCalculation(topic_data, 30, FALSE, FALSE)

    data <- calcRelevancies(relevance_data)

    k <- gsub("LDA_fit_k([0-9]*).*.rds", "\\1", file)

    filename <- paste(project_folder, "/relevancies_k", k, ".xlsx", sep = "")

    # Create an Excel object
    wb <- createWorkbook(creator = Sys.getenv("USERNAME"))
    addWorksheet(wb, "data")
    writeData(wb, "data", data, colNames = FALSE)

    # Add background color to the topics that have a var importance higher proportion
    for (i in 1:length(indices)) {
      last_index <- indices[[i]] * 2
      first_index <- last_index - 1
      addStyle(wb, "data", createStyle(fgFill = "#A0A000"), 1:30, first_index)
      addStyle(wb, "data", createStyle(fgFill = "#A0A000"), 1:30, last_index)
    }

    # Write Excel object to file
    saveWorkbook(wb, filename, overwrite = TRUE)

    count <- count + 1
  }
}

calcRelevancies <- function(data) {
  source("postprocessing/phi-calculations.R")
  source("postprocessing/theta-calculations.R")

  size <- length(data[,1]) # number of words
  topics <- max(data[,2]) # number of topics
  chunkSize <- size/topics

  relevanceCols <- data[,grepl("relevance", colnames(data))]

  recalcRelevance <- apply(relevanceCols, 2, function(x) { max(x) / x })

  rels <- matrix(data = NA, nrow = length(recalcRelevance[,1]), ncol = 1)
  colCount = 1

  for (i in 1:length(recalcRelevance[,1])) {
    rels[i] = recalcRelevance[i, colCount]

    if (i %% chunkSize == 0) {
      colCount = colCount + 1
    }
  }

  # Take the text, bind the relevancies
  #impData <- data[,c(1,2)]
  impData <- cbind(data[,c(1,2)], rels)

  # Split the data into chunks and put into frame
  impression <- data.frame(split(impData, impData$Category))

  impressionFilter <- impression[, -grep("Category", colnames(impression))]

  return(impressionFilter)
}

indice_data <- indiceList()
writeToExcel(indice_data)