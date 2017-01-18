FMeasure <- function(precision, recall, beta = 1) {
  return((1 + beta^2) * ((precision * recall) / ((beta^2 * precision) + recall)))
}

getData <- function(data_location, datasets, folds = TRUE) {
  x <- c()
  y <- c()
  sd <- c()

  precision <- c()
  F1 <- c()
  reduction <- c()

  sizeTrue <- function(x) {
    return(length(x[x == TRUE]))
  }

  # Loop all the data sets
  for (dataset in datasets) {
    topic_data <- readRDS(paste(data_location, dataset, sep = "/"))

    recall_list <- c()
    precision_list <- c()
    F1_list <- c()
    reduction_list <- c()

    # Loop all the folds
    if (folds) {
      for (j in 1:length(topic_data)) {
        recall_list <- c(recall_list, topic_data[[j]]$recall)
        precision_list <- c(precision_list, topic_data[[j]]$precision)
        F1_list <- c(F1_list, FMeasure(topic_data[[j]]$precision, topic_data[[j]]$recall, 3))
        reduction_list <- c(reduction_list, (topic_data[[j]]$TP + topic_data[[j]]$FP) / (sizeTrue(topic_data[[j]]$base_positives) + sizeTrue(topic_data[[j]]$base_negatives)))
      }

      coeff_names <- topic_data[[1]]$rf$coefnames
    } else {
      recall_list <- c(recall_list, topic_data$recall)
      precision_list <- c(precision_list, topic_data$precision)
      F1_list <- c(F1_list, FMeasure(topic_data$precision, topic_data$recall, 3))
      reduction_list <- c(reduction_list, (topic_data$TP + topic_data$FP) / (sizeTrue(topic_data$base_positives) + sizeTrue(topic_data$base_negatives)))

      coeff_names <- topic_data$rf$coefnames
    }

    x <- c(x, length(coeff_names))
    y <- c(y, mean(recall_list))
    sd <- c(sd, sd(recall_list))
    precision <- c(precision, mean(precision_list))
    F1 <- c(F1, mean(F1_list))
    reduction <- c(reduction, 1 - mean(reduction_list))
  }

  return(data.frame(x = x, y = y, sd = sd, precision = precision, F1 = F1, reduction = reduction))
}

plotThis <- function(data_location, file_version, x, y, sd, F1, reduction) {
  plot_frame <- data.frame(x = x, y = y, sd = sd)

  suppressMessages(library("Hmisc"))

  if (rfa_store == TRUE) {
    png(paste(data_location, "/RF_analysis_plot_", file_version, ".png", sep = ""))
  }

  with(
    data = plot_frame,
    expr = errbar(x, y, y + sd, y - sd, add = F, pch = 1, cap = .015, xlab = '', ylab = '', ylim = c(0,1.1))
  )
  # title(main = "Plot")
  title(ylab = "Recall", line = 2.2, cex.lab = 1)
  title(xlab = "# Topics", line = 2.1, cex.lab = 1)
  lines(plot_frame$x, F1, col = 'red')
  lines(plot_frame$x, reduction, col = 'blue')

  if (rfa_store == TRUE) {
    dev.off()
  }
}

setupForestAnalysis <- function(data_location, file_version, folds = TRUE) {
  # DIRECTORY TESTING
  if (!dir.exists(data_location)) {
    stop("Data directory does not exist")
  }

  if (rf_force && dir.exists(data_location)) {
    print("Writing directory already exists")

    print("OVERWRITING")
    overwriteFiles("RF_analysis_plot", data_location, file_version)
  }

  # CHECK FOR EXISTING ANALYSIS
  existing_cf_fits <- getAllOfVersion("RF_CF", data_location, file_version)
  existing_sf_fits <- getAllOfVersion("RF_SF", data_location, file_version)
  existing_analysis <- getAllOfVersion("RF_analysis_plot", data_location, file_version)

  if (length(existing_cf_fits) + length(existing_sf_fits) != length(existing_analysis)) {
    overwriteFiles("RF_analysis_plot", data_location, file_version)

    datasets <- c(existing_cf_fits, existing_sf_fits)

    data <- getData(data_location, datasets, folds)

    plotThis(data_location, file_version, data$x, data$y, data$sd, data$F1, data$reduction)
  }
}