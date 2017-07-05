ngc_all <- read.csv("~/Dropbox/Public/uni/phd/CLEF/data/NGC curve numbers.csv")

ngc_dat <- read.csv("~/Dropbox/Public/uni/phd/CLEF/data/NGC curve numbers high.csv")

png("data/clef-images/recall-curve-high.png", width = 400, height = 400)

plot(seq(1, 100, 10), ngc_dat[1,2:ncol(ngc_dat)], type = "l", xlab = "", ylab = "")
title(ylab = "Recall", line = 2.2, cex.lab = 1.5)
title(xlab = "% of read documents", line = 2.6, cex.lab = 1.5)

for (i in 1:nrow(ngc_dat)) {
  dat <- ngc_dat[i,2:ncol(ngc_dat)]

  lines(seq(1, 100, 10), dat)
}

lines(seq(1, 100, 10), ngc_all[nrow(ngc_all),2:ncol(ngc_all)], col = "red", lwd = "2")

dev.off()

ngc_dat <- read.csv("~/Dropbox/Public/uni/phd/CLEF/data/NGC curve numbers low.csv")

png("data/clef-images/recall-curve-low.png", width = 400, height = 400)

plot(seq(1, 100, 10), ngc_dat[1,2:ncol(ngc_dat)], type = "l", xlab = "", ylab = "", cex.lab = 1.5)
title(ylab = "Recall", line = 2.2, cex.lab = 1.5)
title(xlab = "% of read documents", line = 2.6, cex.lab = 1.5)

for (i in 2:nrow(ngc_dat)) {
  dat <- ngc_dat[i,2:ncol(ngc_dat)]

  lines(seq(1, 100, 10), dat)
}

lines(seq(1, 100, 10), ngc_all[nrow(ngc_all),2:ncol(ngc_all)], col = "red", lwd = "2")

dev.off()

numbers <- c("25", "50", "75")
# numbers <- c("25")
colors <- c()
colors["25"] <- "black"
colors["50"] <- "red"
colors["75"] <- "blue"

rm(f1_df)

plot_done <- FALSE

for (number in numbers) {
  rfs <- readRDS(paste("data/clef-test/RF_CF_fit_k", number, "_1.rds", sep = ""))

  rocs <- lapply(rfs, function(rf) {
    return(rf$ROC)
  })

  f1s <- lapply(rfs, function(rf) {
    return(rf$F1)
  })

  f1s <- unlist(f1s)

  if (!exists("f1_df")) {
    f1_df <- data.frame(mean = mean(f1s), sd = sd(f1s))
  } else {
    f1_df <- rbind(f1_df, c(mean(f1s), sd(f1s)))
  }

  if (plot_done == FALSE) {
    png(paste("data/clef-images/rocs_plot.png"), width = 400, height = 400)

    plot(rocs[[1]], xlab = "", ylab = "")
    title(ylab = "Sensitivity", line = 2.2, cex.lab = 1.5)
    title(xlab = "Specificity", line = 3.2, cex.lab = 1.5)

    plot_done <- TRUE

    for (roc in rocs[2:length(rocs)]) {
      lines(roc, col = colors[[number]])
    }
  } else {
    for (roc in rocs) {
      lines(roc)
    }
  }
}

dev.off()

png("data/clef-images/mean_f1_and_sd_plot.png", height = 400, width = 400)

plot(1:nrow(f1_df), f1_df$mean, pch = 19, ylim = c(0,0.1), xlab = "", ylab = "", xaxt = "n")
title(ylab = "F1-Measure", line = 2.2, cex.lab = 1.5)
title(xlab = "# of topics", line = 2.6, cex.lab = 1.5)

arrows(1:nrow(f1_df), f1_df$mean + f1_df$sd, 1:nrow(f1_df), f1_df$mean - f1_df$sd,
       code = 3, angle = 90, length = 0.05)

axis(side = 1, at = 1:nrow(f1_df), labels = numbers)

dev.off()