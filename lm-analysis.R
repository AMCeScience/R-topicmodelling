library("tm")
library("stats")

# project_location <- c("contrast-all", "contrast-2011", "contrast-2014",
#                       "contrast-2015", "contrast-2016")
project_location <- c("contrast-all", "contrast-2011", "contrast-2014", "contrast-2015", "contrast-2016")
folder <- "data/lm-analysis"

fold_cutoff <- 6

# ######################### CORPUS TOKEN COUNTS #########################

token_counts <- vector("list")
bd_token_counts <- vector("list")
nbd_token_counts <- vector("list")

for (location in project_location) {
  corpus <- readRDS(paste("data/", location, "/clean_corpus_1.rds", sep = ""))

  this_count <- 0
  this_bd_count <- 0
  this_nbd_count <- 0

  for (i in 1:length(corpus)) {
    tokens <- strsplit(corpus[[i]]$content, " ")

    this_count <- this_count + length(tokens[[1]])

    if (corpus[[i]]$meta$included == TRUE) {
      this_bd_count <- this_bd_count + length(tokens[[1]])
    } else {
      this_nbd_count <- this_nbd_count + length(tokens[[1]])
    }
  }

  token_counts[[location]] <- this_count
  bd_token_counts[[location]] <- this_bd_count
  nbd_token_counts[[location]] <- this_nbd_count
}

rm(this_count, this_bd_count, this_nbd_count, tokens, corpus, location)

# ######################### PLOT ROCS, GET AUCS #########################

i <- seq(1, 10, 1)
aucs <- list()

for (location in project_location) {
  all_words <- data.frame(ZplaceholderZ = c(1))
  aucs[[location]] <- list()

  png(paste(folder, "/roc_plot_", location, ".png", sep = ""))

  for (k in i) {
    this_words <- readRDS(paste("data/", location, "/words_", k, ".rds", sep = ""))

    for (word in this_words) {
      if (is.null(all_words[[word]])) {
        all_words[[word]] <- 1
      } else {
        all_words[[word]] <- all_words[[word]] + 1
      }
    }

    this_roc <- readRDS(paste("data/", location, "/roc_", k, ".rds", sep = ""))

    aucs[[location]][[k]] <- this_roc$auc * 1

    if (k < 2) {
      plot(this_roc)
    } else {
      lines(this_roc)
    }
  }

  dev.off()

  all_words <- all_words[, !(names(all_words) %in% c("ZplaceholderZ"))]
  all_words <- all_words[order(all_words, decreasing = TRUE)]

  saveRDS(all_words, paste("data/", location, "/all_words.rds", sep = ""))
}

rm(k, this_words, this_roc, all_words, location)

# ######################### GET MEAN AND SD FOR AUCS #########################

if (exists("auc_plot")) { rm(auc_plot) }

for (list_items in aucs) {
  items <- unlist(list_items)

  if (!exists("auc_plot")) {
    auc_plot <- data.frame(mean = mean(items), sd = sd(items))
  } else {
    auc_plot <- rbind(auc_plot, c(mean(items), sd(items)))
  }
}

png(paste(folder, "mean_auc_and_sd_plot.png", sep = "/"))

plot(1:length(aucs), auc_plot$mean, pch = 19, xlab = "", ylab = "", xaxt = "n", main = "mean AUC and SD per dataset",
     xlim = c(0.5, 0.5 + length(aucs)), ylim = c(0, max(auc_plot$mean + auc_plot$sd)))

arrows(1:length(aucs), auc_plot$mean + auc_plot$sd, 1:length(aucs), auc_plot$mean - auc_plot$sd,
       code = 3, angle = 90, length = 0.05)

axis(side = 1, at = 1:length(aucs), labels = c("All", "2011-2013", "2014", "2015", "2016"))

dev.off()

rm(items)

# ######################### GET PARAMETER CHOSEN FOR EACH DATASET #########################

all_words <- data.frame(ZplaceholderZ = rep(1, length(project_location)))
rownames(all_words) <- project_location

for (location in project_location) {
  words <- readRDS(paste("data/", location, "/all_words.rds", sep = ""))

  for (word in names(words)) {
    if (is.null(all_words[[word]])) {
      all_words[[word]] <- rep(0, length(project_location))
    }

    all_words[location, word] <- words[[word]]
  }
}

# Write to CSV
all_words <- all_words[ , !(names(all_words) %in% c("ZplaceholderZ"))]
all_words <- all_words[,apply(all_words, 2, function(x) any(x > fold_cutoff))]
write.csv(all_words, paste(folder, "contrast_sets_combined.csv", sep = "/"), row.names = project_location)

rm(words, word, location)

# ######################### GET WORD COUNTS FOR EACH DATASET #########################

all_counts <- data.frame(matrix(ncol = length(all_words), nrow = length(project_location)))
colnames(all_counts) <- colnames(all_words)
rownames(all_counts) <- project_location
bd_counts <- all_counts
nbd_counts <- all_counts
adjusted_counts <- data.frame(matrix(ncol = length(all_words), nrow = length(project_location)))
colnames(adjusted_counts) <- colnames(all_words)
rownames(adjusted_counts) <- project_location
adjusted_bd_counts <- adjusted_counts
adjusted_nbd_counts <- adjusted_counts

getIncluded <- function(doc) {
  return(doc$meta$included)
}

for (location in project_location) {
  corpus <- readRDS(paste("data/", location, "/clean_corpus_1.rds", sep = ""))
  dtm <- DocumentTermMatrix(corpus)

  bd_docs <- unlist(lapply(corpus, getIncluded))
  nbd_docs <- !bd_docs

  for (word in colnames(all_words)) {
    all_counts[location, word] <- sum(dtm[, intersect(colnames(dtm), word)])
    bd_counts[location, word] <- sum(dtm[c(bd_docs), intersect(colnames(dtm), word)])
    nbd_counts[location, word] <- sum(dtm[c(nbd_docs), intersect(colnames(dtm), word)])
  }
}

adjusted_counts <- all_counts / unlist(token_counts)
adjusted_bd_counts <- bd_counts / unlist(bd_token_counts)
adjusted_nbd_counts <- nbd_counts / unlist(nbd_token_counts)

#bd_col_counts <- colSums(bd_counts)
#nbd_col_counts <- colSums(nbd_counts)
bd_col_counts <- bd_counts["contrast-all",]
nbd_col_counts <- nbd_counts["contrast-all",]
adjusted_bd_col_counts <- adjusted_bd_counts["contrast-all",]
adjusted_nbd_col_counts <- adjusted_nbd_counts["contrast-all",]

rm(bd_docs, nbd_docs, word, dtm, corpus, location)

# ######################### PLOT WORD COUNTS OVER TIME #########################

# Scale counts for plotting
scaleDf <- function(df) {
  scaled_df <- scale(df, center = FALSE, scale = TRUE)

  scaled_df[is.na(scaled_df)] <- 0

  return(scaled_df)
}

relevant_counts <- all_counts[project_location[2:5],]
relevant_adjusted_counts <- adjusted_counts[project_location[2:5],]
relevant_adjusted_bd_counts <- adjusted_bd_counts[project_location[2:5],]
relevant_adjusted_nbd_counts <- adjusted_nbd_counts[project_location[2:5],]

scaled_counts <- scaleDf(relevant_counts)
sac <- scaleDf(relevant_adjusted_counts)
sabdc <- scaleDf(relevant_adjusted_bd_counts)
sanbdc <- scaleDf(relevant_adjusted_nbd_counts)

x <- seq(1, length(scaled_counts[,1]))

png(paste(folder, "wordcount_plot.png", sep = "/"))

min_y <- round(min(scaled_counts), digits = 2)
max_y <- round(max(scaled_counts), digits = 2)

plot(x, rep(0, length(x)), type = "n", ylim = c(min_y, max_y))

for (i in colnames(scaled_counts)) {
  y <- scaled_counts[,i]

  lines(x, y)
}

dev.off()

rm(x, y, min_y, max_y, i)

# ######################### SLOPE FUNCTIONS #########################

getSlope <- function(points) {
  df <- data.frame(y = points, x = 1:length(points))

  fit <- lm(y ~ x, data = df)

  return(fit)
}

getResidualStdErr <- function(fit) {
  return(sqrt(deviance(fit)/df.residual(fit)))
}

getErrorPercentage <- function(fit, points) {
  return(getResidualStdErr(fit) / mean(points))
}

getSignificance <- function(fit) {
  return(summary(fit)$coefficients[,"Pr(>|t|)"]["x"])
}

# ######################### PLOT SLOPES AND CREATE XLSX #########################

fits <- apply(sac, 2, getSlope)
bd_fits <- apply(sabdc, 2, getSlope)
nbd_fits <- apply(sanbdc, 2, getSlope)
# adj_bd_slopes <- apply(adjusted_bd_counts, 2, getSlope)
# adj_nbd_slopes <- apply(adjusted_nbd_counts, 2, getSlope)

bd_sig <- lapply(bd_fits, getSignificance)
nbd_sig <- lapply(nbd_fits, getSignificance)
all_sig <- lapply(fits, getSignificance)

slopes <- as.data.frame(lapply(fits, coef))
bd_slopes <- as.data.frame(lapply(bd_fits, coef))
nbd_slopes <- as.data.frame(lapply(nbd_fits, coef))

png(paste(folder, "adjusted_wordcount_plot.png", sep = "/"))

plot(rep(0, nrow(sac)), rep(0, nrow(sac)), type = "n", ylim = c(-1, 1), xlim = c(-3,3))

for (slope in slopes) {
  abline(0, slope[2])
}

dev.off()

#slopes_df <- rbind(slopes[-c(1),], bd_slopes[2,], nbd_slopes[2,], sqrt((bd_slopes[2,] - nbd_slopes[2,])^2), bd_error, nbd_error, bd_sig, nbd_sig, all_sig)
#rownames(slopes_df) <- c("All", "BD", "NBD", "Diff", "BD_ERR", "NBD_ERR", "BD_SIG", "NBD_SIG", "ALL_SIG")
slopes_df <- rbind(bd_slopes[2,], nbd_slopes[2,], bd_sig, nbd_sig)
slopes_df[is.na(slopes_df)] <- 1
rownames(slopes_df) <- c("BD", "NBD", "BD_SIG", "NBD_SIG")
slopes_df <- t(slopes_df)

sig_slopes <- slopes_df[slopes_df[,"BD_SIG"] < 0.05 | slopes_df[,"NBD_SIG"] < 0.05,]

library("openxlsx")

parseSlopes <- function(slopes, filename) {
  wb <- createWorkbook()
  addWorksheet(wb, "Sheet1")
  writeData(wb, 1, slopes, rowNames = TRUE)

  cs <- createStyle(bgFill = "#FFC7CE")

  conditionalFormatting(wb, "Sheet1", cols = 4:4, rows = 2:nrow(slopes), rule = "< 0.05", style = cs)
  conditionalFormatting(wb, "Sheet1", cols = 5:5, rows = 2:nrow(slopes), rule = "< 0.05", style = cs)

  saveWorkbook(wb, filename, TRUE)
}

parseSlopes(slopes_df, paste(folder, "significant_slopes.xlsx", sep = "/"))
parseSlopes(sig_slopes, paste(folder, "significant_cut_slopes.xlsx", sep = "/"))

sig_bd_slopes <- slopes_df[slopes_df[,"BD_SIG"] < 0.05, c("BD", "BD_SIG")]
sig_bd_points <- t(sabdc[,slopes_df[,"BD_SIG"] < 0.05])
write.xlsx(cbind(sig_bd_slopes, sig_bd_points), paste(folder, "bd_sig_slopes_and_points.xlsx", sep = "/"))

sig_nbd_slopes <- slopes_df[slopes_df[,"NBD_SIG"] < 0.05, c("NBD", "NBD_SIG")]
sig_nbd_points <- t(sanbdc[,slopes_df[,"NBD_SIG"] < 0.05])
write.xlsx(cbind(sig_nbd_slopes, sig_nbd_points), paste(folder, "nbd_sig_slopes_and_points.xlsx", sep = "/"))

png(paste(folder, "bd_nbd_wordcount_plot.png", sep = "/"))

plot(rep(0, nrow(sac)), rep(0, nrow(sac)), type = "n", ylim = c(-1, 1), xlim = c(-3,3))

for (slope in bd_slopes) {
  abline(0, slope[2], col = "blue")
}

for (slope in nbd_slopes) {
  abline(0, slope[2], col = "red")
}

dev.off()

rm(slope, sig_bd_slopes, sig_bd_points, sig_nbd_slopes, sig_nbd_points)

# ######################### CREATE WORDCLOUDS FOR BD AND NBD #########################

library("wordcloud")

png(paste(folder, "wordclouds.png", sep = "/"), width = 2500, height = 1200, res = 200)

par(mfrow = c(1,2))

wordcloud(names(adjusted_bd_col_counts), adjusted_bd_col_counts, scale = c(3,0.5), min.freq = 10, max.words = Inf)
text(0.5, 1, "Big Data", cex = 2, font = 2, col = "red")
wordcloud(names(adjusted_nbd_col_counts), adjusted_nbd_col_counts, scale = c(3,0.5), min.freq = 10, max.words = Inf)
text(0.5, 1, "Non-Big Data", cex = 2, font = 2, col = "red")

dev.off()

# ######################### CREATE DATAFRAME WITH BD AND NBD FREQUENCIES #########################

freqs_df <- data.frame(raw_bd = t(bd_col_counts), raw_nbd = t(nbd_col_counts),
                       bd_counts = t(adjusted_bd_col_counts),
                       nbd_counts = t(adjusted_nbd_col_counts),
                       diff = t(adjusted_bd_col_counts - adjusted_nbd_col_counts),
                       proportional = t(adjusted_bd_col_counts / adjusted_nbd_col_counts))
colnames(freqs_df) <- c("RAW_BD", "RAW_NBD", "ADJUSTED_BD", "ADJUSTED_NBD", "DIFF", "PROPORTIONAL")

freqs_df$PROPORTIONAL[is.infinite(freqs_df$PROPORTIONAL)] <- 0

png(paste(folder, "proportional_wordcloud.png", sep = "/"), width = 1200, height = 1200, res = 200)

wordcloud(rownames(freqs_df), freqs_df$PROPORTIONAL, scale = c(5,0.25), max.words = Inf)

dev.off()

# Order by proportional difference of BD and NBD
freqs_df <- freqs_df[order(freqs_df$PROPORTIONAL, decreasing = TRUE),]

wb <- createWorkbook()
addWorksheet(wb, "Sheet1")
writeData(wb, 1, freqs_df, rowNames = TRUE)

for (row in 2:(nrow(freqs_df) + 1)) {
  conditionalFormatting(wb, "Sheet1", cols = 2:3, rows = row, style = c("white", "#FFC7CE"), type = "colourScale")
  conditionalFormatting(wb, "Sheet1", cols = 4:5, rows = row, style = c("white", "#FFC7CE"), type = "colourScale")
}

saveWorkbook(wb, paste(folder, "BD_vs_NBD_frequencies.xlsx"), TRUE)

rm(wb)

# ######################### GET INTERESTING DOCUMENTS #########################

library("nnet")

# AUCs for all year dataset
this_auc <- unlist(aucs["contrast-all"])
best_model <- which.is.max(this_auc)

# get best fit
fit <- readRDS(paste("data/contrast-all/fit_", best_model, ".rds", sep = ""))

# get documents
corpus <- readRDS("data/contrast-all/clean_corpus_1.rds")
dtm <- as.matrix(removeSparseTerms(DocumentTermMatrix(corpus), sparse = 0.99))

certainty <- predict(fit, newx = dtm, type = "response", s = "lambda.1se")
prediction <- predict(fit, newx = dtm, type = "class", s = "lambda.1se")
actual <- rep(FALSE, length(corpus))

for (i in 1:length(corpus)) {
  if (corpus[[i]]$meta$included == TRUE) {
    actual[[i]] <- TRUE
  }
}

complete <- cbind(certainty, prediction, actual)
colnames(complete) <- c("prediction", "class", "actual")

complete <- complete[order(complete[,"prediction"], decreasing = TRUE),]

write.xlsx(complete, paste(folder, "classifier_predictions.xlsx", sep = "/"))

orig_docs <- read.csv("originals/contrast-all-articles.csv", header = FALSE, stringsAsFactors = FALSE)

num_docs <- length(complete[,1])

include_certain <- t(lapply(row.names(complete[1:10,]), function(x) { return(orig_docs[x,1]) }))
exclude_certain <- t(lapply(row.names(complete[(num_docs - 9):num_docs,]), function(x) { return(orig_docs[x,1]) }))

write.xlsx(include_certain, paste(folder, "include_certain.xlsx", sep = "/"))
write.xlsx(exclude_certain, paste(folder, "exclude_certain.xlsx", sep = "/"))

only_include <- complete[complete[,"actual"] == TRUE,]
only_exclude <- complete[complete[,"actual"] == FALSE,]

num_include <- length(only_include[,1])
num_exclude <- length(only_exclude[,1])

# Get excludes from the top of the exclude list (highly likely that these are actually included)
difficult_exclude <- t(lapply(row.names(only_exclude[1:10,]), function(x) { return(orig_docs[x,1]) }))
# Get includes from the bottom of the include list (highly likely that these are actually excluded)
difficult_include <- t(lapply(row.names(only_include[(num_include - 9):num_include,]), function(x) { return(orig_docs[x,1]) }))

write.xlsx(difficult_exclude, paste(folder, "difficult_exclude.xlsx", sep = "/"))
write.xlsx(difficult_include, paste(folder, "difficult_include.xlsx", sep = "/"))

rm(this_auc, best_model, fit, corpus, dtm, certainty, prediction, actual, i, num_docs, only_include, only_exclude, num_include, num_exclude)