library("nnet")
library("glmnet")
library("tm")

csv_loc <- "articles-all.csv"
location <- "contrast-all"
folder <- "data/lm-analysis"
csv_folder <- "originals/contrast-new"

corpus <- readRDS(paste("data/", location, "/clean_corpus_1.rds", sep = ""))
dtm <- as.matrix(removeSparseTerms(DocumentTermMatrix(corpus), sparse = 0.99))
csv <- read.csv(paste(csv_folder, csv_loc, sep = "/"), header = FALSE, stringsAsFactors = FALSE)

aucs <- list()

for (k in 1:10) {
  this_roc <- readRDS(paste("data/", location, "/roc_", k, ".rds", sep = ""))

  aucs[[k]] <- this_roc$auc * 1
}

# AUCs for all year dataset
this_auc <- unlist(aucs)
best_model <- which.is.max(this_auc)

# get best fit
fit <- readRDS(paste("data/contrast-all/fit_", best_model, ".rds", sep = ""))

certainty <- predict(fit, newx = dtm, type = "response", s = "lambda.1se")
actual <- rep(FALSE, length(corpus))
years <- rep(NA, length(corpus))

for (i in 1:length(corpus)) {
  if (corpus[[i]]$meta$included == TRUE) {
    actual[[i]] <- TRUE
  }

  years[[i]] <- csv[i,2]
}

cutoff <- 0.11

prediction <- certainty > cutoff

complete <- cbind(certainty, prediction, actual, years)
colnames(complete) <- c("prediction", "class", "actual", "year")

complete[which(complete[,"year"] == "2011"), "year"] <- "2012"
complete[which(complete[,"year"] == "2012"), "year"] <- "2013"
# complete[which(complete[,"year"] == "2013"), "year"] <- "2014"
complete[which(complete[,"year"] == "2017"), "year"] <- "2016"
complete[,"actual"] <- as.logical(complete[,"actual"] == "1")
complete[,"class"] <- as.logical(complete[,"class"] == "1")

unique_years <- unique(complete[,"year"])
unique_years <- unique_years[!is.na(unique_years)]
unique_years <- unique_years[order(unique_years, decreasing = FALSE)]
# unique_years <- c("2013", "2014", "2015", "2016")

complete_tab <- table(complete)

year_counts <- unlist(lapply(unique_years, function(x) { complete_tab[names(complete_tab) == x] }))

fp_tab <- table(complete[complete[,"class"] == TRUE & complete[,"actual"] == FALSE,])
fn_tab <- table(complete[complete[,"class"] == FALSE & complete[,"actual"] == TRUE,])

fp_counts <- unlist(lapply(unique_years, function(x) { fp_tab[names(fp_tab) == x] }))
fn_counts <- unlist(lapply(unique_years, function(x) { fn_tab[names(fn_tab) == x] }))

tp_tab <- table(complete[complete[,"class"] == TRUE & complete[,"actual"] == TRUE,])
tn_tab <- table(complete[complete[,"class"] == FALSE & complete[,"actual"] == FALSE,])

tp_counts <- unlist(lapply(unique_years, function(x) { tp_tab[names(tp_tab) == x] }))
tn_counts <- unlist(lapply(unique_years, function(x) { tn_tab[names(tn_tab) == x] }))

p_tab <- table(complete[complete[,"actual"] == TRUE,])
n_tab <- table(complete[complete[,"actual"] == FALSE,])

p_counts <- unlist(lapply(unique_years, function(x) { p_tab[names(p_tab) == x] }))
n_counts <- unlist(lapply(unique_years, function(x) { n_tab[names(n_tab) == x] }))

# n_all <- sum(n_counts)
# all_count <- sum(p_counts) + sum(n_counts)

ppv <- tp_counts / (tp_counts + fp_counts)
npv <- tn_counts / (tn_counts + fn_counts)

# fpr <- fp_counts / (fp_counts + tn_counts)
# fnr <- fn_counts / (fn_counts + tp_counts)
#
# spec <- tn_counts / (fp_counts + tn_counts)
# sens <- tp_counts / (tp_counts + fn_counts)
#
# lrp <- sens / (1 - spec)
# lrn <- (1 - sens) / spec

# pos_tab <- table(complete[complete[,"class"] == TRUE,])
# neg_tab <- table(complete[complete[,"class"] == FALSE,])
#
# pos_counts <- unlist(lapply(unique_years, function(x) { pos_tab[names(pos_tab) == x] }))
# neg_counts <- unlist(lapply(unique_years, function(x) { neg_tab[names(neg_tab) == x] }))
#
fp_points <- 1 - ppv
# fn_points <- fn_counts / year_counts

fp_train <- data.frame(year = 0:(length(fp_points) - 1), point = fp_points)
# fn_train <- data.frame(year = 0:(length(fn_points) - 1), point = fn_points)

png(paste(folder, "FDR_trend.png", sep = "/"), width = 480, height = 300)

year_labs <- unique_years
year_labs[1] <- "2011 - 2013"

plot(0:(length(fp_points) - 1), fp_points, xaxt = "n", xlab = "", ylab = "", ylim = c(0, 1.0))
title(xlab = "Year", cex.lab = 1.5, line = 2.3)
title(ylab = "False discovery rate", cex.lab = 1.5, line = 2.3)
axis(1, at = 0:(length(fp_points) - 1), labels = year_labs)
# abline(glm(point ~ year, data = fp_train), lty = 2, col = "red")

dev.off()

# png(paste(folder, "FN_trend.png", sep = "/"))
#
# plot(0:(length(fn_points) - 1), fn_points, xaxt = "n", xlab = "", ylab = "")
# title(xlab = "Year", cex.lab = 1.5, line = 2)
# title(ylab = "Relative # of FN", cex.lab = 1.5, line = 2.3)
# axis(1, at = 0:(length(fp_points) - 1), labels = unique_years)
# abline(glm(point ~ year, data = fn_train), lty = 2, col = "red")
#
# dev.off()

complete_ord <- as.data.frame(complete[order(complete[,'prediction'], decreasing = TRUE),], stringsAsFactors = FALSE)
complete_ord[,'prediction'] <- as.numeric(complete_ord[,'prediction'])

num_splits <- 200

splits <- cut(complete_ord[,'prediction'], num_splits, labels = 1:num_splits)

getBarForYear <- function(year) {
  these_docs <- length(which(complete_ord[,'year'] == year))

  these_splits <- splits[which(complete_ord[,'year'] == year)]

  split_counts <- table(these_splits) / these_docs

  plot(1:length(split_counts), split_counts, type = "l", main = year)
}

getCumSumForYear <- function(year, bd = TRUE) {
  year_positions <- which(complete_ord[,'year'] == year & complete_ord[,'actual'] == bd)

  hold <- rep(0, length(complete_ord[,1]))

  increment <- 1 / length(year_positions)

  hold[year_positions] <- increment
  points <- cumsum(hold)

  return(points)
}

count <- 1

for (type in c(TRUE, FALSE)) {
  col_count <- 1

  for (year in unique_years) {
    points <- getCumSumForYear(year, type)

    cols <- c("red", "blue", "black", "green", "purple")

    if (count < 2) {
      plot(1:length(complete_ord[,1]), points, type = "l", col = cols[col_count])
    } else {
      lines(points, col = cols[col_count])
    }

    count <- count + 1
    col_count <- col_count + 1
  }
}

# count <- 1
#
# for (year in c('2012', '2013', '2014', '2015', '2016')) {
#   points <- getBarForYear(year)
#
#   if (count < 1) {
#     plot(1:length(points), points, type = "l")
#   } else {
#     lines(points)
#   }
#
#   count <- count + 1
# }

sens <- function(pos, neg) {
  tp <- length(which(pos[,'actual'] == TRUE))
  fn <- length(which(neg[,'actual'] == TRUE))

  return(tp / (tp + fn))
}

spec <- function(pos, neg) {
  fp <- length(which(pos[,'actual'] == FALSE))
  tn <- length(which(neg[,'actual'] == FALSE))

  return(tn / (tn + fp))
}

points <- 100
sens_list <- list()
spec_list <- list()

for (point in 1:points) {
  cutoff <- point / points

  pos_preds <- complete_ord[complete_ord[,'prediction'] > cutoff,]
  neg_preds <- complete_ord[complete_ord[,'prediction'] <= cutoff,]

  sens_list[point] <- sens(pos_preds, neg_preds)
  spec_list[point] <- spec(pos_preds, neg_preds)
}

sens_list <- unlist(sens_list)
spec_list <- unlist(spec_list)

plot(seq(0.01, 1, 1/points), sens_list, type = "l", col = "blue")
lines(seq(0.01, 1, 1/points), spec_list, col = "red")
abline(v = 0.11, lty = 2)
