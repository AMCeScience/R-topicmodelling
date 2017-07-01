dat <- readRDS("data/matrix/RF_CF_1.rds")
dat_lda <- readRDS("data/contrast/batch1/RF_CF_fit_k75_1.rds")

plot(dat[[1]]$ROC)

for (i in 1:length(dat)) {
  lines(dat[[i]]$ROC)
  #lines(dat_lda[[i]]$ROC, col = "blue")
}