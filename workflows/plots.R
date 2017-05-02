for (k in c(10, 25, 50, 75)) {
  i1 <- readRDS(paste("data/10-2-17/batch1/average_importance_k", k, ".rds", sep = ""))
  i2 <- readRDS(paste("data/10-2-17/batch2/average_importance_k", k, ".rds", sep = ""))
  i3 <- readRDS(paste("data/10-2-17/batch3/average_importance_k", k, ".rds", sep = ""))
  i4 <- readRDS(paste("data/10-2-17/batch4/average_importance_k", k, ".rds", sep = ""))

  png(paste("average_importance_k", k, ".png", sep = ""), width = 400, height = 300)

  par(cex = 1.3)

  plot(1:k, i1, type = "l", col = "#333333", xlab = "", ylab = "")
  lines(1:k, i2, col = "#27B9B9")
  lines(1:k, i3, col = "#C02942")
  lines(1:k, i4, col = "#97CD39")
  title(main = paste("T =", k, sep = " "))
  title(ylab = "Variable Importance", line = 2.2, cex.lab = 1)
  title(xlab = "(Sorted) Topic Importance Rank", line = 2.1, cex.lab = 1)
  dev.off()
}

f1 <- readRDS("data/10-2-17/batch1/F1_list.rds")
f2 <- readRDS("data/10-2-17/batch2/F1_list.rds")
f3 <- readRDS("data/10-2-17/batch3/F1_list.rds")
f4 <- readRDS("data/10-2-17/batch4/F1_list.rds")

library("Hmisc")

png("F1_plot.png", width = 400, height = 400)

defpar <- par()
par(cex = 1.5)

plot(f1$x, f1$F1, ylim = c(0.5, 1.0), col = "#030C22", xaxt = "n", ylab = "", xlab = "", pch = 0)
axis(1, at = f1$x, labels = f1$x)
points(f2$x, f2$F1, col = "#20293F", pch = 18)
points(f3$x, f3$F1, col = "#404749", pch = '*')
points(f4$x, f4$F1, col = "#A9B0B3", pch = 11)

title(ylab = expression("F"[1]*"-measure"), line = 2.2, cex.lab = 1)
title(xlab = "Number of Topics", line = 2.1, cex.lab = 1)
dev.off()

# Manual stuff here
w1 <- readRDS("data/10-2-17/batch1/words.rds")
w2 <- readRDS("data/10-2-17/batch2/words.rds")
w3 <- readRDS("data/10-2-17/batch3/words.rds")
w4 <- readRDS("data/10-2-17/batch4/words.rds")

words <- c(w1, w2, w3, w4)

library("tm")

corpus <- Corpus(VectorSource(words))
tdm <- TermDocumentMatrix(corpus)

m <- as.matrix(tdm)
v <- sort(rowSums(m), decreasing = TRUE)
d <- data.frame(word = names(v), freq = v)

library("wordcloud")

png("wordcloud.png", width = 1280, height = 800)

pal <- brewer.pal(9, "YlGnBu")
pal <- pal[-(1:4)]

wordcloud(d$word, d$freq, scale = c(5,.1), min.freq = 3, max.words = 100, random.order = FALSE, rot.per = .15, colors = pal)

dev.off()