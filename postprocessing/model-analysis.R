workspace <- "~/workspace/R"

setwd(workspace)

data1 <- readRDS("data/bla/1saliency_terms.rds")
data2 <- readRDS("data/bla/2saliency_terms.rds")

data1 <- data1$Term
data2 <- data2$Term

print(length(unique(data1)))
print(length(unique(data2)))

udat1 <- unique(data1)
udat2 <- unique(data2)

outersect <- function(x, y) {
  sort(c(setdiff(x, y),
         setdiff(y, x)))
}

outer <- outersect(udat1, udat2)

#freqs <- table(c(data2[data2 %in% outer], data1[data1 %in% outer]))

pal <- brewer.pal(7,"Dark2")
png("wordcloud_outer.png", width = 1280,height = 800)
wordcloud(outer, scale = c(2,1), min.freq = 1, max.words = 100, random.order = F, rot.per = 0.1, colors = pal, vfont = c("sans serif","bold"))
dev.off()