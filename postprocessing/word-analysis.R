workspace <- "~/workspace/R"

setwd(workspace)

data <- read.csv2("data/impressions2.csv", header = FALSE)

stacked <- matrix(t(data), ncol = 2, nrow = (ncol(data) / 2) * nrow(data), byrow = TRUE)

vectorised <- split(stacked[,2], stacked[,1])

words <- matrix(data = NA, nrow = length(vectorised), ncol = 6)

for (i in 1:length(vectorised)) {
  item <- vectorised[i]
  
  name <- names(item)
  val <- item[[1]]
  
  val[val > 5] = 5
  val[val < 1] = NA
  
  words[i, 1] = name
  words[i, 2] = length(unique(val))
  words[i, 3] = if (length(unique(val)) == 1) 1 else 0
  words[i, 4] = if (length(unique(val)) != 1) 1 else 0
  words[i, 5] = length(val)
  words[i, 6] = toString(val)
}

# Used for visual inspection of results
notunique = words[words[,4] == 1,]

library(data.table)

td <- data.table(words = words[,1], unique = as.numeric(words[,3]), notunique = as.numeric(words[,4]), appearance = as.numeric(words[,5]))
grouped <- td[,list(unique = sum(unique), notunique = sum(notunique)), by = appearance]
ordered <- grouped[order(rank(appearance))]

subset <- ordered[2:12]

plotmatrix <- matrix(data = NA, nrow = 2, ncol = length(subset$appearance))
dimnames(plotmatrix) <- list(rownames(plotmatrix), subset$appearance)
plotmatrix[1,] <- subset$unique
plotmatrix[2,] <- subset$notunique

barplot(plotmatrix, border = "transparent", col = c("gray35", "gray70"), ylim = c(0, 50), legend.text = c("Unique", "Not Unique"))
title(xlab = "Word Frequency", line = 2.2, cex.lab = 1)
title(ylab = "Number of Words", line = 2.3, cex.lab = 1)
