# dat <- readRDS('data/aspergillosis/sysrev-nostemminggrams/30_set.rds')
dat <- readRDS('data/lyme/30_set.rds')
# dat <- readRDS('data/complete-folds/30_set.rds')
# dat <- readRDS('data/complete/30_set.rds')
# dat <- readRDS('data/complete-reversed/30_set.rds')

# includes <- c(2076, 2554, 2072, 1922, 2954, 3025, 3336, 1524, 2698, 2895, 1926, 1596, 2624, 2307, 1828, 
#               2205, 2647, 1924, 1539, 2863, 2044, 1314, 1967, 1846, 3341, 2995, 2130, 2907, 2329, 2252,
#               2791, 2952, 2877, 2766, 1057, 2651, 2464, 1132, 2311, 2061, 2053, 2336, 3141, 2768, 2802,
#               3183, 905, 1343, 2250, 1083, 2817, 481, 1056)

includes_lyme <- c(141, 175, 279, 281, 306, 336, 338, 343, 382, 384, 531, 601, 607, 640, 665,
                   680, 718, 729, 743, 757, 832, 860, 887, 923, 958, 1017, 1076, 1142, 1230, 1258, 1337,
                   1380, 1399, 1400, 1472, 1532, 1589, 1603, 1668, 1699, 1709, 1710, 1765, 1768, 1828,
                   1854, 1859, 1872, 1960, 1961, 1977, 1978, 1980, 2012, 2014, 2183, 2219, 2291, 2293,
                   2341, 2389, 2410, 2483, 2509, 2550, 2568, 2595, 2615, 2640, 2663, 2690, 2691, 2738,
                   2765, 2798, 2837, 2852, 2988, 2990, 2996, 3003, 3095, 3166, 3174, 3215, 3321, 3324,
                   3457, 3571, 3583, 3605, 3608, 3614, 3616, 3635, 3647, 3648, 3655, 3657, 3684, 3786,
                   3841, 3856, 3965)

includes <- includes_lyme
# includes <- includes_lyme + 3514

# includes <- c(includes, includes_lyme + 3514)

sizeTrue <- function(x) {
  return(length(x[x == TRUE]))
}

cutoff_steps <- seq(0, 1, 0.01)

x <- cutoff_steps
to_read <- rep(0, length(cutoff_steps))
included <- rep(0, length(cutoff_steps))

for (i in 1:10) {
  rfProbs <- dat[[i]]$rfProbs
  # rfProbs <- dat$rfProbs
  
  rfProbs$rows = row.names(rfProbs)
  
  y <- vector(length = length(rfProbs$include))
  y[] <- 'exclude'
  y[rfProbs$rows %in% includes] <- 'include'
  
  rfProbs$real <- factor(y)
  
  # rfProbs[order(rfProbs$include, decreasing = TRUE),]
  
  this_to_read <- c()
  this_included <- c()
  
  for (step in cutoff_steps) {
    all_included <- rfProbs[rfProbs$include >= step,]
    
    this_to_read <- c(this_to_read, length(all_included$include) / length(rfProbs$include))
    this_included <- c(this_included, sizeTrue(all_included$real == 'include') / sizeTrue(rfProbs$real == 'include'))
  }
  
  to_read <- to_read + this_to_read
  included <- included + this_included
}

plotFrame <- data.frame(x = x, to_read = to_read / 10, included = included / 10)
# plotFrame <- data.frame(x = x, to_read = to_read, included = included)

with(plotFrame, plot(x, to_read, type = 'l', col = 'blue', xlab = '', ylab = ''))
title(main = 'Complete folds: varying Cut-off')
title(ylab = '% of total', line = 2.2, cex.lab = 1)
title(xlab = "Cut-off value", line = 2.1, cex.lab = 1)
lines(x, plotFrame$included, col = 'red')
legend('topright', legend = c('workload', 'includes'), lty = c(1,1), col = c('blue', 'red'), cex = 0.75)
