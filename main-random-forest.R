args <- commandArgs(trailingOnly = TRUE)

includes <- c(2076, 2554, 2072, 1922, 2954, 3025, 3336, 1524, 2698, 2895, 1926, 1596, 2624, 2307, 1828, 
              2205, 2647, 1924, 1539, 2863, 2044, 1314, 1967, 1846, 3341, 2995, 2130, 2907, 2329, 2252,
              2791, 2952, 2877, 2766, 1057, 2651, 2464, 1132, 2311, 2061, 2053, 2336, 3141, 2768, 2802,
              3183, 905, 1343, 2250, 1083, 2817, 481, 1056)

includes_lyme <- c(141, 175, 279, 281, 306, 336, 338, 343, 382, 384, 531, 601, 607, 640, 665,
                   680, 718, 729, 743, 757, 832, 860, 887, 923, 958, 1017, 1076, 1142, 1230, 1258, 1337,
                   1380, 1399, 1400, 1472, 1532, 1589, 1603, 1668, 1699, 1709, 1710, 1765, 1768, 1828,
                   1854, 1859, 1872, 1960, 1961, 1977, 1978, 1980, 2012, 2014, 2183, 2219, 2291, 2293,
                   2341, 2389, 2410, 2483, 2509, 2550, 2568, 2595, 2615, 2640, 2663, 2690, 2691, 2738,
                   2765, 2798, 2837, 2852, 2988, 2990, 2996, 3003, 3095, 3166, 3174, 3215, 3321, 3324,
                   3457, 3571, 3583, 3605, 3608, 3614, 3616, 3635, 3647, 3648, 3655, 3657, 3684, 3786,
                   3841, 3856, 3965)

includes_lyme <- includes_lyme + 3514

includes <- c(includes, includes_lyme)

source("random-forest-builder.R")

if (length(args) > 0) {
  print("Taking cli arguments.")
  
  workspace = args[1]
  folder = args[2]
  cores <- 7
  #datasets <- c(5, 10, 15, 20, 25, 30, 35, 40, 45, 50, 55, 60, 65, 70, 75, 80, 85, 90, 95, 100, 150, 200, 250, 300, 350, 400, 450, 500)
  datasets <- c(20, 25, 30, 40, 50, 75, 100)
  folds <- 1:10
  store <- TRUE
  
  print(paste("Changing to workspace:", workspace))
  
  setwd(workspace)
  
  mclapply(datasets, function(set) runFullSet(set, folder, store), mc.cores = cores, mc.silent = TRUE)
} else {
  print("Taking preset arguments.")
  
  workspace <- "~/workspace/R"
  folder = 'data/complete'
  cores <- 2
  datasets <- c(10)
  folds <- 1:1
  store <- FALSE
  
  setwd(workspace)
  
  data <- runFullSet(25, folder, store)
}