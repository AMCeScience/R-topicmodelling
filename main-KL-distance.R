source("KL-distance.R")

files <- list.files("data", "TM_LDA*")

unorderedContainer <- list()
container <- list()

for (i in 1:length(files)) {
  file1 <- readRDS(paste("data", files[i], sep = "/"))
  
  orderedRes <- list()
  unorderedRes <- list()
  
  for (j in i:length(files)) {
    file2 <- readRDS(paste("data", files[j], sep = "/"))
    
    res <- KLdistFromRunResults(file1, file2, minimialise = FALSE)
    
    unorderedRes[[j]] <- res
    orderedRes[[j]] <- KLorder(res)
  }
  
  unorderedContainer[[i]] <- unorderedRes
  container[[i]] <- orderedRes
}