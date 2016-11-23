# Symmetrized Kullback Leibler distance
KLdist <- function(phi1, phi2, j1, j2, W) {
  KLsum <- function(phix, phiy, jx, jy, W) {
    sum <- 0
    
    for (k in 1:W) {
      sum <- sum + ( phix[jx,k] * log2( phix[jx,k] / phiy[jy,k] ) )
    }
    
    return(sum)
  }
  
  distance = 0.5 * KLsum(phi1, phi2, j1, j2, W) + 0.5 * KLsum(phi2, phi1, j2, j1, W)
  
  return(distance)
}

KLdistFromRunResults <- function(run1, run2, minimialise = FALSE) {
  # Distinct word types
  W = length(run1$usedTerms)
  # Posteriors
  phi1 = run1$posterior$phi
  phi2 = run2$posterior$phi
  # Number of topics
  Topics1 = run1$numberOfTopics
  Topics2 = run2$numberOfTopics
  
  KLdistList = data.frame()
  
  # Loop over topics, calculate KL distance
  for (t1 in 1:Topics1) {
    for (t2 in 1:Topics2) {
      KLdistList[t1, t2] = KLdist(phi1, phi2, t1, t2, W)
    }
  }
  
  if (minimialise) {
    for (t1 in 1:Topics1) {
      KLdistList[t1,] = KLdistList[t1,] - min(KLdistList[t1,])
    }
  }
  
  return(KLdistList)
}

KLdistFromIds <- function(id1, id2) {
  source("postprocessing/postprocessing.R")
  
  run1 <- readFileId(id1)
  run2 <- readFileId(id2)
  
  return(KLdistFromRunResults(run1, run2))
}

KLorder <- function(set) {
  # Start with the full set as subset
  subset <- set
  
  setLength = length(set[1,])
  
  for (i in 1:(setLength - 1)) {
    # Order the current subset
    subset <- subset[order(subset[i,])]
    
    # Replace original data in set with ordered subset
    set[,i:setLength] <- subset
    
    # Create subset of unordered items
    subset <- subset[,2:length(subset[1,])]
  }
  
  return(set)
}




