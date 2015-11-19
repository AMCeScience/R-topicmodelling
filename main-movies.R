#!/usr/bin/env Rscript

# Clear workspace
rm(list = ls())

workspace <- "~/workspace/R/movies"

# Set workspace to folder where articles.csv is placed
setwd(workspace)

data(reviews, package = "LDAvisData")

# read in some stopwords:
stop_words <- stopwords("SMART")

# pre-processing:
dtm.control <- list(
  tolower   		      = T,
  removePunctuation 	= T,
  removeNumbers 		  = T,
  stopwords 			    = stopwords("english"),
  stemming 			      = T,
  wordLengths 		    = c(3, Inf),
  weighting 		    	= weightTf
)

corp <- Corpus(VectorSource(reviews))
dtm <- DocumentTermMatrix(corp, control = dtm.control)
# exclude terms that occur less than 5 times
idx <- colSums(as.matrix(dtm)) > 5
dtm <- dtm[, idx]
# throw out any empty documents
idx <- rowSums(as.matrix(dtm)) > 0
dtm <- dtm[idx, ]

save(dtm, file = "data/dtm.rda")

########################################################################################