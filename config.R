configLoaded <- TRUE

k <- 4 # Number of topics
iter <- 4000 # Iterations
burnin <- 2000
thin <- 500
nstart <- 5
keep <- FALSE
alpha <- 0.25
beta <- 0.1

# Preprocessing Variables
numberOfGrams <- 1:2
CSVfileName <- "articles.csv"
extraStopWords <- c("big_data")