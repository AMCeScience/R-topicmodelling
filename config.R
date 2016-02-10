configLoaded <- TRUE

ks <- c(2:10, 25, 50) #Number of topics
iter <- 1500 # Iterations
burnin <- 1400
keep <- 50
alpha <- 0.25
beta <- 0.1

# Preprocessing Variables
numberOfGrams <- 1:2
CSVfileName <- "articles.csv"
extraStopWords <- c("big_data")