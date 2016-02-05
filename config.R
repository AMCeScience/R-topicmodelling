configLoaded <- TRUE

ks <- c(2:10, 25, 50) #Number of topics
iter <- 200 # Iterations
burnin <- 180
keep <- 2
alpha <- 1
beta <- 0.1

# Preprocessing Variables
numberOfGrams <- 1:2
CSVfileName <- "articles.csv"
extraStopWords <- c("big_data")