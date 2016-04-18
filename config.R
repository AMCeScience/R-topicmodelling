configLoaded <- TRUE

k <- 4 # Number of topics
iter <- 4000 # Iterations
burnin <- 2000
thin <- 500
nstart <- 5
keep <- FALSE
alpha <- 0.25
beta <- 0.1
seed <- list(123234, 890, 112, 239234, 1947)

folder = "data"

# Preprocessing Variables
numberOfGrams <- 1:2
CSVfileName <- "articles.csv"
extraStopWords <- c("big_data", "ieee", "discussion", "conclusion", "introduction", "methods", "psycinfo_database", "rights_reserved", "record_apa", "journal_abstract", "apa_rights", "psycinfo", "reserved_journal", "conclusionadvancement", "apa", "reserved")