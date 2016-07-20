configLoaded <- TRUE

k <- 75 # Number of topics
iter <- 500 # Iterations
burnin <- 200
thin <- 200
nstart <- 3
keep <- 10
alpha <- 0.38
beta <- 0.01
seed <- list(123234, 890, 112, 239234, 1947)
optimal_alphas <- c(1, 0.9, 0.9, 0.85, 0.77, 0.73, 0.71, 0.69, 0.65, 0.6, 0.54, 0.51, 0.45, 0.41, 0.38, 0.31, 0.25, 0.21, 0.17, 0.13)

folder = "data"
rangeAlpha = "true"

# Preprocessing Variables
numberOfGrams <- 1:2
CSVfileName <- "articles.csv"
extraStopWords <- c("big", "data", "ieee", "discussion", "conclusion", "introduction", "methods", "psycinfo_database", 
                    "rights_reserved", "record_apa", "journal_abstract", "apa_rights", "psycinfo", "reserved_journal", 
                    "conclusionadvancement", "apa", "reserved", "rights_journal", 
                    "test", "case", "proven_probable", "perform", "result", "detect", "present", "reveal", "year",
                    "show", "report", "disseminate", "technique", "major", "increase", "include", "challenge", "total",
                    "special", "count", "significantly_higher")