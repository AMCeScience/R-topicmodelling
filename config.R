configLoaded <- TRUE

k <- 10 # Number of topics
iter <- 500 # Iterations
burnin <- 200
thin <- 200
nstart <- 3
keep <- 10
alpha <- k/50
beta <- 0.01
seed <- list(123234, 890, 112, 239234, 1947)

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