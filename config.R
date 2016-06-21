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
rangeAlpha = "true"

# Preprocessing Variables
numberOfGrams <- 1:2
CSVfileName <- "articles.csv"
extraStopWords <- c("ieee", "discussion", "conclusion", "introduction", "methods", "psycinfo_database", 
                    "rights_reserved", "record_apa", "journal_abstract", "apa_rights", "psycinfo", "reserved_journal", 
                    "conclusionadvancement", "apa", "reserved", "rights_journal", 
                    "test", "case", "proven_probable", "perform", "result", "detect", "present", "reveal", "year",
                    "show", "report", "disseminate", "technique", "major", "increase", "include", "challenge", "total",
                    "special", "count", "significantly_higher")