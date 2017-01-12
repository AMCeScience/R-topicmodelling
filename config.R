configLoaded <- TRUE

# GENERIC
corpus_folder <- "originals"
data_folder <- "data"
parallel_cores <- 2
parallel_silent <- TRUE

# CLEAN
clean_force <- TRUE
clean_store <- TRUE
clean_gram <- FALSE
clean_stem <- FALSE
clean_overwrite <- FALSE
clean_grams <- 1:2
clean_extra_stopwords <- c("big", "data", "ieee", "discussion", "conclusion", "introduction", "methods", "psycinfo_database", 
                           "rights_reserved", "record_apa", "journal_abstract", "apa_rights", "psycinfo", "reserved_journal", 
                           "conclusionadvancement", "apa", "reserved", "rights_journal", 
                           "test", "case", "proven_probable", "perform", "result", "detect", "present", "reveal", "year",
                           "show", "report", "disseminate", "technique", "major", "increase", "include", "challenge", "total",
                           "special", "count", "significantly_higher")

# FIT
fit_k <- 25 # Number of topics
fit_iter <- 500 # Iterations
fit_burnin <- 200
fit_thin <- 200
fit_nstart <- 3
fit_keep <- 10
fit_divider <- 50
fit_alpha <- fit_k / fit_divider
fit_beta <- 0.01
fit_seed <- list(123234, 890, 112, 239234, 1947)
fit_store <- TRUE

# RANDOM FOREST
rf_store <- TRUE
rf_folds <- 1:10