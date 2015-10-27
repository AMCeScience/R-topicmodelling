#!/usr/bin/env Rscript

# Add R/library folders to user root folder

# Go to /usr/lib/R/etc/Renviron.site
# Add R_LIBS=~/R/library
# Add R_LIBS_USER=~/R/library

# Go to /usr/lib/R/etc/Rprofile.site
# Uncomment default repo setting, set with default CRAN repo: http://cran.us.r-project.org
# https://stackoverflow.com/questions/11488174/how-to-select-a-cran-mirror-in-r

# Install the gsl package for topicmodels to work: sudo apt-get install libgsl0-dev

install.packages(c("tm", "SnowballC", "quanteda", "stringi", "lda", "topicmodels", "LDAvis", "dplyr"))