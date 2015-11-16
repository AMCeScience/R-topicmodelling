#!/usr/bin/env Rscript

# Follow this doc to install the newest R version
# https://cran.r-project.org/bin/linux/ubuntu/README

# Add R/library folders to user root folder

# Go to /usr/lib/R/etc/Renviron.site
# Add R_LIBS=~/R/library
# Add R_LIBS_USER=~/R/library

# Go to /usr/lib/R/etc/Rprofile.site
# Uncomment default repo setting, set with default CRAN repo: http://cran.us.r-project.org
# https://stackoverflow.com/questions/11488174/how-to-select-a-cran-mirror-in-r

# Install the gsl package for topicmodels to work: sudo apt-get install libgsl0-dev

# Run this script, get a cup of tea this might take a while

install.packages(c("tm", "SnowballC", "quanteda", "stringi", "lda", "topicmodels", "LDAvis", "dplyr", "Rmpfr"))