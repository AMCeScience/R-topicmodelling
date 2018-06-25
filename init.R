#!/usr/bin/env Rscript

# https://cran.r-project.org/bin/linux/ubuntu/README
# http://sysads.co.uk/2014/06/install-r-base-3-1-0-ubuntu-14-04/

# Remove any previous R installations
# sudo apt-get remove r-base-core

# Switch sources
# Go to /etc/apt/sources.list
# Add deb http://cran.rstudio.com/bin/linux/ubuntu trusty/

# Add keys to system
# gpg --keyserver keyserver.ubuntu.com --recv-key E084DAB9
# gpg -a --export E084DAB9 | sudo apt-key add -

# Install R
# sudo apt-get update
# sudo apt-get upgrade
# sudo apt-get install r-base

# Add R/library folders to user root folder

# Go to /usr/lib/R/etc/Renviron.site
# Add R_LIBS=~/R/library
# Add R_LIBS_USER=~/R/library

# Go to /usr/lib/R/etc/Rprofile.site
# Uncomment default repo setting, set with default CRAN repo: http://cran.us.r-project.org
# https://stackoverflow.com/questions/11488174/how-to-select-a-cran-mirror-in-r

# Go to /usr/local/lib/R/
# Make site-library writable: chmod o+w site-library

# Install the following packages for topicmodels to work: apt-get install build-essential gfortran libblas-dev liblapack-dev libgsl0-dev libgmp3-dev libmpfr4 libmpfr-dev libssl-dev libcurl4-openssl-dev

# Run this script, get a cup of tea this might take a while

list.of.packages <- c("tm", "SnowballC", "quanteda", "stringi", "lda", "topicmodels", "LDAvis", "dplyr", "Rmpfr", "stringr", "coda", "Hmisc", "caret", "wordcloud", "doMC")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if (length(new.packages)) install.packages(new.packages, dependencies = TRUE, quiet = FALSE)