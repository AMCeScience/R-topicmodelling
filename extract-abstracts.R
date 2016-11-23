setwd('~/workspace/R')

library(openxlsx)

data <- read.xlsx('data/lyme-study.xlsx', cols = c(3,6))

#complete <- data[!is.na(data$ABSTRACT),]

merged <- paste(data$TITLE, data$ABSTRACT, sep = " ")

merged <- sub(' NA', '', merged)

# Remove some punctuation
#rmvd <- gsub("/. /", " ", merged, perl = TRUE)
#rmvd <- gsub("/.$|,|;/", " ", rmvd, perl = TRUE)
rmvd <- gsub('[^[:alpha:] ]', '', merged)

write.table(rmvd, file='lyme.csv', row.names = FALSE, col.names = FALSE)

file1 <- as.matrix(read.csv(file = 'articles_sysrev.csv', header = FALSE)[1])
file2 <- as.matrix(read.csv(file = 'lyme.csv', header = FALSE))

data <- rbind(file1, file2)

write.table(data, file = 'complete.csv', row.names = FALSE, col.names = FALSE)