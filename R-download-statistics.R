workspace <- "~/workspace/R"

setwd(workspace)

start <- as.Date('2015-01-01')
today <- as.Date('2015-12-31')

all_days <- seq(start, today, by = 'day')

year <- as.POSIXlt(all_days)$year + 1900
urls <- paste0('http://cran-logs.rstudio.com/', year, '/', all_days, '.csv.gz')

# for (i in 1:length(urls)) {
#   download.file(urls[i], sprintf("data/Rstatistics/temp%i.csv.gz", i))
# }

pb <- txtProgressBar(min=0, max=length(urls), style=3)

count1 = 0
count2 = 0

for (i in 1:length(urls)) {
  df.csv <- read.csv(sprintf("data/Rstatistics/temp%i.csv", i))
  pack <- tolower(as.character(df.csv$package))
  first.package <- which(pack == "topicmodels")
  second.package <- which(pack == "tm")
  
  count1 = count1 + length(first.package)
  count2 = count2 + length(second.package)
  
  setTxtProgressBar(pb, i)
}
close(pb)
print(count1)
print(count2)