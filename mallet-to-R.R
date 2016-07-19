setwd("~/workspace/mallet/data")

normalise <- function(data) {
  norm <- function(row) { row / sum(row) }
  
  return(apply(data, 1, norm))
}

word_weights <- read.table("output/sysrev-topic-word-weights.txt", header=F, sep="\t", quote="")

phi <- as.data.frame(split(word_weights[,3], word_weights[,2]))

# TODO: Flip outcome
norm_phi <- normalise(phi)

#outputtopickeysresult <- read.table("sysrev-topic-keys.txt", header=F, sep="\t")
#outputdoctopicsresult <-read.table("sysrev-doc-topics.txt", header=F, sep="\t")

#dat <- outputdoctopicsresult
#l_dat <- reshape(dat, idvar=1:2, varying=list(topics=colnames(dat[,seq(3, ncol(dat)-1, 2)]), props=colnames(dat[,seq(4, ncol(dat), 2)])), direction="long")
#library(reshape2)
#w_dat <- dcast(l_dat, V2 ~ V3)
#rm(l_dat)
#write.csv(w_dat, "topic_model_table.csv")