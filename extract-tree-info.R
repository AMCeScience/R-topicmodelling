library("randomForest")

setwd("~/workspace/R")

rf_data <- readRDS("data/clef-test/RF_CF_fit_k75_1.rds")
tm_data <- readRDS("data/clef-test/LDA_fit_k75_a0.67_b0.01_1.rds")


tree <- rf_data[[1]]$rf

varImp <- importance(tree$finalModel)
topics <- varImp[varImp > 10,]

topic_lists <- terms(tm_data$LDAData, 30)

interesting_topics <- topic_lists[, varImp > 10]