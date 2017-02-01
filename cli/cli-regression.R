#!/usr/bin/env Rscript
# https://stackoverflow.com/questions/18306362/run-r-script-from-command-line

# CALL
# ./cli-regression.R ~/workspace/R test_corp

formatInput <- function(thetas, includes) {
  # Create include/exclude factor
  y <- vector(length = length(thetas[,1]))
  y[] <- "exclude"
  y[includes] <- "include"

  y <- factor(y)

  # Create data frame out of thetas
  input <- data.frame(X = thetas)
  # Append the factor
  input$Class <- y

  return(input)
}

source("cli-input.R")
source("libraries/utils.R")

if (length(args) < 2) {
  stop("Project name not provided")
}

project_folder <- getProjectFolder(args[2])
version <- getLastVersion("LDA_fit", project_folder)
files <- getAllOfVersion("LDA_fit", project_folder, version)

library(caret)

train_set <- createDataPartition(formatted_data$Class, p = 0.9, list = FALSE)

training <- formatted_data[train_set,]
testing <- formatted_data[-train_set,]

ctrl <- trainControl(method = "repeatedcv", number = 10, savePredictions = TRUE)

model_fit <- train(Class ~ X.1 + X.2 + X.3 + X.4 + X.5 + X.6 + X.7 + X.8 + X.9,
                   data = formatted_data, method = "glm", family = "binomial", trControl = ctrl, tuneLength = 9)

model_fit <- train(Class ~ X.1 + X.2 + X.3 + X.4 + X.5 + X.6 + X.7 + X.8 + X.9, data = training, method = "glm", family = "binomial")

for (file in files) {
  data <- readRDS(paste(project_folder, file, sep = "/"))

  formatted_data <- formatInput(data$posterior$theta, seq(1, 1306, 1))

  fit <- glm(Class ~ X.1 + X.2 + X.3 + X.4 + X.5 + X.6 + X.7 + X.8 + X.9, family = "binomial", data = training)

  fit.pred <- predict.glm(fit, type = "response")

  fit.real <- ifelse(formatted_data$Class == 1, 1, 0)
  fit.predt <- function(t) ifelse(fit.pred > t, 1, 0)

  confusionMatrix(fit.predt(0.3), fit.real)

  stop()
}