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

# source("cli-input.R")
source("config.R")
source("libraries/utils.R")

# if (length(args) < 2) {
#   stop("Project name not provided")
# }

# project_folder <- getProjectFolder(args[2])
project_folder <- getProjectFolder("contrast/batch1")
version <- getLastVersion("LDA_fit", project_folder)
files <- getAllOfVersion("LDA_fit", project_folder, version)

library(caret)

# model_fit <- train(Class ~ X.1 + X.2 + X.3 + X.4 + X.5 + X.6 + X.7 + X.8 + X.9,
                   # data = formatted_data, method = "glm", family = "binomial", trControl = ctrl, tuneLength = 9)

# model_fit <- train(Class ~ X.1 + X.2 + X.3 + X.4 + X.5 + X.6 + X.7 + X.8 + X.9, data = training, method = "glm", family = "binomial")

for (file in files) {
  data <- readRDS(paste(project_folder, file, sep = "/"))

  formatted_data <- formatInput(data$posterior$theta, seq(1, 1306, 1))

  train_set <- createDataPartition(formatted_data$Class, p = 0.9, list = FALSE)

  training <- formatted_data[train_set,]
  testing <- formatted_data[-train_set,]

  # ctrl <- trainControl(method = "cv", number = 10, savePredictions = TRUE)
  ctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 10)
  formula <- Class ~ X.1 + X.2 + X.3 + X.4 + X.5 + X.6 + X.7 + X.8 + X.10

  # model_fit <- glm(formula, family = "binomial", data = training)

  model_fit <- train(formula, data = training, trControl = ctrl, method = "glm", family = binomial, tuneLength = 10)

  fit.pred <- predict(model_fit, testing, type = "raw")
  table(testing$Class, fit.pred)

  print(confusionMatrix(fit.pred, testing$Class))
  print(varImp(model_fit))

  # fit.real <- ifelse(formatted_data$Class == "include", 1, 0)
  # fit.predt <- function(t) ifelse(fit.pred > t, 1, 0)

  # print(confusionMatrix(fit.predt(0.3), fit.real))

  stop()
}