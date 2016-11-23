# Investigate single documents in DTM
#m <- as.matrix(data$dtm[50,])
#colnames(m)[m > 0]

create_set <- function() {
  includes <- c(2076, 2554, 2072, 1922, 2954, 3025, 3336, 1524, 2698, 2895, 1926, 1596, 2624, 2307, 1828, 
                2205, 2647, 1924, 1539, 2863, 2044, 1314, 1967, 1846, 3341, 2995, 2130, 2907, 2329, 2252,
                2791, 2952, 2877, 2766, 1057, 2651, 2464, 1132, 2311, 2061, 2053, 2336, 3141, 2768, 2802,
                3183, 905, 1343, 2250, 1083, 2817, 481, 1056)
  
  thetas <- data$posterior$theta
  
  y <- vector(length = length(thetas[,1]))
  y[] <- 'exclude'
  y[includes] <- 'include'
  
  y <- factor(y)
  
#   selection <- order(-importance(rf1))[0:10]
#   
#   input <- data.frame(X = thetas)[selection]
  input <- data.frame(X = thetas)
  input$y <- y
  
  return(input)
}

# myFormula <- y ~ Topic1 + Topic2 + Topic3 + Topic4 + Topic5 + Topic6 + Topic7 + Topic8 + Topic9 + Topic10 + Topic11 + Topic12 + Topic13 + Topic14
# 
# input.glm <- glm(myFormula, family = "binomial", data = input)
# 
# summary(input.glm)
# 
# pred <- predict(input.glm, type = "response")
# plot(input$y, pred, xlab = "Observed", ylab = "Prediction")
# 
# abline(a = 0, b = 1)
# 
# split into two subsets: training (70%) and test (30%)
ind <- sample(2, nrow(input), replace=TRUE, prob=c(0.7, 0.3))
train.data <- input[ind==1,]
test.data <- input[ind==2,]

# use all other variables to predict Species
library(randomForest)

# rf1 = randomForest(Class~.,input,ntree=500, sampsize=5000)
# rf2 = randomForest(Class~.,input,ntree=4000,sampsize=c(50,500,500),strata=input$Class)
# rf3 = randomForest(Class~.,input,ntree=4000,sampsize=c(50,100,100),strata=input$Class)
# rf4 = randomForest(Class~.,input,ntree=4000,sampsize=c(50,50,50)  ,strata=input$Class)







# plot(importance(rf)[order(importance(rf))])

rf1 = randomForest(y~., train.data, ntree=500, sampsize=2000)
rf2 = randomForest(y~., train.data, ntree=4000, sampsize=c(500,50), strata=input$y)
rf3 = randomForest(y~., train.data, ntree=4000, sampsize=c(100,50), strata=input$y)
rf4 = randomForest(y~., train.data, ntree=4000, sampsize=c(50,50), strata=input$y)
rf4 = randomForest(y~., train.data, ntree=4000, sampsize=c(30,30), strata=input$y)

table(predict(rf1), train.data$y)

print(rf)

plot(rf, main = "")

varImpPlot(rf1)

pred <- predict(rf1, newdata = test.data)
table(pred, test.data$y)
plot(margin(rf1, test.data$y))

par(mfrow=c(1,1))
plot(roc(rf4$votes[,1],factor(1 * (rf1$y=='include'))),main="ROC curves for four models predicting class 0")
plot(roc(rf2$votes[,1],factor(1 * (rf1$y=='include'))),col=2,add=T)
plot(roc(rf3$votes[,1],factor(1 * (rf1$y=='include'))),col=3,add=T)
plot(roc(rf4$votes[,1],factor(1 * (rf1$y=='include'))),col=4,add=T)
