split_dtm_once <- function(dtm, perc = 0.1) {
  n <- dtm$nrow
  idx <- sample(nrow(dtm), size = floor(n * perc))
  
  dtm_train <- dtm[-idx,]
  dtm_test <- dtm[idx,]
  
  return(list(train = dtm_train, test = dtm_test))
}

split_dtm <- function(dtm, perc = 0.1) {
  data_left <- dtm
  group <- list()
  
  n <- data_left$nrow
  
  for (j in 1:10) {
    if (j == 10) {
      group[j] <- list(data_left)
    } else {
      idx <- sample(nrow(data_left), size = floor(n * perc))
      
      group[j] <- list(data_left[idx, ])
      data_left <- data_left[-idx, ]
    }
  }
  
  return(group)
}

split_corpus <- function(corpus, perc = 0.1) {
  data_left <- corpus
  group <- list()
  
  n <- length(data_left)
  
  for (j in 1:10) {
    n_now <- length(data_left)
    
    if (j == 10) {
      group[j] <- list(data_left)
    } else {
      idx <- sample(n_now, size = floor(n * perc))
      
      group[j] <- list(data_left[idx])
      data_left <- data_left[-idx]
    }
  }
  
  return(group)
}

merge_dtms <- function(dtms, n_test) {
  indexes <- c(1:10)[-n_test]
  
  train <- do.call(tm:::c.DocumentTermMatrix, dtms[indexes])
  test <- dtms[[n_test]]
  
  return(list(train = train, test = test))
}

merge_corpus <- function(corpuses, n_test) {
  test <- corpuses[[n_test]]
  
  merging <- corpuses[-n_test]
  train <- merging[[1]]
  
  for (i in 2:length(merging)) {
    train <- c(train, merging[[i]])
  }
  
  return(list(test = test, train = train))
}