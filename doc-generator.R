# Individual documents have the same tokens
sameTokens <- function() {
  words = c("tokena", "tokenb", "tokenc", "tokend")
  
  documents = data.frame(V1 = character())
  
  for (i in 1:100) {
    tokenid = (i %% 4) + 1
    
    doc = data.frame(V1 = paste(rep(words[tokenid], 400), collapse = " "))
    
    documents = rbind(documents, doc)
  }
  
  return(as.matrix(documents))
}

# Documents have an even spread of tokens
evenSpreadTokens <- function() {
  words = c("tokena", "tokenb", "tokenc", "tokend")
  
  documents = data.frame(V1 = character())
  
  for (i in 1:100) {
    docwords = character()
    
    for (tokenid in 1:4) {
      docwords = paste(c(paste(rep(words[tokenid], 100), collapse = " "), docwords), collapse = " ")
    }
    
    doc = data.frame(V1 = docwords)
    
    documents = rbind(documents, doc)
  }
  
  return(as.matrix(documents))
}

# Documents ahve a random spread of tokens