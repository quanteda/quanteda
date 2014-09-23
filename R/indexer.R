
# @export
index <- function(corp){

  # the aim here is to use a list like a dictionary/hashtable
  # where each name (key) is a word type, and each element
  # (value) is a vector recording the document number and
  # position (token number) of each occurence of the word
  
  textsvec <- tokens(corp)
  vocab <- types(corp)
  dict <- vector("list", length(vocab))
  # pre-allocate dictionary
  print(length(vocab))
  names(dict) <- vocab
  
  docNum <- 1
  print(length(textsvec))
  while(docNum < length(textsvec)){
    print(docNum)
    tokNum <- 1
    curText <- unlist(textsvec[docNum])
    print(length(curText))
    while(tokNum < length(curText)){
      curToken <- curText[tokNum]
      dict[[curToken]] <- c(dict[[curToken]], c(docNum, tokNum))
      tokNum <- tokNum + 1
    }
    docNum <- docNum + 1
  }
  return(dict)
}

