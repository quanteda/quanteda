tokenize <-
function(str){
  str <- clean(str)
  tokens <- scan(what="char", text=str, quiet=TRUE)
  # flush out "empty" strings caused by removal of punctuation and numbers
  tokens <- tokens[tokens!=""]
  return(tokens)
}
