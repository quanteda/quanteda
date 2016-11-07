dfm_dict <- function(x, dict, concatenator = ' ', verbose = FALSE){
  toks <- tokens(x)
  keys <- unlist(dict@.Data, use.names = FALSE)
  keys_multi <- keys[stringi::stri_detect_fixed(keys, concatenator)]
  seqs <- stringi::stri_split_fixed(keys_multi, concatenator)
  
  toks2 <- joinTokens(toks, seqs, verbose = verbose, concatenator = concatenator)
  res <- dfm(toks2, dictionary=dict)
  return(res)
}

cop <- subset(inaugCorpus)
dict <- dictionary(list(country = "united states"))
head(dfm_dict(cop, dict, verbose=TRUE)[,"country"])

