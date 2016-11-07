
# @examples
# cop <- subset(inaugCorpus, Year>19)
# dict <- dictionary(list(country = "united states"))
# head(dfm_dict(cop, dict)[,"country"])

dfm_dict <- function(x, dict){
  toks <- tokens(x)
  keys <- unlist(dict@.Data, use.names = FALSE)
  keys_multi <- keys[stringi::stri_detect_fixed(keys, ' ')]
  seqs <- stringi::stri_split_fixed(keys_multi, ' ')
  
  toks2 <- joinTokens.tokenizedTextsHashed(toks, seqs, verbose = TRUE, concatenator = ' ')
  res <- dfm(toks2, dictionary=dict)
  return(res)
}
