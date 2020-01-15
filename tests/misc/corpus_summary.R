x <- data_corpus_inaugural

get_overview <- function(x) {
    toks <- tokens(x) %>%
        tokens_tolower()
    
    ndocs <- ndoc(x)
    ntoks <- sum(ntoken(toks))
    
    toks <- tokens(toks, remove_punct = TRUE, remove_symbols = TRUE)
    npunctsym <- ntoks - sum(ntoken(toks))
    
    toks <- tokens(toks, remove_punct = TRUE, remove_symbols = TRUE)
    
    dfmat <- dfm(x)
    ntoks <- ntoken(x)
    
    dfmat <- dfm_remove(dfmat, "\\p{P}\\{S}")
    punctsym <- 
    
}




# number of documents
ndoc(x)

# median token length


# vocabulary size


dfm()