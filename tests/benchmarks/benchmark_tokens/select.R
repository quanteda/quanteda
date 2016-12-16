load("/home/kohei/Documents/Brexit/Analysis/data_corpus_guardian.RData")
tok <- tokens(data_corpus_guardian)

tok2 <- tokens_select(tok, stopwords())
head(tok2)

tok3 <- tokens_select(tok, stopwords(), padding = TRUE)
head(tok3)

tok4 <- tokens_remove(tok, stopwords(), padding = TRUE)
head(tok4)

system.time(tokens_select(tok, '*'))
system.time(tokens_select(tok, list(c('President', '*'))))

