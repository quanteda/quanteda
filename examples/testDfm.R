# debugging script

dummyCorpus <- createCorpus(c("a b e","d e e"))
dummyCorpus2 <- createCorpus(c("a b e","d e e f"))
dfm(dummyCorpus)
dfm(dummyCorpus2)

tt <- c(c("a","b"), c("d","e"))
sapply(tt, length)