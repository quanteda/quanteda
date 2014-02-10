# debugging script

dummyCorpus <- createCorpus(c("a b e","d e e"))

dfm(dummyCorpus)

tt <- c(c("a","b"), c("d","e"))
sapply(tt, length)