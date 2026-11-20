library(quanteda)

dfmt <- dfm(tokens(data_corpus_inaugural))
femt <- matrix(rnorm(ncol(dfmt) * k), ncol = k, 
               dimnames = list(featnames(dfmt), NULL)) # dummy

demt <- dem(x = dfmt, y = femt)
dim(demt)
names(attributes(demt))
head(demt)
