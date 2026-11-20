library(quanteda)
library(testthat)

dfmt <- dfm(tokens(data_corpus_inaugural))
k <- 5
femt <- matrix(rnorm(ncol(dfmt) * k), ncol = k, 
               dimnames = list(featnames(dfmt), NULL)) # dummy

demt <- dem(x = dfmt, y = femt)
dim(demt)
names(attributes(demt))
head(demt)

expect_identical(
    docnames(demt),
    docnames(dfmt)
)

expect_identical(
    attr(demt, "docvars"),
    attr(dfmt, "docvars")
)

expect_s4_class(
    demt,
    "dem"
)

expect_identical(
    dim(dem),
    c(60L, 5L)
)
