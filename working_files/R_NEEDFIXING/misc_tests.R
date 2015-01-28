library(quanteda)
tte <- inaugCorpus

df1 <- dfm(inaugTexts)
df2 <- dfms(inaugTexts)

df2m <- as.matrix(df2)

class(df2)

df2 <- as.dfm()


class(df2) <- c("dfm", class(df2))

trimdfm(df2)
textmodel(df2, df1)
