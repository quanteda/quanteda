## ----eval=TRUE-----------------------------------------------------------
library(quanteda)

## ----eval=TRUE, fig.width=8, fig.height=8--------------------------------
# Create a dfm from a somewhat smaller corpus
inaugDfm <- dfm(data_corpus_inaugural[0:10], ignoredFeatures = stopwords('english'))
# Some words will not fit on a plot this size, so suppress those warings
plot(trim(inaugDfm, minCount = 10, verbose = FALSE))

## ----eval=TRUE, fig.width=8, fig.height=8--------------------------------
compDfm <- dfm(corpus_subset(data_corpus_inaugural, President %in% c("Washington", "Jefferson", "Madison")),
               groups = "President", ignoredFeatures = stopwords("english"))
plot(trim(compDfm, minCount = 5, verbose = FALSE), comparison = TRUE)

## ----eval=TRUE, fig.width=8, fig.height=8--------------------------------
plot(inaugDfm, min.freq = 10,
     colors = c('red', 'pink', 'green', 'purple', 'orange', 'blue'))

## ----eval=TRUE, fig.width=8, fig.height=12-------------------------------
plot(kwic(data_corpus_inaugural, "american"))

## ----eval=TRUE, fig.width=8, fig.height=12-------------------------------
plot(
     kwic(data_corpus_inaugural, "american"),
     kwic(data_corpus_inaugural, "people"),
     kwic(data_corpus_inaugural, "communist")
)

## ----eval=TRUE, fig.width=8, fig.height=1.5------------------------------
  mobydickCorpus <- corpus(data_char_mobydick)

  plot(
       kwic(mobydickCorpus, "whale"),
       kwic(mobydickCorpus, "ahab")
  )


## ----eval=TRUE, fig.width=8, fig.height=12-------------------------------
plot(
     kwic(data_corpus_inaugural, "american"),
     kwic(data_corpus_inaugural, "people"),
     kwic(data_corpus_inaugural, "communist"),
     scale = 'absolute'
)

## ----eval=TRUE, fig.width=8, fig.height=12-------------------------------
library(ggplot2)
theme_set(theme_bw())
g <- plot(
     kwic(data_corpus_inaugural, "american"),
     kwic(data_corpus_inaugural, "people"),
     kwic(data_corpus_inaugural, "communist")
)
g + aes(color = keyword) + scale_color_manual(values = c('blue', 'red', 'green'))


## ----eval=TRUE, fig.width=8, fig.height=4--------------------------------
inaugFeatures <- topfeatures(inaugDfm, 100)

# Create a data.frame for ggplot
topDf <- data.frame(
    list(
        term = names(inaugFeatures),
        frequency = unname(inaugFeatures)
    )
)

# Sort by reverse frequency order
topDf$term <- with(topDf, reorder(term, -frequency))

ggplot(topDf) + geom_point(aes(x=term, y=frequency)) +
    theme(axis.text.x=element_text(angle=90, hjust=1))

## ----eval=TRUE, fig.width=8, fig.height=4--------------------------------

americanFreq <- data.frame(list(
    document = rownames(inaugDfm[, 'american']),
    frequency = unname(as.matrix(inaugDfm[, 'american']))
))

ggplot(americanFreq) + geom_point(aes(x=document,y=frequency)) +
    theme(axis.text.x=element_text(angle=90, hjust=1))


## ----eval=TRUE-----------------------------------------------------------
relDfm <- weight(inaugDfm, type='relFreq') * 100
head(relDfm)

relFreq <- data.frame(list(
    document = rownames(inaugDfm[, 'american']),
    frequency = unname(as.matrix(relDfm[, 'american']))
))

ggplot(relFreq) + geom_point(aes(x=document,y=frequency)) +
    theme(axis.text.x=element_text(angle=90, hjust=1))

