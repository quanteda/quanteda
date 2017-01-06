## ----eval=TRUE-----------------------------------------------------------
library(quanteda)

## ----eval=TRUE, fig.width=8, fig.height=8--------------------------------
# Create a dfm from a somewhat smaller corpus
inaugDfm <- dfm(data_corpus_inaugural[0:10], remove = stopwords('english'), removePunct = TRUE)
# Some words will not fit on a plot this size, so suppress those warings
textplot_wordcloud(dfm_trim(inaugDfm, min_count = 10, verbose = FALSE))

## ----eval=TRUE, fig.width=8, fig.height=8--------------------------------
compDfm <- dfm(corpus_subset(data_corpus_inaugural, President %in% c("Washington", "Jefferson", "Madison")),
               groups = "President", remove = stopwords("english"), removePunct = TRUE)
textplot_wordcloud(dfm_trim(compDfm, min_count = 5, verbose = FALSE), comparison = TRUE)

## ----eval=TRUE, fig.width=8, fig.height=8--------------------------------
textplot_wordcloud(inaugDfm, min.freq = 10,
     colors = c('red', 'pink', 'green', 'purple', 'orange', 'blue'))

## ----eval=TRUE, fig.width=8, fig.height=3--------------------------------
textplot_xray(kwic(data_corpus_inaugural[50:57], "american"))

## ----eval=TRUE, fig.width=8, fig.height=4--------------------------------
textplot_xray(
     kwic(data_corpus_inaugural[50:57], "american"),
     kwic(data_corpus_inaugural[50:57], "people"),
     kwic(data_corpus_inaugural[50:57], "communist")
)

## ----eval=TRUE, fig.width=8, fig.height=1.5------------------------------
mobydickCorpus <- corpus(data_char_mobydick)

textplot_xray(
    kwic(data_char_mobydick, "whale"),
    kwic(data_char_mobydick, "ahab")
)

## ----eval=TRUE, fig.width=8, fig.height=4--------------------------------
textplot_xray(
     kwic(data_corpus_inaugural[50:57], "american"),
     kwic(data_corpus_inaugural[50:57], "people"),
     kwic(data_corpus_inaugural[50:57], "communist"),
     scale = 'absolute'
)

## ----eval=TRUE, fig.width=8, fig.height=4--------------------------------
library(ggplot2)
theme_set(theme_bw())
g <- textplot_xray(
     kwic(data_corpus_inaugural[50:57], "american"),
     kwic(data_corpus_inaugural[50:57], "people"),
     kwic(data_corpus_inaugural[50:57], "communist")
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
    theme(axis.text.x = element_text(angle=90, hjust=1))


## ----eval=TRUE-----------------------------------------------------------
relDfm <- weight(inaugDfm, type='relFreq') * 100
head(relDfm)

relFreq <- data.frame(list(
    document = rownames(inaugDfm[, 'american']),
    frequency = unname(as.matrix(relDfm[, 'american']))
))

ggplot(relFreq) + geom_point(aes(x=document,y=frequency)) +
    theme(axis.text.x = element_text(angle=90, hjust=1))

