## ----eval=TRUE-----------------------------------------------------------
library(quanteda)

## ----eval=TRUE, fig.width=8, fig.height=8--------------------------------
inaugDfm <- dfm(inaugCorpus)
suppressWarnings( # Some words will not fit on a plot this size, so suppress those warings
                 plot(inaugDfm)
)

## ----eval=TRUE, fig.width=8, fig.height=8--------------------------------
firstDfm <- dfm(texts(inaugCorpus)[0:8])
suppressWarnings( # Some words will not fit on a plot this size, so suppress those warings
  plot(firstDfm, comparison=T)
)

## ----eval=TRUE, fig.width=8, fig.height=8--------------------------------
inaugDfm <- dfm(inaugCorpus,
                colors=c('red', 'yellow', 'pink', 'green', 'purple', 'orange', 'blue')
            )
suppressWarnings( # Some words will not fit on a plot this size, so suppress those warings
  plot(inaugDfm)
)

## ----eval=TRUE, fig.width=8, fig.height=2.5------------------------------
# using words from tokenized corpus for dispersion
plot(kwic(inaugCorpus, "american"))

## ----eval=TRUE, fig.width=8, fig.height=12-------------------------------
plot(
     kwic(inaugCorpus, "american"),
     kwic(inaugCorpus, "people"),
     kwic(inaugCorpus, "communist")
)

## ----eval=TRUE, fig.width=8, fig.height=4--------------------------------
inaugAdams <- corpus(inaugTexts[[3]])

#  plot(
#       kwic(inaugAdams, "america"),
#       kwic(inaugAdams, "citizen"),
#       kwic(inaugAdams, "heart")
#  )


## ----eval=TRUE, fig.width=8, fig.height=12-------------------------------
plot(
     kwic(inaugCorpus, "american"),
     kwic(inaugCorpus, "people"),
     kwic(inaugCorpus, "communist"),
     scale='absolute'
)

## ----eval=TRUE, fig.width=8, fig.height=12-------------------------------
library(ggplot2)
theme_set(theme_bw())
g <- plot(
     kwic(inaugCorpus, "american"),
     kwic(inaugCorpus, "people"),
     kwic(inaugCorpus, "communist")
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

# Sort by reverse frequency order
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
  frequency = unname(as.matrix(inaugDfm[, 'american']))
))

ggplot(relFreq) + geom_point(aes(x=document,y=frequency)) +
  theme(axis.text.x=element_text(angle=90, hjust=1))



