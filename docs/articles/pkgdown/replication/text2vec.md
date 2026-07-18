# Replication: word embedding

This is intended to show how **quanteda** can be used with the
[**text2vec**](http://text2vec.org) package in order to replicate its
[gloVe example](http://text2vec.org/glove.md).

``` r

library("quanteda")
```

## Download the corpus

Download a corpus comprising the texts used in the [**text2vec**
vignette](http://text2vec.org/glove.md).

``` r

wiki_corp <- quanteda.corpora::download(url = "https://www.dropbox.com/s/9mubqwpgls3qi9t/data_corpus_wiki.rds?dl=1")
```

## Select features

First, we tokenize the corpus, and then get the names of the features
that occur five times or more.

``` r

wiki_toks <- tokens(wiki_corp)
feats <- dfm(wiki_toks, verbose = TRUE) |>
    dfm_trim(min_termfreq = 5) |>
    featnames()
## Creating a dfm from a tokens object...
##  ...lowercasing
##  ...complete, elapsed time: 1.37 seconds.
## Finished constructing a 1 x 253,854 sparse dfm.

# leave the pads so that non-adjacent words will not become adjacent
wiki_toks <- tokens_select(wiki_toks, feats, padding = TRUE)
```

## Construct the feature co-occurrence matrix

``` r

wiki_fcm <- fcm(wiki_toks, context = "window", count = "weighted", weights = 1 / (1:5), tri = TRUE)
```

## Fit word embedding model

Fit the [GloVe model](https://nlp.stanford.edu/pubs/glove.pdf) using
[**rsparse**](http://text2vec.org).

``` r

library("text2vec")
```

GloVe is an unsupervised learning algorithm for obtaining vector
representations for words. Training is performed on aggregated global
word-word co-occurrence statistics from a corpus, and the resulting
representations showcase interesting linear substructures of the word
vector space.

GloVe encodes the ratios of word-word co-occurrence probabilities, which
is thought to represent some crude form of meaning associated with the
abstract concept of the word, as vector difference. The training
objective of GloVe is to learn word vectors such that their dot product
equals the logarithm of the words’ probability of co-occurrence.

``` r

glove <- GlobalVectors$new(rank = 50, x_max = 10)
wv_main <- glove$fit_transform(wiki_fcm, n_iter = 10,
                               convergence_tol = 0.01, n_threads = 8)
dim(wv_main)
## [1] 71290    50
```

## Averaging learned word vectors

The two vectors are main and context. According to the Glove paper,
averaging the two word vectors results in more accurate representation.

``` r

wv_context <- glove$components
dim(wv_context)
## [1]    50 71290

word_vectors <- wv_main + t(wv_context)
```

## Examining term representations

Now we can find the closest word vectors for `paris - france + germany`

``` r

berlin <- word_vectors["paris", , drop = FALSE] -
  word_vectors["france", , drop = FALSE] +
  word_vectors["germany", , drop = FALSE]
library("quanteda.textstats")
cos_sim <- textstat_simil(x = as.dfm(word_vectors), y = as.dfm(berlin),
                          method = "cosine")
head(sort(cos_sim[, 1], decreasing = TRUE), 5)
##     paris    berlin    munich   germany    vienna 
## 0.7636617 0.7167626 0.7136267 0.6753233 0.6711393
```

Here is another example for `london = paris - france + uk + england`

``` r

london <-  word_vectors["paris", , drop = FALSE] -
    word_vectors["france", , drop = FALSE] +
    word_vectors["uk", , drop = FALSE] +
    word_vectors["england", , drop = FALSE]

cos_sim <- textstat_simil(as.dfm(word_vectors), y = as.dfm(london),
                          margin = "documents", method = "cosine")
head(sort(cos_sim[, 1], decreasing = TRUE), 5)
##        uk   england    london      york      club 
## 0.7676968 0.7654016 0.7477249 0.7428076 0.7315798
```
