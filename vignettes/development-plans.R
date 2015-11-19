## ---- eval=FALSE---------------------------------------------------------
#  changeunits
#  corpus
#  docnames, <-
#  docvars, <-
#  encoding, <-
#  language, <-
#  metacorpus, metadoc, <-
#  ndoc
#  ntoken
#  segment      # also works on character vectors
#  settings, <-
#  subset
#  summary
#  textfile
#  texts

## ---- eval=FALSE---------------------------------------------------------
#  tokenize

## ---- eval=FALSE---------------------------------------------------------
#  phrasetotoken

## ---- eval=FALSE---------------------------------------------------------
#  collocations

## ---- eval=FALSE---------------------------------------------------------
#  kwic

## ---- eval=FALSE---------------------------------------------------------
#  ntoken

## ---- eval=FALSE---------------------------------------------------------
#  syllables
#  wordstem

## ---- eval=FALSE---------------------------------------------------------
#  removeFeatures

## ---- eval=FALSE---------------------------------------------------------
#  ngrams
#  skipgrams

## ---- eval=FALSE---------------------------------------------------------
#  dfm         # also works directly on (the texts of) a corpus
#  convert
#  docfreq
#  docnames
#  features
#  lexdiv
#  ndoc
#  ntoken
#  plot
#  print, show
#  removeFeatures
#  similarity
#  sort
#  textmodel, textmodel_*
#  topfeatures
#  trim
#  weight
#  settings

## ---- eval=FALSE---------------------------------------------------------
#  dictionary
#  stopwords
#  textfile

## ---- eval=FALSE---------------------------------------------------------
#  exampleString       # character, length 1
#  ukimmigTexts        # character, length 14
#  inaugTexts          # character, length 57
#  ie2010Corpus        # corpus
#  inaugCorpus         # corpus
#  LBGexample          # dfm

## ---- eval=FALSE---------------------------------------------------------
#  englishSyllables    # named character vector, length 133245
#  stopwords           # named list .stopwords, length 16

## ---- eval=FALSE---------------------------------------------------------
#  mydfm <- texts(mycorpus, group = "party") %>% toLower %>% tokenize %>% wordstem %>%
#                                  removeFeatures(stopwords("english")) %>% dfm

## ---- eval=FALSE---------------------------------------------------------
#  mydfm <- texts(ie2010Corpus, groups = "party") %>% toLower %>% tokenize %>%
#               removeFeatures(stopwords("english")) %>% wordstem %>% dfm
#  
#  # same as:
#  mydfm2 <- dfm(ie2010Corpus, groups = "party", ignoredFeatures = stopwords("english"), stem = TRUE)

