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
#  data_char_sampletext # character, length 1
#  data_char_ukimmig2010  # character, length 14
#  data_char_inaugural           # character, length 57
#  data_corpus_irishbudget2010         # corpus
#  data_corpus_inaugural          # corpus
#  data_dfm_LBGexample  # dfm

## ---- eval=FALSE---------------------------------------------------------
#  englishSyllables    # named character vector, length 133245
#  stopwords           # named list .stopwords, length 16

## ---- eval=FALSE---------------------------------------------------------
#  mydfm <- texts(mycorpus, group = "party") %>% toLower %>% tokenize %>% wordstem %>%
#                                  removeFeatures(stopwords("english")) %>% dfm

## ---- eval=FALSE---------------------------------------------------------
#  mydfm <- texts(data_corpus_irishbudget2010, groups = "party") %>% toLower %>% tokenize %>%
#               removeFeatures(stopwords("english")) %>% wordstem %>% dfm
#  
#  # same as:
#  mydfm2 <- dfm(data_corpus_irishbudget2010, groups = "party", ignoredFeatures = stopwords("english"), stem = TRUE)

