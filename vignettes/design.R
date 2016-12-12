## ---- eval=FALSE---------------------------------------------------------
#  mydfm <- texts(mycorpus, group = "party") %>% toLower %>% tokenize %>% wordstem %>%
#                                  removeFeatures(stopwords("english")) %>% dfm

