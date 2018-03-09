## ----setup, include = FALSE----------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ---- eval=FALSE---------------------------------------------------------
#  mydfm <- texts(mycorpus, group = "party") %>%
#      char_tolower() %>%
#      tokens() %>%
#      tokens_wordstem() %>%
#      tokens_remove(stopwords("english")) %>%
#      dfm()

