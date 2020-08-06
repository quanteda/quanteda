# run this to update the corpus objects to the newest package versions

data_corpus_inaugural <- corpus(data_corpus_inaugural)
usethis::use_data(data_corpus_inaugural, overwrite = TRUE)
