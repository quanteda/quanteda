# run this to update the corpus objects to the newest package versions

data_corpus_dailnoconf1991 <- corpus(data_corpus_dailnoconf1991)
usethis::use_data(data_corpus_dailnoconf1991, overwrite = TRUE)

data_corpus_inaugural <- corpus(data_corpus_inaugural)
usethis::use_data(data_corpus_inaugural, overwrite = TRUE)

data_corpus_irishbudget2010 <- corpus(data_corpus_irishbudget2010)
usethis::use_data(data_corpus_irishbudget2010, overwrite = TRUE)
