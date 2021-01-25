# run this to update the corpus objects to the newest package versions

library("quanteda")
data_corpus_inaugural <- data_corpus_inaugural +
    corpus(readtext::readtext("tests/data_creation/inaugural/2021-Biden.txt"))
docvars(data_corpus_inaugural) <- read.csv("tests/data_creation/inaugural/inaugural_docvars.csv")

usethis::use_data(data_corpus_inaugural, overwrite = TRUE)
