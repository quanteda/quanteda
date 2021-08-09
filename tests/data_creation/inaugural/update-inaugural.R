# run this to update the corpus objects to the newest package versions

library("quanteda")

data_corpus_inaugural <- corpus_subset(data_corpus_inaugural,
                                       President != "Biden" & Year != "2021")

metadata <- meta(data_corpus_inaugural)

data_corpus_biden_2021 <- corpus(readtext::readtext("tests/data_creation/inaugural/2021-Biden.txt"))
docnames(data_corpus_biden_2021) <- "2021-Biden"

data_corpus_inaugural <- data_corpus_inaugural +
    data_corpus_biden_2021
    
docvars(data_corpus_inaugural) <- read.csv("tests/data_creation/inaugural/inaugural_docvars.csv")
data_corpus_inaugural$Party <- factor(data_corpus_inaugural$Party)

meta(data_corpus_inaugural) <- metadata

usethis::use_data(data_corpus_inaugural, overwrite = TRUE)
