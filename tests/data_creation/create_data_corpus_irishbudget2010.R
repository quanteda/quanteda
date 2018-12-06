## adjust the docnames of data_corpus_irishbudget2010
## using the corpus included in quanteda v1.3.14

# require(devtools) 
# install_version("quanteda", version = "1.3.14", repos = "http://cran.us.r-project.org")

library(quanteda)

docvars(data_corpus_irishbudget2010)

docnames(data_corpus_irishbudget2010) <- paste0(
    docvars(data_corpus_irishbudget2010, "name"), 
    ", ",
    docvars(data_corpus_irishbudget2010, "foren"), 
    " (",
    docvars(data_corpus_irishbudget2010, "party"),
    ")"
)

usethis::use_data(data_corpus_irishbudget2010, overwrite = TRUE)

