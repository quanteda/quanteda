# Test script for memory errors on linux
# 
# sudo apt-get install valgrind
# 
# R -d "valgrind --tool=memcheck --leak-check=full" --vanilla < tests/valgrind.R
# 
# See https://cran.r-project.org/doc/manuals/r-devel/R-exts.html#Using-Valgrind
#--------------------------------------------------------------------------------

library(quanteda)
library(testthat)
#tokens("New York City is located in the United States.") |>
#    tokens_compound(pattern = phrase(c("New York City", "United States")))

#tokens("one~two~three") |>
#    tokens_split(separator = "~")

toks <- tokens(data_corpus_inaugural)
    
#    toks_temp1 <- tokens_replace(toks, "it", "ZZZZZZZ", case_insensitive = FALSE)
#    expect_equal(as.list(tokens_replace(toks_temp1, "ZZZZZZZ", "it")),
#                 as.list(toks))
