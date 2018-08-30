makeWordList <- function(filename) {
    wordList <- readtext::readtext(filename)[["text"]]
    wordList <- tokens(wordList, remove_hyphens = FALSE, what = "fasterword") %>%
        as.character() %>%
        stringi::stri_trim_both() %>%
        sort()
    wordList
}

dalechall_old    <- makeWordList("~/Dropbox (Personal)/QUANTESS/quanteda_working_files/readability/Dale-Chall.txt")
dalechall_extended <- makeWordList("~/Dropbox (Personal)/QUANTESS/quanteda_working_files/readability/Dale-Chall-extended.txt")


spache    <- makeWordList("~/Dropbox (Personal)/QUANTESS/quanteda_working_files/readability/Spache.txt")
wordlists <- list(dalechall = dalechall_extended, spache = spache)
devtools::use_data(wordlists)
