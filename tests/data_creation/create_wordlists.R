makeWordList <- function(filename) {
    wordList <- readtext::readtext(filename)[["text"]] %>%
        stringi::stri_split_regex(pattern = "\\p{WHITE_SPACE}+", simplify = TRUE) %>%
        stringi::stri_trim_both() %>%
        sort()
    wordList
}

dalechall_old    <- makeWordList("~/Dropbox (Personal)/QUANTESS/quanteda_working_files/readability/Dale-Chall.txt")
dalechall_extended <- makeWordList("~/Dropbox (Personal)/QUANTESS/quanteda_working_files/readability/Dale-Chall-extended.txt")


spache    <- makeWordList("~/Dropbox (Personal)/QUANTESS/quanteda_working_files/readability/Spache.txt")
data_char_wordlists <- list(dalechall = dalechall_extended, spache = spache)
devtools::use_data(data_char_wordlists, overwrite = TRUE)
