library("quanteda")

makewordlist <- function(filename) {
    wordlist <- readtext::readtext(filename)[["text"]] %>%
        stringi::stri_split_regex(pattern = "\\p{WHITE_SPACE}+", simplify = TRUE) %>%
        stringi::stri_trim_both() %>%
        sort()
    wordlist
}

dalechall_extended <- makewordlist("Dale-Chall-extended.txt")
spache <- makewordlist("Spache.txt")

data_char_wordlists <- list(dalechall = dalechall_extended, spache = spache)
devtools::use_data(data_char_wordlists, overwrite = TRUE)



# Spache, George D. "The Spache readability formula." Good reading for poor
# readers (1974): 195-207.

# Spache, G. (1953). "A New Readability Formula for Primary-Grade Reading
# Materials". The Elementary School Journal. 53 (7): 410–13. doi:10.1086/458513.
# JSTOR 998915.
