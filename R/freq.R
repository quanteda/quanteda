freq.tokens <- function(x, boolean = FALSE) {
    freq(as.tokens_xptr(x), boolean = boolean)
} 

freq.tokens_xptr <- function(x, boolean = FALSE) {
    quanteda:::cpp_get_freq(x, boolean = boolean, no_padding = TRUE)
} 