bootstrap_dfm <- function(x, ...) {
    UseMethod("dfm_bootstrap")
}

bootstrap_dfm.corpus <- function(x, ...) {
    x_sentences <- corpus_reshape(x, to = "sentences")
    
}

bootstrap_dfm.character <- function(x, ...) {
    dfm_bootstrap(corpus(x), ...)
}



