#' Compute sentiment using a dictionary method
#'
#' Compute sentiment scores from tokens or documet-feature matrices, based on
#' assigned categories (types or features) of positive, negative, and neutral
#' sentiment. Several formulas are available, or the user can supply a new one.
#' @param x a [dfm] or [tokens] object
#' @param pos,neg character; names of the features or types for positive and negative sentiment, respectively
#' @poram neut character; (optional) name of the neutral sentiment feature or type
#' @param formula function; the formula for computing sentiment, which must refer to pos, neg, and neutral
#' @return a data.frame of sentiment scores
#' @export
#' @examples
#' tail(data_corpus_inaugural, n = 5) %>%
#'     tokens() %>%
#'     tokens_lookup(dictionary = data_dictionary_LSD2015) %>%
#'     textstat_sentiment()
#' textstat_entropy(data_dfm_lbgexample, "features")
textstat_sentiment <- function(x, dictionary, pos = "positive", neg = "negative", neut = NULL, 
                               fun = sent_logit, ...) {
    UseMethod("textstat_sentiment")
}

#' @export
textstat_sentiment.default <-  function(x, dictionary, 
                                        pos = "positive", neg = "negative", neut = NULL, 
                                        fun = sent_logit, ...) {
    stop(friendly_class_undefined_message(class(x), "textstat_sentiment"))
}

#' @export
textstat_sentiment.tokens <- function(x, dictionary, 
                                      pos = "positive", neg = "negative", neut = NULL, 
                                      fun = sent_logit, ...) {
    if (missing(dictionary)) {
        if (!all(c(pos, neg, neut) %in% types(x)))
            stop("pos, neg, and neut must name existing types")
        dictionary <- dictionary(structure(
            list(pos, neg, neut), names = c("pos", "neg", "neut")
        ))
    } else {
        if (!all(c(pos, neg, neut) %in% names(dictionary)))
            stop( "pos, neg, and neut must name dictionary keys")
    }
    
    x <- tokens_lookup(x, dictionary = dictionary, nomatch = "__OTHER__")
    textstat_sentiment(dfm(x), pos = pos, neg = neg, neut = neut, fun = fun)
}

#' @export
textstat_sentiment.dfm <- function(x, dictionary, pos = "positive", neg = "negative", neut = NULL, 
                                   fun = sent_logit, ...) {
    if (missing(dictionary)) {
        if (!all(c(pos, neg, neut) %in% featnames(x)))
            stop("pos, neg, and neut must name existing types")
        dictionary <- dictionary(structure(
            list(pos, neg, neut), names = c(pos, neg, neut)
        ))
    } else {
        if (!all(c(pos, neg, neut) %in% names(dictionary)))
            stop("pos, neg, and neut must name dictionary keys")
    }
    
    x <- dfm_lookup(x, dictionary = dictionary, nomatch = "__OTHER__")
    
    compute_sentiment(p = x[, pos], n = x[, neg], o = x[, neut], fun = fun, ...)
}

sent_logit <- function(p, n, o = NULL, smooth = 0.5) {
    log(p + smooth) - log(n + smooth)
}

sent_abspropdiff <- function(p, n, o = NULL, smooth = 0.5) {
    log(p + smooth) - log(n + smooth)
}

compute_sentiment <- function(p, n, o, fun = sent_logit, ...) {
    result <- fun(p, n, o, ...)
    result <- convert(as.dfm(result), to = "data.frame")
    names(result)[2] <- "sentiment"
    result
}
