# textstat_sentiment ----------------

#' Compute sentiment using a dictionary method
#'
#' Compute sentiment scores from tokens or document-feature matrices, based on
#' assigned categories (types or features) of positive, negative, and neutral
#' sentiment. Several formulas are available, or the user can supply a new one.
#' @param x a character, [corpus], [tokens], or [dfm] object containing
#'   text, tokens, or features whose sentiment will be scored
#' @param dictionary a [dictionary] that has [polarity] set, indicating which
#'   keys are associated with positive, negative, and (optionally) neutral
#'   sentiment
#' @param fun function; the formula for computing sentiment, which must refer to
#'   `pos`, `neg`, and (optionally) `neut`.  The default is the "logit" scale
#'   (Lowe et al 2011) which is the log of (positive / negative) counts.  See
#'   [sentiment-functions] for details and for additional available functions,
#'   as well as details on how to supply custom functions.
#' @param ... additional arguments passed to `fun`
#' @return a data.frame of sentiment scores
#' @export
#' @references  Lowe, W., Benoit, K. R., Mikhaylov, S., & Laver, M. (2011).
#'   Scaling Policy Preferences from Coded Political Texts. _Legislative Studies
#'   Quarterly_, 36(1), 123–155.
#'   <http://doi.org/10.1111/j.1939-9162.2010.00006.x>
#' @examples
#' corp <- tail(data_corpus_inaugural, n = 5)
#' toks <- tokens(corp)
#' dfmat <- dfm(toks)
#' polar1 <- list(pos = "positive", neg = "negative")
#' polar2 <- list(pos = c("positive", "neg_negative"),
#'                neg = c("negative", "neg_positive"))
#'
#' polarity(data_dictionary_LSD2015) <- polar1
#' textstat_sentiment(corp, dictionary = data_dictionary_LSD2015)
#' textstat_sentiment(toks, dictionary = data_dictionary_LSD2015)
#' textstat_sentiment(dfmat, dictionary = data_dictionary_LSD2015)
#'
#' polarity(data_dictionary_LSD2015) <- polar2
#' textstat_sentiment(corp, dictionary = data_dictionary_LSD2015)
#' textstat_sentiment(toks, dictionary = data_dictionary_LSD2015)
#' textstat_sentiment(corp, dictionary = data_dictionary_LSD2015)
#' textstat_sentiment(dfmat, dictionary = data_dictionary_LSD2015)
#'
#' # with a user-supplied function
#' sent_fn <- function(x) (x[, "pos"] - x[, "neg"]) / rowSums(x) * 100
#' textstat_sentiment(toks, data_dictionary_LSD2015, fun = sent_fn)
textstat_sentiment <- function(x, dictionary, fun = sent_logit, ...) {
    UseMethod("textstat_sentiment")
}

#' @export
textstat_sentiment.default <-  function(x, dictionary, fun = sent_logit, ...) {
    stop(friendly_class_undefined_message(class(x), "textstat_sentiment"))
}

#' @export
textstat_sentiment.character <- function(x, ...) {
    textstat_sentiment(corpus(x), ...)
}

#' @export
textstat_sentiment.corpus <- function(x, ...) {
    textstat_sentiment(tokens(x), ...)
}

#' @export
textstat_sentiment.tokens <- function(x, dictionary, ...) {
    dict <- get_polarity_dictionary(dictionary)
    poldict <- dictionary(polarity(dict))
    polarity(poldict) <- polarity(dict)

    tokens(x) %>%
        tokens_lookup(dictionary = dict, nomatch = "other") %>%
        dfm() %>%
        textstat_sentiment(dictionary = poldict, ...)
}

#' @export
textstat_sentiment.dfm <- function(x, dictionary, fun = sent_logit, ...) {
    dict <- get_polarity_dictionary(dictionary)

    result <- fun(dfm_lookup(x, dict, nomatch = "other"), ...)
    result <- convert(as.dfm(result), to = "data.frame")
    names(result)[2] <- "sentiment"

    class(result) <- c("sentiment", "textstat", "data.frame")
    attr(result, "fun") <- fun
    attr(result, "fun_name") <- as.character(substitute(fun))

    result
}


# valence setting and checking functions --------------

#' Set or get the valences of dictionary values or keys
#'
#' Set or retrieve the valences of a [dictionary] object for the purposes of
#' sentiment analysis.  Valences consist of numerical values attached to each
#' dictionary "value".  For dictionaries with a more "polarity"-based approach,
#' a key may have the same valence for every value in that key -- for instance,
#' -1 for all values in a "negative" key.
#' 
#' Valences are used only in [textstat_sentiment()].
#'
#' A dictionary may have only one set of valences at a time, but may be
#' changed as needed.
#' @param x a [dictionary] object
#' @return `valences()` returns the valences as a list named numeric vectors,
#'   where each list element corresponds to a key in the dictionary, and each
#'   numeric element matches a value within that key.
#' @keywords dictionary textstat utility
#' @seealso [textstat_sentiment()], [polarity()]
#' @export
#'
#' @examples
#' # setting valences
#' dict <- dictionary(list(
#'     happiness = c("happy", "jubilant", "exuberant", "content"),
#'     anger = c("mad", "peeved", "irate", "furious", "livid")
#' ))
#' valence(dict)
#' # using a 5-point scale: 1:1 match
#' valence(dict) <- list(happiness = c(3, 4, 5, 2),
#'                       anger = c(3.1, 2.4, 2.9, 4.1, 5.0))
#' valence(dict)
#' # with single valences applied to all values within the keys
#' valence(dict) <- c(happiness = 1, anger = -1)
#' valence(dict)
#' # with named elements - order does not matter
#' valence(dict) <- list(
#'     happiness = c(exuberant = 5, jubilant = 4, happy = 3, content = 2)
#' ) 
#' valence(dict) 
#' 
valence <- function(x) {
    UseMethod("valence")
}

#' @export
valence.dictionary2 <- function(x) {
    x@meta$object$valence
}

#' @rdname valence
#' @param value named list consisting of numerical value.  The names of the
#'   elements must correspond to a dictionary key. Each element must be:
#'   * a single numeric value that will be applied to all of the dictionary
#'   values in that key; or
#'   * a vector of numeric values that matches the length and order of the
#'   dictionary values in that key; or
#'   * a named numeric vector where each element name matches dictionary values
#'   in the key.
#' @return `valence<-` sets the dictionary's valences.
#' @export
"valence<-" <- function(x, value) {
    UseMethod("valence<-")
}

#' @export
"valence<-.dictionary2" <- function(x, value) {
    value <- as.list(value)
    check_valences(x, value)
    x@meta$object$valence <- set_valences(x, value)
    x
}    

check_valences <- function(dictionary, valences) {
    if (!is.list(valences) || any(names(valences) == ""))
        stop("valence must be a fully named list", call. = FALSE)
    for (key in names(valences)) {
        if (!key %in% names(dictionary))
            stop("'", key, "' is not a dictionary key", call. = FALSE)
        if (length(valences[[key]]) != 1 &&
            length(valences[[key]]) != length(dictionary[[key]]))
            stop("valence value length not equal to number of values for key '",
                 key, "'", call. = FALSE)
    }
}    

set_valences <- function(dictionary, valences) {
    for (key in names(valences)) {
        if (length(valences[[key]]) == 1)
            valences[[key]] <- rep(valences[[key]], length(dictionary[[key]]))
        valences[[key]] <- structure(valences[[key]], names = dictionary[[key]])
    }
    valences
}

# polarity setting and checking functions --------------

#' Set or get polarities for dictionary keys
#' 
#' `polarity()` is a shortcut to valences that allows an easier method for 
#' setting valences that are the same within a key, for example a key for
#' "positive" or "negative" whose values will have a uniform valence.  It
#' operates by setting the same valences for all values in the key.
#' @return `polarity()` returns the numeric valences of keys as a list.
#' @keywords dictionary textstat utility internal
#' @export
#' @examples
#' # setting polarities
#' dict <- dictionary(list(
#'     happy = c("happy", "jubilant", "exuberant"),
#'     sad = c("sad", "morose", "down")
#' ))
#' polarity(dict)
#' polarity(dict) <- c(happy = 1, sad = -1)
#' polarity(dict)
#' valence(dict)
#'
#' # can list multiple keys
#' polarity(data_dictionary_LSD2015) <- list(
#'     "positive" = 1, "neg_negative" = 1, "negative" = -1, "neg_positive" = -1
#' )
#' polarity(data_dictionary_LSD2015)
#' lapply(valence(data_dictionary_LSD2015), head)
polarity <- function(x) {
    UseMethod("polarity")
}

#' @export
polarity.dictionary2 <- function(x) {
    x@meta$object$polarity
}

#' @rdname polarity
#' @return `polarity<-` sets the dictionary's polarity.
#' @export
"polarity<-" <- function(x, value) {
    UseMethod("polarity<-")
}

#' @export
"polarity<-.dictionary2" <- function(x, value) {
    value <- as.list(value)
    valence(x) <- value
    x@meta$object$polarity <- value
    x
}
