# textstat_sentiment ----------------

#' Compute sentiment using a dictionary method
#'
#' Compute sentiment scores from tokens or document-feature matrices, based on
#' assigned categories (types or features) of positive, negative, and neutral
#' sentiment. Several formulas are available, or the user can supply a new one.
#' @param x a [dfm] or [tokens] object containing sentiment types or features
#' @param pos,neg character; names of the dictionary keys or features 
#'   for positive and negative sentiment, respectively
#' @param neut character; (optional) name of the neutral sentiment feature or 
#'   key
#' @param fun function; the formula for computing sentiment, which must refer to
#'   `pos`, `neg`, and (optionally) `neut`.  Default is the Lowe et al (2001)
#'   "logit" scale which is the log of (positive / negative) counts.  See
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
#' toks <- tail(data_corpus_inaugural, n = 5) %>%
#'     tokens(remove_punct = TRUE, remove_numbers = TRUE) %>%
#'     tokens_lookup(dictionary = data_dictionary_LSD2015, nomatch = "_other_")
#' textstat_sentiment(toks)
#' textstat_sentiment(toks, smooth = 1)
#' textstat_sentiment(toks, fun = sent_relpropdiff)
#' 
#' dfmat <- dfm(toks)
#' textstat_sentiment(dfmat)
#' 
#' # works with multiple types associated with each sentiment category
#' textstat_sentiment(toks, pos = c("positive", "neg_negative"),
#'                    neg = c("negative", "neg_positive"))
#'
#' # with a user-supplied function
#' sent_fn <- function(x) (x[, "pos"] - x[, "neg"]) / rowSums(x) * 100
#' textstat_sentiment(toks, fun = sent_fn)
textstat_sentiment <- function(x, pos = "positive", neg = "negative", 
                               neut = NULL, fun = sent_logit, ...) {
    UseMethod("textstat_sentiment")
}

#' @export
textstat_sentiment.default <-  function(x, pos = "positive", neg = "negative", 
                                        neut = NULL, fun = sent_logit, ...) {
    stop(friendly_class_undefined_message(class(x), "textstat_sentiment"))
}

#' @export
textstat_sentiment.tokens <- function(x, pos = "positive", neg = "negative", 
                                      neut = NULL, fun = sent_logit, ...) {
    textstat_sentiment(dfm(x), pos, neg, neut, fun = fun, ...)
}

#' @export
textstat_sentiment.dfm <- function(x, pos = "positive", neg = "negative",
                                   neut = NULL, fun = sent_logit, ...) {
    if (!all(c(pos, neg, neut) %in% featnames(x)))
        stop("pos, neg, and neut must match features in x")

    # get the categories that are not sentiment
    x_other <- dfm_remove(x, c(pos, neg, neut))
    colnames(x_other) <- rep("other", nfeat(x_other))
    x_other <- dfm_compress(x_other, margin = "features")
    
    x <- dfm_lookup(x, dictionary(list(
        "pos" = pos, "neg" = neg, "neut" = as.character(neut)
    )))
    
    x <- cbind(x, x_other)
    
    result <- fun(x, ...)
    result <- convert(as.dfm(result), to = "data.frame")
    names(result)[2] <- "sentiment"
    
    class(result) <- c("sentiment", "textstat", "data.frame")
    attr(result, "fun") <- fun
    attr(result, "fun_name") <- as.character(substitute(fun))
    
    result
}


# sentiment formula functions --------------

#' Sentiment functions
#' 
#' Functions for computing sentiment, for [textstat_sentiment()].  Each function
#' takes . Additional arguments may be passed via `...`,
#' such as `smooth` for the logit scale.
#' 
#' @details 
#' User supplied functions must take `x` and `...`, and refer to the required
#' feature names for the sentiment categories `pos`,
#' `neg`, `neut`, and `other`.  (The `other` category is only required when 
#' a scaling function needs the count of non-sentiment associated features.)
#' 
#' @param x a [dfm] that has the following required feature names: `pos`,
#' `neg`, `neut`, and `other`
#' @param ... additional parameters as needed
#' @keywords textstat internal
#' @references  Lowe, W., Benoit, K. R., Mikhaylov, S., & Laver, M. (2011).
#'   Scaling Policy Preferences from Coded Political Texts. _Legislative Studies
#'   Quarterly_, 36(1), 123–155.
#'   <http://doi.org/10.1111/j.1939-9162.2010.00006.x>
#' @name sentiment-functions
NULL

#' @description `sent_logit` is \eqn{log(\frac{pos}{neg})}.
#' @rdname sentiment-functions
#' @param smooth additional smoothing function added to `pos` and `neg` before
#'   logarithmic transformation
#' @export
sent_logit <- function(x, smooth = 0.5, ...) {
    log(x[, "pos"] + smooth) - log(x[, "neg"] + smooth)
}

#' @description `sent_abspropdiff` is \eqn{\frac{pos - neg}{N}}, where \eqn{N}
#'   is the total number of all features in a document.
#' @rdname sentiment-functions
#' @export
sent_abspropdiff <- function(x, ...) {
    (x[, "pos"] - x[, "neg"]) / rowSums(x)
}

#' @description `rel_abspropdiff` is \eqn{\frac{pos - neg}{pos + neg}}.
#' @rdname sentiment-functions
#' @export
sent_relpropdiff <- function(x, ...) {
    (x[, "pos"] - x[, "neg"]) / (x[, "pos"] + x[, "neg"])
}
