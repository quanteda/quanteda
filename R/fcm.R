#' Create a feature co-occurrence matrix
#'
#' Create a sparse feature co-occurrence matrix, measuring co-occurrences of
#' features within a user-defined context. The context can be defined as a
#' document or a window within a collection of documents, with an optional
#' vector of weights applied to the co-occurrence counts.
#' @param x a [tokens], or [dfm] object from which to generate the feature
#'   co-occurrence matrix
#' @param context the context in which to consider term co-occurrence:
#'   `"document"` for co-occurrence counts within document; `"window"`
#'   for co-occurrence within a defined window of words, which requires a
#'   positive integer value for `window`.  Note: if `x` is a dfm
#'   object, then `context` can only be `"document"`.
#' @param window positive integer value for the size of a window on either side
#'   of the target feature, default is 5, meaning 5 words before and after the
#'   target feature
#' @param count how to count co-occurrences:
#'   \describe{
#'   \item{`"frequency"`}{count the number of co-occurrences within the
#'   context}
#'   \item{`"boolean"`}{count only the co-occurrence or not within the
#'   context, irrespective of how many times it occurs.}
#'   \item{`"weighted"`}{count a weighted function of counts, typically as
#'   a function of distance from the target feature.  Only makes sense for
#'   `context = "window"`.}
#'   }
#' @param weights a vector of weights applied to each distance from
#'   `1:window`, strictly decreasing by default; can be a custom-defined
#'   vector of the same length as `window`
#' @param ordered if `TRUE`, count only the forward co-occurrences for each
#'   target token for bigram models, so that the `i, j` cell of the fcm is the
#'   number of times that token `j` occurs before the target token `i` within
#'   the window. Only makes sense for `context = "window"`, and when `ordered =
#'   TRUE`, the argument `tri` has no effect.
#' @param tri if `TRUE` return only upper triangle (including diagonal).
#'   Ignored if `ordered = TRUE`.
#' @param ... not used here
#' @author Kenneth Benoit (R), Haiyan Wang (R, C++), Kohei Watanabe (C++)
#' @import Matrix
#' @export
#' @aliases is.fcm
#' @details The function [fcm()] provides a very general
#'   implementation of a "context-feature" matrix, consisting of a count of
#'   feature co-occurrence within a defined context.  This context, following
#'   Momtazi et. al. (2010), can be defined as the *document*,
#'   *sentences* within documents, *syntactic relationships* between
#'   features (nouns within a sentence, for instance), or according to a
#'   *window*.  When the context is a window, a weighting function is
#'   typically applied that is a function of distance from the target word (see
#'   Jurafsky and Martin 2015, Ch. 16) and ordered co-occurrence of the two
#'   features is considered (see Church & Hanks 1990).
#'
#'   [fcm] provides all of this functionality, returning a \eqn{V * V}
#'   matrix (where \eqn{V} is the vocabulary size, returned by
#'   [nfeat()]). The `tri = TRUE` option will only return the
#'   upper part of the matrix.
#'
#'   Unlike some implementations of co-occurrences, [fcm] counts feature
#'   co-occurrences with themselves, meaning that the diagonal will not be zero.
#'
#'   [fcm] also provides "boolean" counting within the context of "window",
#'   which differs from the counting within "document".
#'
#'   `is.fcm(x)` returns `TRUE` if and only if its x is an object of
#'   type [fcm].
#' @references
#'   Momtazi, S., Khudanpur, S., & Klakow, D. (2010). "A comparative study of
#'   word co-occurrence for term clustering in language model-based sentence
#'   retrieval. *Human Language Technologies: The 2010 Annual Conference of the
#'   North American Chapter of the ACL*, Los Angeles, California, June 2010,
#'   325-328.  https://aclanthology.org/N10-1046/
#'
#'   Jurafsky, D. & Martin, J.H. (2018). From *Speech and Language Processing:
#'   An Introduction to Natural Language Processing, Computational Linguistics,
#'   and Speech Recognition*. Draft of September 23, 2018 (Chapter 6, Vector
#'   Semantics). Available at <https://web.stanford.edu/~jurafsky/slp3/>.
#'
#'  Church, K. W. & P. Hanks (1990). [Word association norms, mutual
#'  information, and lexicography](https://dl.acm.org/doi/10.5555/89086.89095).
#'  *Computational Linguistics*, 16(1), 22-29.
#' @examples
#' # see http://bit.ly/29b2zOA
#' toks1 <- tokens(c("A D A C E A D F E B A C E D"))
#' fcm(toks1, context = "window", window = 2)
#' fcm(toks1, context = "window", count = "weighted", window = 3)
#' fcm(toks1, context = "window", count = "weighted", window = 3,
#'     weights = c(3, 2, 1), ordered = TRUE, tri = FALSE)
#'
#' # with multiple documents
#' toks2 <- tokens(c("a a a b b c", "a a c e", "a c e f g"))
#' fcm(toks2, context = "document", count = "frequency")
#' fcm(toks2, context = "document", count = "boolean")
#' fcm(toks2, context = "window", window = 2)
#'
#' txt3 <- c("The quick brown fox jumped over the lazy dog.",
#'          "The dog jumped and ate the fox.")
#' toks3 <- tokens(char_tolower(txt3), remove_punct = TRUE)
#' fcm(toks3, context = "document")
#' fcm(toks3, context = "window", window = 3)
fcm <- function(x, context = c("document", "window"),
                count = c("frequency", "boolean", "weighted"),
                window = 5L,
                weights = NULL,
                ordered = FALSE,
                tri = TRUE, ...) {
    check_dots(...)
    UseMethod("fcm")
}

#' @export
fcm.default <- function(x, ...) {
    check_class(class(x), "fcm")
}

#' @noRd
#' @export
fcm.character <- function(x, ...) {
    lifecycle::deprecate_stop(
        when = "3.0", 
        what = "fcm.character()",
        details = 'Please apply `tokens()` to the object first.'
    )
}

#' @noRd
#' @export
fcm.corpus <- function(x, ...) {
    lifecycle::deprecate_stop(
        when = "3.0", 
        what = "fcm.corpus()",
        details = 'Please apply `tokens()` to the object first.'
    )
}

#' @noRd
#' @export
fcm.dfm <- function(x, context = c("document", "window"),
                       count = c("frequency", "boolean", "weighted"),
                       window = 5L,
                       weights = NULL,
                       ordered = FALSE,
                       tri = TRUE, ...) {

    x <- as.dfm(x)
    context <- match.arg(context)
    count <- match.arg(count)
    window <- check_integer(window, min = 1)
    ordered <- check_logical(ordered)
    tri <- check_logical(tri)

    attrs <- attributes(x)
    if (!nfeat(x)) {
        result <- build_fcm(
            make_null_dfm(),
            featnames(x),
            count = count, context = context, margin = featfreq(x),
            weights = 1, tri = tri,
            meta = attrs[["meta"]])

        return(result)
    }

    if (context != "document")
        stop("fcm.dfm only works on context = \"document\"")

    if (count == "weighted")
        stop("Cannot have weighted counts with context = \"document\"")
    if (count == "boolean") {
        m <- Matrix::colSums(x > 1)
        x <- dfm_weight(x, "boolean")
    } else {
        m <- Matrix::colSums(x)
    }
    temp <- Matrix::crossprod(x)

    # correct self-co-occurrence
    if (count == "boolean") {
        Matrix::diag(temp) <- m
    } else {
        Matrix::diag(temp) <- (Matrix::diag(temp) - m) / 2
    }
    if (tri)
        temp <- Matrix::triu(temp)

    result <- build_fcm(
        temp,
        featnames(x),
        count = count, context = context, margin = featfreq(x),
        weights = 1, tri = tri,
        meta = attrs[["meta"]])

    return(result)
}


#' @noRd
#' @import Matrix
#' @export
fcm.tokens_xptr <- function(x, context = c("document", "window"),
                       count = c("frequency", "boolean", "weighted"),
                       window = 5L,
                       weights = NULL,
                       ordered = FALSE,
                       tri = TRUE, ...) {

    context <- match.arg(context)
    count <- match.arg(count)
    window <- check_integer(window, min = 1)
    ordered <- check_logical(ordered)
    tri <- check_logical(tri)

    attrs <- attributes(x)
    if (ordered)
        tri <- FALSE
    if (context == "document") {
        result <- fcm(dfm(x, tolower = FALSE, verbose = FALSE), count = count, tri = tri)
    } else {
        if (count == "weighted") {
            if (!is.null(weights)) {
                weights <- check_double(weights, max_len = Inf)
                if (length(weights) != window)
                    stop("The length of weights must be equal to the window size")
            } else {
                weights <- 1 / seq_len(window)
            }
        } else {
            weights <- rep(1, window)
        }
        type <- get_types(x)
        boolean <- count == "boolean"
        temp <- cpp_fcm(x, length(type), weights, boolean, ordered,
                        get_threads())
        temp <- as(temp, "CsparseMatrix")
        if (!ordered) {
            if (tri) {
                temp <- Matrix::triu(temp)
            } else {
                temp <- Matrix::forceSymmetric(temp)
            }
        }
        result <- build_fcm(
            temp,
            type,
            count = count, context = context, margin = featfreq(dfm(x, tolower = FALSE)),
            weights = weights, ordered = ordered, tri = tri,
            meta = attrs[["meta"]])
    }
    return(result)
}

#' @export
fcm.tokens <- function(x, ...) {
    fcm(as.tokens_xptr(x), ...)
}

#' @noRd
#' @rdname as.fcm
#' @export
is.fcm <- function(x) {
    is(x, "fcm") && isS4(x)
}
