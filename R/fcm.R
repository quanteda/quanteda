
#' Create a feature co-occurrence matrix
#' 
#' Create a sparse feature co-occurrence matrix, measuring co-occurrences of
#' features within a user-defined context. The context can be defined as a
#' document or a window within a collection of documents, with an optional
#' vector of weights applied to the co-occurrence counts.
#' @param x character, \link{corpus}, \link{tokens}, or \link{dfm} object from
#'   which to generate the feature co-occurrence matrix
#' @param context the context in which to consider term co-occurrence:
#'   \code{"document"} for co-occurrence counts within document; \code{"window"}
#'   for co-occurrence within a defined window of words, which requires a
#'   positive integer value for \code{window}.  Note: if \code{x} is a dfm
#'   object, then \code{context} can only be \code{"document"}.
#' @param window positive integer value for the size of a window on either side
#'   of the target feature, default is 5, meaning 5 words before and after the
#'   target feature
#' @param count how to count co-occurrences:
#'   \describe{
#'   \item{\code{"frequency"}}{count the number of co-occurrences within the
#'   context}
#'   \item{\code{"boolean"}}{count only the co-occurrence or not within the
#'   context, irrespective of how many times it occurs.}
#'   \item{\code{"weighted"}}{count a weighted function of counts, typically as
#'   a function of distance from the target feature.  Only makes sense for
#'   \code{context = "window"}.}
#'   }
#' @param weights a vector of weights applied to each distance from
#'   \code{1:window}, strictly decreasing by default; can be a custom-defined
#'   vector of the same length as \code{length(weights)}
#' @param ordered if \code{TRUE} the number of times that a term appears before
#'   or after the target feature are counted separately. Only makes sense for
#'   context = "window".
#' @param span_sentence if \code{FALSE}, then word windows will not span
#'   sentences
#' @param tri if \code{TRUE} return only upper triangle (including diagonal).
#'   Ignored if \code{ordered = TRUE}
#' @param ... not used here
#' @author Kenneth Benoit (R), Haiyan Wang (R, C++), Kohei Watanabe (C++)
#' @import Matrix
#' @export
#' @aliases is.fcm
#' @details The function \code{\link{fcm}} provides a very general
#'   implementation of a "context-feature" matrix, consisting of a count of
#'   feature co-occurrence within a defined context.  This context, following
#'   Momtazi et. al. (2010), can be defined as the \emph{document},
#'   \emph{sentences} within documents, \emph{syntactic relationships} between
#'   features (nouns within a sentence, for instance), or according to a
#'   \emph{window}.  When the context is a window, a weighting function is
#'   typically applied that is a function of distance from the target word (see
#'   Jurafsky and Martin 2015, Ch. 16) and ordered co-occurrence of the two
#'   features is considered (see Church & Hanks 1990).
#'   
#'   \link{fcm} provides all of this functionality, returning a \eqn{V * V}
#'   matrix (where \eqn{V} is the vocabulary size, returned by
#'   \code{\link{nfeat}}). The \code{tri = TRUE} option will only return the
#'   upper part of the matrix.
#'   
#'   Unlike some implementations of co-occurrences, \link{fcm} counts feature
#'   co-occurrences with themselves, meaning that the diagonal will not be zero.
#'   
#'   \link{fcm} also provides "boolean" counting within the context of "window",
#'   which differs from the counting within "document".
#'   
#'   \code{is.fcm(x)} returns \code{TRUE} if and only if its x is an object of 
#'   type \link{fcm}.
#'   
#' @references 
#'   Momtazi, S., Khudanpur, S., & Klakow, D. (2010). 
#'   "\href{https://www.lsv.uni-saarland.de/fileadmin/publications/SaeedehMomtazi-HLT_NAACL10.pdf}{A
#'    comparative study of word co-occurrence for term clustering in language 
#'   model-based sentence retrieval.}" \emph{Human Language Technologies: The 
#'   2010 Annual Conference of the North American Chapter of the ACL}, Los 
#'   Angeles, California, June 2010, 325-328.
#'   
#'   Jurafsky, D. & Martin, J.H. (2018).
#'   From \emph{Speech and Language Processing: An Introduction to Natural Language Processing, 
#'   Computational Linguistics, and Speech Recognition}. Draft of September 23, 2018 
#'   (Chapter 6, Vector Semantics). Available at \url{https://web.stanford.edu/~jurafsky/slp3/}.
#'   
#'   Church, K. W. & P. Hanks (1990) .
#'  \href{http://dl.acm.org/citation.cfm?id=89095}{Word association norms,
#'   mutual information, and lexicography}. \emph{Computational Linguistics},
#'   16(1), 22–29.
#' @examples
#' # see http://bit.ly/29b2zOA
#' txt1 <- "A D A C E A D F E B A C E D"
#' fcm(txt1, context = "window", window = 2)
#' fcm(txt1, context = "window", count = "weighted", window = 3)
#' fcm(txt1, context = "window", count = "weighted", window = 3, 
#'              weights = c(3, 2, 1), ordered = TRUE, tri = FALSE)
#' 
#' # with multiple documents
#' txt2 <- c("a a a b b c", "a a c e", "a c e f g")
#' fcm(txt2, context = "document", count = "frequency")
#' fcm(txt2, context = "document", count = "boolean")
#' fcm(txt2, context = "window", window = 2)
#' 
#' 
#' # from tokens
#' txt3 <- c("The quick brown fox jumped over the lazy dog.",
#'          "The dog jumped and ate the fox.")
#' toks <- tokens(char_tolower(txt3), remove_punct = TRUE)
#' fcm(toks, context = "document")
#' fcm(toks, context = "window", window = 3)
fcm <- function(x, context = c("document", "window"), 
                count = c("frequency", "boolean", "weighted"),
                window = 5L,
                weights = 1L,
                ordered = FALSE,
                span_sentence = TRUE, tri = TRUE, ...) {
    UseMethod("fcm")
}

#' @export
fcm.default <- function(x, ...) {
    stop(friendly_class_undefined_message(class(x), "fcm"))
}

#' @noRd
#' @export
fcm.character <- function(x, ...) {
    fcm(tokens(x), ...)
}

#' @noRd
#' @export
fcm.corpus <- function(x, ...) {
    fcm(tokens(x), ...)
}

#' @noRd
#' @import Matrix
#' @export
fcm.dfm <- function(x, context = c("document", "window"), 
                       count = c("frequency", "boolean", "weighted"),
                       window = 5L,
                       weights = 1L,
                       ordered = FALSE,
                       span_sentence = TRUE, tri = TRUE, ...) {
    
    context <- match.arg(context)
    count <- match.arg(count)
    window <- as.integer(window)
    x <- as.dfm(x)
    margin <- colSums(x)
    
    if (!nfeat(x)) {
        result <- new("fcm", as(make_null_dfm(), "dgCMatrix"), count = count,
                      context = context, window = window, margin = numeric(),
                      weights = weights, tri = tri)
        return(result)
    }
    
    if (!span_sentence) 
        warning("spanSentence = FALSE not yet implemented")
    if (context != "document") 
        stop("fcm.dfm only works on context = \"document\"")

    if (count == "boolean") {
        temp <- x > 1
        x <- dfm_weight(x, "boolean") 
    } else if (count == "frequency") {
        temp <- x
        temp@x <- choose(temp@x, 2)
    } else {
        stop("Cannot have weighted counts with context = \"document\"")
    }

    # compute co_occurrence of the diagonal elements
    sum_col <- colSums(temp) # apply(temp, MARGIN = 2, sum)
    feature <- sum_col >= 1
    index_diag <- which(feature)
    length_feature <- length(feature)
    temp2 <- Matrix::sparseMatrix(i = index_diag,
                                  j = index_diag,
                                  x = sum_col[feature],
                                  dims = c(length_feature , length_feature))
    
    result <- Matrix::crossprod(x)
    diag(result) <- 0
    result <- result + temp2
    result <- result[rownames(result), colnames(result)]
    
    # discard the lower diagonal if tri == TRUE
    if (tri) result <- Matrix::triu(result)

    # create a new feature context matrix
    result <- new("fcm", as(result, "dgCMatrix"), count = count,
                  context = context, window = window, margin = margin,
                  weights = weights, tri = tri)
    set_fcm_dimnames(result) <- list(rownames(result), colnames(result))
    return(result)
}

    
#' @noRd
#' @import data.table
#' @import Matrix
#' @export
fcm.tokens <- function(x, context = c("document", "window"), 
                       count = c("frequency", "boolean", "weighted"),
                       window = 5L,
                       weights = 1L,
                       ordered = FALSE,
                       span_sentence = TRUE, tri = TRUE, ...) {
    context <- match.arg(context)
    count <- match.arg(count)
    window <- as.integer(window)
    # TODO could add a warning if not roundly coerced to integer
    
    if (ordered) tri <- FALSE
    if (!span_sentence) 
        warning("spanSentence = FALSE not yet implemented")
    
    if (context == "document")
        result <- fcm(dfm(x, tolower = FALSE, verbose = FALSE), count = count, tri = tri)
        
    if (context == "window") { 
        if (any(window < 1L)) stop("The window size is too small.")
        if (count == "weighted") {
            if (!missing(weights) && length(weights) != window) {
                warning ("weights length is not equal to the window size, weights are assigned by default!")
                weights <- 1
            }
        }
        if (!is.tokens(x)) x <- as.tokens(x)
        type <- types(x)
        n <- sum(lengths(x)) * window * 2
        result <- as(qatd_cpp_fcm(x, length(type), count, window, 
                                  weights, ordered, tri, n), "dgCMatrix")
        set_fcm_dimnames(result) <- list(type, type)
    }

    # discard the lower diagonal if tri == TRUE
    if (tri) result <- Matrix::triu(result)
    
    # create a new feature context matrix
    result <- new("fcm", as(result, "dgCMatrix"), count = count,
                  context = context, window = window, margin = colSums(dfm(x)),
                  weights = weights, tri = tri)
    return(result)
}     

#' @rdname print.dfm
#' @export
setMethod("print", signature(x = "fcm"), 
          function(x, show.values = NULL, show.settings = FALSE, 
                   show.summary = TRUE, 
                   ndoc = quanteda_options("print_dfm_max_ndoc"), 
                   nfeature = quanteda_options("print_dfm_max_nfeat"), ...) {
              if (show.summary) {
                  cat("Feature co-occurrence matrix of: ",
                      format(ndoc(x), big.mark = ","), " by ",
                      format(nfeat(x), big.mark = ","), " feature",
                      if (nfeat(x) != 1L) "s" else "",
                      if (is.resampled(x)) paste(", ", nresample(x), " resamples", sep = "") else "",
                      ".\n", sep = "")
              }
              print_dfm(x, ndoc, nfeature, show.values, show.settings, ...)
          })

#' @rdname print.dfm
#' @export
setMethod("show", signature(object = "fcm"), function(object) print(object))

#' @rdname print.dfm
#' @method head fcm
#' @export
head.fcm <- function(x, n = 6L, nfeature = 6L, ...) {
    head.dfm(x, n, nfeature, ...)
}

#' @rdname print.dfm
#' @method tail fcm
#' @export
tail.fcm <- function(x, n = 6L, nfeature = 6L, ...) {
    head.dfm(x, n, nfeature, ...)
}

#' @noRd
#' @rdname fcm-class
#' @export
is.fcm <- function(x) {
    is(x, "fcm")
}
