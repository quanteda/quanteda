#' @include dfm-classes.R

#' @title Virtual class "fcm" for a feature co-occurrence matrix
#' 
#' @description The fcm class of object is a special type of \link{dfm-class}
#'   object with additional slots, described below.  
#'     
#' @slot context the context definition
#' @slot window the size of the window, if \code{context = "window"}
#' @slot count how co-occurrences are counted
#' @slot weights context weighting for distance from target feature, equal in length to \code{window}
#' @slot tri whether the lower triangle of the symmetric \eqn{V \times V} matrix is recorded
#' @slot ordered whether a term appears before or after the target feature 
#'      are counted seperately
#' @seealso \link{fcm}
#' @export
#' @import methods
#' @docType class
#' @name fcm-class
#' @keywords internal
setClass("fcm",
         slots = c(context = "character", window = "integer", count = "character", weights = "numeric", ordered = "logical", tri = "logical"),
         # prototype = list(Dimnames = list(contexts = NULL, features = NULL)),
         contains = c("dfm", "dgCMatrix"))


#' create a feature co-occurrence matrix
#' 
#' Create a sparse feature co-occurrence matrix, measuring co-occurrences of
#' features within a user-defined context. The context can be defined as a
#' document or a window within a collection of documents, with an optional
#' vector of weights applied to the co-occurrence counts.
#' @param x character vector, corpus, or tokens object from which to generate 
#'   the feature co-occurrence matrix
#' @param context the context in which to consider term co-occurrence: 
#'   \code{"document"} for co-occurrence counts within document; \code{"window"}
#'   for co-occurrence within a defined window of words, which requires a 
#'   postive integer value for \code{window}
#' @param window positive integer value for the size of a window on either side 
#'   of the target feature, default is 5, meaning 5 words before and after the 
#'   target feature
#' @param count how to count co-occurrences:
#'   \describe{
#'   \item{\code{frequency}}{count the number of co-occurrences within the context}
#'   \item{\code{boolean}}{count only the co-occurrence or not within the context, 
#'    irrespective of how many times it occurs.}
#'   \item{\code{weighted}}{count a weighted function of counts, typically as a 
#'   function of distance from the target feature.  Only makes sense for \code{context = "window"}.}
#'   }
#' @param weights a vector of weights applied to each distance from 
#'   \code{1:window}, strictly decreasing by default; can be a customer defined vector of the same length as 
#'   \code{length(weights)}
#' @param ordered if \code{TRUE} the number of times that a term appears before or after the target feature 
#'      are counted seperately. Only makes sense for context = "window".
#' @param span_sentence if \code{FALSE}, then word windows will not span 
#'   sentences
#' @param tri if \code{TRUE} return only upper triangle (including diagonal)
#' @param ... not used here
#' @author Kenneth Benoit (R), Haiyan Wang (R, C++), Kohei Watanabe (C++)
#' @import Matrix
#' @export
#' @aliases is.fcm
#' @details The function \code{\link{fcm}} provides a very general
#'   implementation of a "context-feature" matrix, consisting of a count of
#'   feature co-occurrence within a defined context.  This context, following
#'   Momtazi et. al. (2010), can be defined as the \emph{document},
#'   \emph{sentences} within documents, \emph{syntactic relationships} beteeen
#'   features (nouns within a sentence, for instance), or according to a
#'   \emph{window}.  When the context is a window, a weighting function is
#'   typically applied that is a function of distance from the target word (see
#'   Jurafsky and Martin 2015, Ch. 16) and ordered co-occurrence of the two
#'   features is considered (see Chruch, K.W. & Hanks, P. (1990)).
#'   
#'   \link{fcm} provides all of this functionality, returning a \eqn{V * V}
#'   matrix (where \eqn{V} is the vocabulary size, returned by
#'   \code{\link{ntype}}). The \code{tri = TRUE} option will only return the
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
#'   Angeles, California, June 2010, pp. 325-328.
#'   
#'   Daniel Jurafsky & James H. Martin. (2015) \emph{Speech and Language 
#'   Processing}.  Draft of April 11, 2016. 
#'   \href{https://web.stanford.edu/~jurafsky/slp3/16.pdf}{Chapter 16, Semantics
#'   with Dense Vectors.}
#'   
#'   Chruch, K.W. & Hanks, P. (1990) 
#'   "\href{http://dl.acm.org/citation.fcm?id=89095}{Word association norms,
#'   mutual information, and lexicography}" \emph{Computational Linguistics},
#'   16(1):22–29.
#' @examples
#' # see http://bit.ly/29b2zOA
#' txt <- "A D A C E A D F E B A C E D"
#' fcm(txt, context = "window", window = 2)
#' fcm(txt, context = "window", count = "weighted", window = 3)
#' fcm(txt, context = "window", count = "weighted", window = 3, 
#'              weights = c(3,2,1), ordered = TRUE, tri = FALSE)
#' 
#' # with multiple documents
#' txts <- c("a a a b b c", "a a c e", "a c e f g")
#' fcm(txts, context = "document", count = "frequency")
#' fcm(txts, context = "document", count = "boolean")
#' fcm(txts, context = "window", count = "boolean", window = 2)
#' 
#' txt <- c("The quick brown fox jumped over the lazy dog.",
#'          "The dog jumped and ate the fox.")
#' toks <- tokenize(toLower(txt), removePunct = TRUE)
#' fcm(toks, context = "document")
#' 
#' # with hashed tokens
#' toksHashed <- tokens(toLower(txt), removePunct = TRUE)
#' fcm(toks, context = "window", window = 3)
#' fcm(toksHashed, context = "window", window = 3)
fcm <- function(x, context = c("document", "window"), 
                count = c("frequency", "boolean", "weighted"),
                window = 5L,
                weights = 1L,
                ordered = FALSE,
                span_sentence = TRUE, tri = TRUE, ...) {
    UseMethod("fcm")
}

#' @noRd
#' @export
fcm.character <- function(x, ...) {
    fcm(tokenize(x), ...)
}

#' @noRd
#' @import data.table
#' @import Matrix
#' @export
fcm.tokenizedTexts <- function(x, context = c("document", "window"), 
                       count = c("frequency", "boolean", "weighted"),
                       window = 5L,
                       weights = 1L,
                       ordered = FALSE,
                       span_sentence = TRUE, tri = TRUE, ...) {
    context <- match.arg(context)
    count <- match.arg(count)
    feature <- V1 <- NULL  # to avoid no visible binding errors in CHECK
    # could add a warning if not roundly coerced to integer
    window <- as.integer(window)
    
    if (!span_sentence) 
        warning("spanSentence = FALSE not yet implemented")
    
    if (context == "document") {
        tokenCount <- dfm(x, tolower = FALSE, verbose = FALSE)
        
        if (count == "boolean") {
            x <- tf(tokenCount, "boolean") 
            result <- Matrix::crossprod(x) 
            tokenCo <- tokenCount > 1
        } else if (count == "frequency") {
            result <- Matrix::crossprod(tokenCount)
            tokenCo <- apply(tokenCount, MARGIN=c(1,2), function(x) choose(x,2))
        } else {
            stop("Cannot have weighted counts with context = \"document\"")
        }
        
        # compute co_occurrence of the diagonal elements
        tokenCoSum <- colSums(tokenCo) # apply(tokenCo, MARGIN = 2, sum)
        ft <- tokenCoSum >= 1
        diagIndex <- which(ft)
        lengthToken <- length(ft)
        diagCount <- Matrix::sparseMatrix(i = diagIndex,
                                          j = diagIndex,
                                          x = tokenCoSum[ft],
                                          dims = c(lengthToken , lengthToken))
        diag(result) <- 0
        result <- result + diagCount
        result <- result[rownames(result), colnames(result)]
    }
        
    if (context == "window") { 
        try (if (window < 2) stop("The window size is too small.")) 
            
        if (count == "weighted") {
            if (!missing(weights) & length(weights) != window) {
                warning ("weights length is not equal to the window size, weights are assigned by default!")
                weights <- 1
            }
        }
        
        if (is.tokens(x)) {
            n <- sum(lengths(unlist(x))) * window * 2
            result <- fcm_hash_cpp(x, length(unique(unlist(x))), count, window, weights, ordered, tri, n)
            # set the dimnames of result
            types <- types(x)
        } else {
            types <- unique(unlist(x, use.names = FALSE))
            n <- sum(lengths(x)) * (window + 1)
            result <- fcm_cpp(x, types, count, window, weights, ordered, tri, n)
        }
        # set the dimnames of result
        dimnames(result) <- list(features = types, features = types)
    }

    # discard the lower diagonal if tri == TRUE
    if (tri) # & !is.tokens(x))
        result <- Matrix::triu(result)

    # create a new feature context matrix
    result <- new("fcm", as(result, "dgCMatrix"), count = count,
                  context = context, window = window, weights = weights, tri = tri)
    # set the names 
    names(result@Dimnames) <- c("features", "features")
    result
}     


#' @rdname print.dfm
#' @export
setMethod("print", signature(x = "fcm"), 
          function(x, show.values = FALSE, show.settings = FALSE, show.summary = TRUE, nfeature = 20L, ...) {
              ndoc <- nfeature
              if (show.summary) {
                  cat("Feature co-occurrence matrix of: ",
                      format(ndoc(x), , big.mark = ","), " by ",
                      # ifelse(ndoc(x) > 1 | ndoc(x) == 0, "s, ", ", "),
                      format(nfeature(x), big.mark = ","), " feature",
                      ifelse(nfeature(x) > 1 | nfeature(x) == 0, "s", ""),
                      ifelse(is.resampled(x), paste(", ", nresample(x), " resamples", sep = ""), ""),
                      ".\n", sep = "")
              }
              if (show.settings) {
                  cat("Settings: TO BE IMPLEMENTED.")
              }
              if (show.values | (nrow(x) <= ndoc & ncol(x) <= nfeature)) {
                  Matrix::printSpMatrix2(x[1:min(ndoc, ndoc(x)), 1:min(nfeature, nfeature(x))], 
                                         col.names = TRUE, 
                                         zero.print = ifelse(x@tri, ".", 0), ...)
              }
          })

#' @rdname print.dfm
#' @export
setMethod("show", signature(object = "fcm"), function(object) print(object))

#' @noRd
#' @rdname fcm-class
#' @export
is.fcm <- function(x) {
    is(x, "fcm")
}




