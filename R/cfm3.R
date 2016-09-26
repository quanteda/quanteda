#' @include dfm-classes.R

#' @title Virtual class "cfm3" for a context-feature matrix
#' 
#' @description The cfm3 class of object is a special type of \link{dfm-class}.
#'   
#' @slot context the context definition
#' @slot window the size of the window, if \code{context = "window"}
#' @slot count how co-occurrences are counted
#' @slot weights context weighting for distance from target feature, equal in length to \code{window}
#' @slot tri whether the lower triangle of the symmetric \eqn{V \times V} matrix is recorded
#' @seealso \link{cfm3}
#' @export
#' @import methods
#' @docType class
#' @name cfm3-class
setClass("cfm3",
         slots = c(context = "character", window = "integer", count = "character", weights = "numeric", ordered = "logical", tri = "logical"),
         # prototype = list(Dimnames = list(contexts = NULL, features = NULL)),
         contains = c("dfm", "dgCMatrix"))



#' create a context-feature co-occurrence matrix
#' 
#' Create a sparse context-feature co-occurrence matrix, measuring 
#' co-occurrences of features within a user-defined context. The context can be 
#' defined as a document or a window within a collection of documents, with an 
#' optional vector of weights applied to the co-occurrence counts.
#' @param x character vector, corpus, or tokenized texts from which to generate 
#'   the context-feature co-occurrence matrix.
#' @author Kenneth Benoit (R), Kohei Watanabe (C++) and Haiyan Wang
#' @import Matrix
#' @export
#' @details The funciton \link{cfm3} provides a very general implementation of a 
#'   "context-feature" matrix, consisting of a count of feature co-occurrence 
#'   within a defined context.  This context, following Momtazi et. al. (2010), 
#'   can be defined as the \emph{document}, \emph{sentences} within documents, 
#'   \emph{syntactic relationships} beteeen features (nouns within a sentence,
#'   for instance), or according to a \emph{window}.  When the context is a window, 
#'   a weighting function is typically applied that is a function of distance from the 
#'   target word (see Jurafsky and Martin 2015, Ch. 16) and ordered co-occurrence of 
#'   the two terms is counted (see Chruch, K.W. & Hanks, P. (1990)).  
#'   
#'   \link{cfm3} provides all of this functionality, returning a \eqn{V * V} matrix
#'   (where \eqn{V} is the vocabulary size, returned by \code{\link{ntype}}). The \code{tri = TRUE} option will only return the upper
#'   part of the matrix.
#'   
#'   Unlike some implementations of co-occurrences, \link{cfm3} counts feature co-occurrences 
#'   with themselves, meaning that the diagonal will not be zero.
#' @references Momtazi, S., Khudanpur, S., & Klakow, D. (2010). 
#'   "\href{https://www.lsv.uni-saarland.de/fileadmin/publications/SaeedehMomtazi-HLT_NAACL10.pdf}{A
#'    comparative study of word co-occurrence for term clustering in language 
#'   model-based sentence retrieval.}" \emph{Human Language Technologies: The 
#'   2010 Annual Conference of the North American Chapter of the ACL}, Los 
#'   Angeles, California, June 2010, pp. 325–328.
#'   
#'   Daniel Jurafsky & James H. Martin. (2015) \emph{Speech and Language 
#'   Processing}.  Draft of April 11, 2016. 
#'   \href{https://web.stanford.edu/~jurafsky/slp3/16.pdf}{Chapter 16, Semantics
#'   with Dense Vectors.}
#'   
#'   Chruch, K.W. & Hanks, P. (1990)  "\href{http://dl.acm.org/citation.cfm3?id=89095}{Word 
#'   association norms, mutual information, and lexicography}"\emph {Computational Linguistics, 16(1):22–29.

cfm3 <- function(x, ...) {
    UseMethod("cfm3")
}

#' @rdname cfm3
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
#'    irrespective of how many times it occurs}
#'   \item{\code{weighted}}{count a weighted function of counts, typically as a 
#'   function of distance from the target feature.  Only makes sense for \code{context = "window"}.}
#'   }
#' @param weights a vector of weights applied to each distance from 
#'   \code{1:window}, strictly decreasing by default; can be a customer defined vector of the same length as 
#'   \code{length(weights)}
#' @param ordered if \code{TRUE} a term before or after the target feature is counted seperately. 
#'      Only makes sense for context = "window".
#' @param span_sentence if \code{FALSE}, then word windows will not span 
#'   sentences
#' @param tri if \code{TRUE} return only upper triangle (including diagonal)
#' @param ... not used here
#' @examples
#' # see http://bit.ly/29b2zOA
#' txt <- "A D A C E A D F E B A C E D"
#' cfm3(txt, context = "window", window = 2)
#' cfm3(txt, context = "window", window = 2, ordered = TRUE, tri = FALSE)
#' cfm3(txt, context = "window", count = "weighted", window = 2, tri = FALSE)
#' cfm3(txt, context = "window", count = "weighted", window = 3, weights = c(3,2,1), tri = FALSE)
#' 
#' # with multiple documents
#' txts <- c("a a a b b c", "a a c e", "e f g")
#' cfm3(txts, context = "document", count = "frequency")
#' cfm3(txts, context = "document", count = "boolean")
#' 
#' txt <- c("The quick brown fox jumped over the lazy dog.",
#'          "The dog jumped and ate the fox.")
#' toks <- tokenize(toLower(txt), removePunct = TRUE)
#' cfm3(toks, context = "document")
#' cfm3(toks, context = "window", window = 3)
#' cfm3(toks, context = "window", window = 2)
#' @import data.table
#' @import Matrix
#' @export
cfm3.tokenizedTexts <- function(x, context = c("document", "window"), 
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
        tokenCount <- dfm(x, toLower = FALSE, verbose = FALSE)
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
        tokenCoSum <- apply(tokenCo, MARGIN = 2, sum)
        ft <- tokenCoSum >= 1
        diagIndex <- which(ft)
        lengthToken <- length(ft)
        diagCount <- Matrix::sparseMatrix(i = diagIndex,
                                          j = diagIndex,
                                          x = tokenCoSum[ft],
                                          dims = c(lengthToken , lengthToken))
        diag(result) <- 0
        result <- result + diagCount
    }
        
    if (context == "window") {    
        if (count == "weighted"){
            if (!missing(weights) & length(weights) != window){
                warning ("weights length is not equal to the window size, weights are assigned by default!")
                weights <- 1
            }
        }
        types <- unique(unlist(x, use.names = FALSE))
        n <- sum(lengths(x)) * window * 2
        y <- fcm_cpp3(x, types, count, window, weights, ordered, n)
        #y<-fcm_cpp()
        sizeM <- max(y$target, y$collocate)
        if (ordered){
            result <- Matrix::sparseMatrix(i = y$target, 
                                           j = y$collocate, 
                                           x = y$values,
                                           dims = c(sizeM, sizeM),
                                           dimnames = list(contexts = types, features = types),
                                           symmetric = FALSE)
        }else{
            result <- Matrix::sparseMatrix(i = y$target, 
                                           j = y$collocate, 
                                           x = y$values,
                                           dims = c(sizeM, sizeM),
                                           dimnames = list(contexts = types, features = types),
                                           symmetric = TRUE)
        }
        if (count == "boolean") result <- (result >= 1) * 1
    }

    # order the features alphabetically
    result <- result[order(rownames(result)), order(colnames(result))]

    # discard the lower diagonal if tri == TRUE
    if (tri)
        result[lower.tri(result, diag = FALSE)] <- 0

    # create a new feature context matrix
    result <- new("cfm3", as(result, "dgCMatrix"), count = count,
                  context = context, window = window, weights = weights, tri = tri)
    # set the names 
    names(result@Dimnames) <- c("contexts", "features")
    result
}     



#' @rdname cfm3
#' @export
cfm3.corpus <- function(x, ...) {
    cfm3(texts(x), ...)
}

#' @rdname cfm3
#' @export
cfm3.character <- function(x, ...) {
    cfm3(tokenize(x), ...)
}


#' @rdname print.dfm
#' @export
setMethod("print", signature(x = "cfm3"), 
          function(x, show.values = FALSE, show.settings = FALSE, show.summary = TRUE, nfeature = 20L, ...) {
              ndoc <- nfeature
              if (show.summary) {
                  cat("Context-feature matrix of: ",
                      format(ndoc(x), , big.mark = ","), " context",
                      ifelse(ndoc(x) > 1 | ndoc(x) == 0, "s, ", ", "),
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
setMethod("show", signature(object = "cfm3"), function(object) print(object))


# # C++ wrapper for cfm3()
# # Author: Kohei Watanabe
# cfm3Cpp_tokenizedTexts <- function(x, context = c("document", "window"), window = 5L, verbose = FALSE) {
#     context <- match.arg(context)
#     if (context == 'document') {
#         window <- max(lengths(x))
#     }
#     types <- unique(unlist(x, use.names=FALSE))
#     n <- sum(lengths(x)) * window * 2
#     y <- fcm_cpp(x, types, window, n)
#     
#     if (verbose) message("Making sparseMatrix\n")
#     mx <- Matrix::sparseMatrix(i = y$target, 
#                                j = y$collocate, 
#                                x = 1L,
#                                dimnames = list(context = types, features = types))
#     
#     return(mx)
# }
# 

 







      

