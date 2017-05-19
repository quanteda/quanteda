###
### Methods to extend the "spacyr" package, by defining methods for 
### spacy_parsed objects
###

#' extensions of methods defind in the quanteda package
#' 
#' Extensions to quanteda functions.  You must have attached \pkg{quanteda} for these
#' to work.
#' @name spacyr-methods
#' @section Usage:
#' \code{docnames(x)} returns the document names
#' 
#' \code{ndoc(x)} returns the number of documents
#' 
#' \code{ntoken(x, ...)} returns the number of tokens by document
#' 
#' \code{ntype(x, ...)} returns the number of types (unique tokens) by document
#' 
#' @param x the \code{spacyr_parsed} object returned by \code{spacy_parse}
#' @param ... unused
#' @examples 
#' \dontrun{
#' require(spacyr)
#' spacy_initialize()
#' txt <- c(doc1 = "And now, now, now for something completely different.",
#'          doc2 = "Jack and Jill are children.")
#' parsed <- spacy_parse(txt)
#' ntype(parsed)
#' ntoken(parsed)
#' ndoc(parsed)
#' docnames(parsed)
#' }
NULL

#' @rdname spacyr-methods
#' @details
#' \code{docnames} returns the document names
#' 
#' @noRd
#' @export
docnames.spacyr_parsed <- function(x) {
    unique(x$doc_id)
}


#' @rdname spacyr-methods
#' @details
#' \code{ndoc} returns the number of documents
#' 
#' @noRd
#' @export
ndoc.spacyr_parsed <- function(x) {
    length(docnames(x))
}

#' @rdname spacyr-methods
#' @details
#' \code{ntoken} returns the number of tokens by document
#' 
#' @noRd
#' @export
ntoken.spacyr_parsed <- function(x, ...) {
    lengths(split(x$token, x$doc_id))
}

#' @rdname spacyr-methods
#' @details
#' \code{ntype} returns the number of types (unique tokens) by document
#' 
#' @noRd
#' @export
ntype.spacyr_parsed <- function(x, ...) {
    sapply(split(x$token, x$doc_id), function(y) length(unique(y)))
}

#' #' @noRd
#' #' @export
#' spacy_parse.corpus <- function(x, ...) {
#'     spacy_parse(texts(x), ...)
#' }


# print a tokenizedTexts objects
# 
# print method for a \link{tokenize}dText object
# param x a tokenizedText_tagged object created by \link{tokens_tags_out}
# @param sep separator for printing tokens and tags, default is \code{"_"}.  If
#   \code{NULL}, print tokens and tags separately.
# @param ... further arguments passed to base print method
# @export
# @keywords internal
# @method print tokenizedTexts_tagged
# print.tokenizedTexts_tagged <- function(x, sep = "_", ...) {
#     ndocuments <- ifelse(is.list(x), length(x), 1)
#     if( "token" %in% class(x)) {
#         x <- as.tokenizedTexts(x)
#         class(x) <- c("tokenizedTexts_tagged", class(x))   
#     }
#     cat("tokenizedText_tagged object from ", ndocuments, " document", 
#         ifelse(ndocuments > 1, "s", ""), 
#         " (tagset = ", attr(x, "tagset"), ").\n", 
#         sep = "")
#     
#     if (!is.null(sep)) {
#         
#         docs <- factor(rep(docnames(x), times = ntoken(x)), levels = docnames(x))
#         tmp <- split(paste(unlist(x), unlist(attr(x, "tags")), sep = sep),  docs)
#         class(tmp) <- "listof"
#         print(tmp)
#         
#     } else {
#         
#         for (e in docnames(x)) {
#             cat(paste0(e, ":\n"))
#             if (is.list(x[[tolower(e)]])) { 
#                 class(x[[tolower(e)]]) <- "listof"
#                 print(x[[tolower(e)]], ...)
#             } else {
#                 print(as.character(x[[tolower(e)]]), ...)
#             }
#         }
#         
#     }
# }


# summarize a tagged tokenizedTexts object
# 
# Generate frequency counts of POS by document, returning a data.frame.
# @param object tokenizedTexts_tagged object to be summarized
# @param ... unused
# @importFrom data.table rbindlist
# @export
# @method summary spacyr_parsed
# summary.spacyr_parsed <- function(object, ...) {
#     object <- data.table(object)
#     result <- data.frame(
#         data.table::rbindlist(lapply(attr(object, "tags"), function(doc) as.list(table(doc))), 
#                               use.names = TRUE, fill = TRUE)
#     )
#     result[is.na(result)] <- 0
#     row.names(result) <- docnames(object)
#     
#     result
# }

