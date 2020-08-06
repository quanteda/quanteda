#' Summarize a corpus
#'
#' Displays information about a corpus, including attributes and metadata such
#' as date of number of texts, creation and source.
#'
#' @param object corpus to be summarized
#' @param n maximum number of texts to describe, default=100
#' @param showmeta set to `TRUE` to include document-level
#'   meta-data
#' @param tolower convert texts to lower case before counting types
#' @param ... additional arguments passed through to [tokens()]
#' @export
#' @method summary corpus
#' @keywords internal corpus
#' @examples
#' summary(data_corpus_inaugural)
#' summary(data_corpus_inaugural, n = 10)
#' corp <- corpus(data_char_ukimmig2010,
#'                docvars = data.frame(party=names(data_char_ukimmig2010)))
#' summary(corp, showmeta = TRUE) # show the meta-data
#' sumcorp <- summary(corp) # (quietly) assign the results
#' sumcorp$Types / sumcorp$Tokens # crude type-token ratio
summary.corpus <- function(object, n = 100, tolower = FALSE, showmeta = TRUE, ...) {
    object <- as.corpus(object)
    ndoc_all <- ndoc(object)
    object <- head(object, n)
    ndoc_show <- ndoc(object)
    result <- textstat_summary(object, cache = FALSE, tolower = tolower, ...)
    result <- result[,c("document", "types", "tokens", "sents")]
    names(result) <- c("Text", "Types", "Tokens", "Sentences")
    if (showmeta)
        result <- cbind(result, docvars(object))
    attr(result, "ndoc_all") <- ndoc_all
    attr(result, "ndoc_show") <- ndoc_show
    class(result) <- c("summary.corpus", "data.frame")
    return(result)
}

#' @export
#' @rdname corpus-class
#' @method print summary.corpus
print.summary.corpus <- function(x, ...) {
    
    ndoc_all <- attr(x, "ndoc_all")
    ndoc_show <- attr(x, "ndoc_show")
    
    cat("Corpus consisting of ", ndoc_all, " document", if (ndoc_all > 1) "s" else "", sep = "")
    if (!is.null(ndoc_show))
        cat(", showing ", ndoc_show, " document", if (ndoc_show > 1) "s" else "", sep = "")
    cat(":\n\n")
    print.data.frame(x, row.names = FALSE)
    cat("\n")
}

#' @noRd
#' @export
#' @method [ summary.corpus
`[.summary.corpus` <- function(x, i, j, ...) {
    class(x) <- "data.frame"
    row.names(x) <- NULL
    NextMethod("[")
}