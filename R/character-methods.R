#' Summary statistics on a character vector
#' 
#' Internal-only function to compute summary statistics on a character object.
#' @inheritParams summary.corpus
#' @keywords char internal 
#' @examples
#' # summarize texts
#' quanteda:::summarize_texts(c("Testing this text. Second sentence.", "And this one."))
#' quanteda:::summarize_texts(data_char_ukimmig2010)
#' summary_ukimmig2010 <- quanteda:::summarize_texts(data_char_ukimmig2010)
#' head(summary_ukimmig2010)
summarize_texts <- function(object, tolower = FALSE, ...) {

    dict <- dictionary(list(
        "number" = "\\p{N}",
        "punct" = "\\p{P}",
        "symbol" = "\\p{S}",
        "any" = "[\\p{N}\\p{P}\\p{S}]"
    ))
    is_dup <- duplicated(texts(object))
    n_sent <- ntoken(tokens(object, what = "sentence"))
    temp <- dfm(tokens(object, ...))
    if (tolower) 
        temp <- dfm_tolower(temp)
    result <- convert(
        dfm_lookup(temp, dictionary = dict, valuetype = "regex"),
        "data.frame"
    )
    result$n_sent <- n_sent
    result$n_token <- ntoken(temp)
    result$n_type <- nfeat(temp) 
    result$dupli <- is_dup
    result$noise <- result$any / result$n_token
    return(result)
}


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
summary.corpus <- function(object, cache = TRUE, ...) {
    #parent <- deparse(substitute(object))
    object <- as.corpus(object)
    meta <- meta(object, "all")
    if (cache) {
        hash <- digest::digest(list(object, utils::packageVersion("quanteda"), ...),
                               algo = "sha256")
        if (identical(meta$object$summary$hash, hash)) {
            cat("Use summary cache\n")
            result <- meta$object$summary$data
        } else {
            cat("Summarize and cache\n")
            result <- summarize_texts(texts(object), ...)
            meta$object$summary <- list("hash" = hash, "data" = result)
        }
    } else {
        cat("Summarize but don't not cache\n")
        result <- summarize_texts(texts(object), ...)
        meta$object$summary <- NULL
    }
    qatd_cpp_set_meta(object, meta)
    #class(result) <- c("summary.corpus", "data.frame")
    return(result)
}

#' @method summary tokens
summary.tokens <- function(object, cache = TRUE, ...) {
    
}

#' @method summary dfm
summary.dfm <- function(object, cache = TRUE, ...) {
    
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


