#' Get or assign corpus texts \[deprecated\]
#'
#' Get or replace the texts in a [corpus], with grouping options.
#' Works for plain character vectors too, if `groups` is a factor.
#' 
#' @description This function is **deprecated** and will be removed in the next
#'   major release. 
#' * Use [as.character.corpus()] to turn a corpus into a simple named character
#'   vector.
#' * Use [corpus_group()] instead of `texts(x, groups = ...)` to aggregate texts
#'   by a grouping variable.
#' * Use \code{\link{[<-}} instead of `texts()<-` for replacing texts in a corpus object.
#
#' @note The `groups` will be used for concatenating the texts based on shared
#' values of `groups`, without any specified order of aggregation.
#' @param x a [corpus]
#' @inheritParams groups
#' @param spacer when concatenating texts by using `groups`, this will be the
#'   spacing added between texts.  (Default is two spaces.)
#' @return For `texts`, a character vector of the texts in the corpus.
#'
#'   For `texts <-`, the corpus with the updated texts.
#' @export
#' @keywords corpus internal
texts <- function(x, groups = NULL, spacer = " ") {
    UseMethod("texts")
}

#' @importFrom stringi stri_c_list
#' @export
texts.corpus <- function(x, groups = NULL, spacer = " ") {
    spacer <- check_character(spacer)

    if (!missing(groups)) {
        .Deprecated("corpus_group")
        grp <- eval(substitute(groups), get_docvars(x, user = TRUE, system = TRUE), parent.frame())
        x <- corpus_group(x, grp, concatenator = spacer)
    } else {
        .Deprecated("as.character")
    }
    
    as.character(x, use.names = TRUE)
}

#' @rdname texts
#' @param value character vector of the new texts
#' @return for `texts <-`, a corpus with the texts replaced by `value`
#' @export
#' @note You are strongly encouraged as a good practice of text analysis
#'   workflow *not* to modify the substance of the texts in a corpus.
#'   Rather, this sort of processing is better performed through downstream
#'   operations.  For instance, do not lowercase the texts in a corpus, or you
#'   will never be able to recover the original case.  Rather, apply
#'   [tokens_tolower()] after applying [tokens()] to a
#'   corpus, or use the option `tolower = TRUE` in [dfm()].
"texts<-" <- function(x, value) {
    UseMethod("texts<-")
}

#' @export
"texts<-.corpus" <- function(x, value) {
    .Deprecated(msg = "use the '[<-' replacement for a corpus object instead")
    x <- as.corpus(x)
    attrs <- attributes(x)
    value <- as.character(value)
    if (length(x) != length(value))
        stop(message_error("ndoc_mismatch"), call. = FALSE)
    x <- value
    attributes(x) <- attrs
    return(x)
}
