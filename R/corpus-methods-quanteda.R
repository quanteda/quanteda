#' Get or assign corpus texts
#'
#' Get or replace the texts in a [corpus], with grouping options.
#' Works for plain character vectors too, if `groups` is a factor.
#' @note The `groups` will be used for concatenating the texts based on shared
#' values of `groups`, without any specified order of aggregation.
#' @param x a [corpus] or character object
#' @inheritParams groups
#' @param spacer when concatenating texts by using `groups`, this will be the
#'   spacing added between texts.  (Default is two spaces.)
#' @return For `texts`, a character vector of the texts in the corpus.
#'
#'   For `texts <-`, the corpus with the updated texts.
#' @export
#' @keywords corpus
#' @examples
#' nchar(texts(corpus_subset(data_corpus_inaugural, Year < 1806)))
#'
#' # grouping on a document variable
#' nchar(texts(corpus_subset(data_corpus_inaugural, Year < 1806), groups = "President"))
#'
#' # grouping a character vector using a factor
#' nchar(texts(data_corpus_inaugural[1:5],
#'       groups = "President"))
#' nchar(texts(data_corpus_inaugural[1:5],
#'       groups = factor(c("W", "W", "A", "J", "J"))))
#'
texts <- function(x, groups = NULL, spacer = " ") {
    UseMethod("texts")
}

#' @noRd
#' @importFrom stringi stri_c_list
#' @export
texts.corpus <- function(x, groups = NULL, spacer = " ") {
    x <- as.corpus(x)
    attrs <- attributes(x)
    if (!is.null(groups)) {
        if (!is.factor(groups))
            groups <- generate_groups(x, groups)
        result <- stri_c_list(split(x, groups), sep = spacer)
        attrs$docvars <- group_docvars(attrs$docvars, groups)
    } else {
        result <- as.character(unclass(x))
    }
    names(result) <- attrs$docvars[["docname_"]]
    return(result)
}

#' @noRd
#' @importFrom stringi stri_c_list
#' @export
texts.character <- function(x, groups = NULL, spacer = " ") {
    texts(corpus(x), groups = groups, spacer = spacer)
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
#' @examples
#' corp <- corpus(c("We must prioritise honour in our neighbourhood.",
#'                  "Aluminium is a valourous metal."))
#' texts(corp) <-
#'     stringi::stri_replace_all_regex(texts(corp),
#'                                    c("ise", "([nlb])our", "nium"),
#'                                    c("ize", "$1or", "num"),
#'                                    vectorize_all = FALSE)
#' texts(corp)
#' texts(corp)[2] <- "New text number 2."
#' texts(corp)
"texts<-" <- function(x, value) {
    UseMethod("texts<-")
}

#' @noRd
#' @export
"texts<-.corpus" <- function(x, value) {
    x <- as.corpus(x)
    attrs <- attributes(x)
    x <- value
    attributes(x) <- attrs
    return(x)
}

#' @rdname texts
#' @details `as.character(x)` where `x` is a corpus is equivalent to
#' calling `texts(x)`
#' @param ... unused
#' @method as.character corpus
#' @return `as.character(x)` is equivalent to `texts(x)`
#' @export
as.character.corpus <- function(x, ...) {
    x <- as.corpus(x)
    texts(x)
}

#' coerce a compressed corpus to a standard corpus
#'
#' Recast a compressed corpus object into a standard (uncompressed) corpus
#' object.
#' @param x a compressed [corpus] object
#' @export
#' @keywords internal
as.corpus <- function(x) {
    UseMethod("as.corpus")
}

#' @export
as.corpus.default <- function(x) {
    stop(friendly_class_undefined_message(class(x), "as.corpus"))
}

#' @export
#' @method as.corpus corpus
as.corpus.corpus <- function(x) {
    upgrade_corpus(x)
}

#' @export
#' @method as.corpus corpuszip
as.corpus.corpuszip <- function(x) {
    x <- unclass(x)
    txt <- memDecompress(x$texts, "gzip", asChar = TRUE)
    txt <- strsplit(txt, paste0("###END_DOCUMENT###", "\n"))
    txt <- unlist(txt, use.names = FALSE)

    # drop internal variables
    flag <- is_system(names(x$documents))
    corpus(txt, x$docnames, docvars = x$documents[!flag])
}
