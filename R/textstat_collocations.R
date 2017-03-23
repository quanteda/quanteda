#' calculate collocation statistics
#' 
#' @rdname textstat_collocations
#' @param x a \link{dfm} containing the features to be examined for keyness
#' @param target the document index (numeric, character or logical) identifying the 
#'   document forming the "target" for computing keyness; all other documents' 
#'   feature frequencies will be combined for use as a reference
#' @param measure (signed) association measure to be used for computing keyness.
#'   Currenly available: \code{"chi2"} (\eqn{chi^2} with Yates correction); 
#'   \code{"exact"} (Fisher's exact test); \code{"lr"} for the likelihood ratio
#'   \eqn{G} statistic with Yates correction.
#' @export
#' @keywords textstat
#' @examples
#' txts <- c("This is software testing: looking for (word) pairs!  
#'          This [is] a software testing again. For.",
#'          "Here: this is more Software Testing, looking again for word pairs.")
#' toks <- tokens(txts)
#' cols <- textstat_collocations(toks, 'lr')
#' cols <- textstat_collocations(toks, 'lr', size = 3)
#' cols <- textstat_collocations(toks, 'lr', "*", min_count = 1)
#' cols <- textstat_collocations(toks, 'lr', "^([a-z]+)$", valuetype = 'regex', min_count = 1)
#' as.tokens(cols)
#' 
#' toks <- tokens(corpus_segment(data_corpus_inaugural, what = "sentence"))
#' toks <- tokens_select(toks, stopwords("english"), "remove", padding = TRUE)
#' 
#' # extracting multi-part proper nouns (capitalized terms)
#' seqs <- textstat_collocations(toks, "bj", "^([A-Z][a-z\\-]{2,})", valuetype="regex", case_insensitive = FALSE)
#' head(seqs, 10)
#' 
#' # more efficient when applied to the same tokens object 
#' toks_comp <- tokens_compound(toks, seqs)
#' 
#' # types can be any words
#' seqs2 <- textstat_collocations(toks, "bj", "^([a-z]+)$", valuetype="regex", case_insensitive = FALSE, 
#'                                min_count = 2, ordered = TRUE)
#' head(seqs2, 10)
#' 
textstat_collocations <- function(x, ...) {
    UseMethod("textstat_collocations")
}

#' @noRd
#' @export
textstat_collocations.tokens <- function(x, method = 'lr', ...) {
    
    if (method == 'bj') {
        result <- sequences.tokens(x, ...)
    } else {
        result <- collocations.tokens(x, method, ...)
    }
    class(result) <- c("collocations", 'data.frame')
    return(result)
}

#' @rdname textstat_collocations
#' @noRd
#' @export    
textstat_collocations.corpus <- function(x, method, ...) {
    textstat_collocations(texts(x), ...)
}

#' @rdname textstat_collocations
#' @noRd
#' @export    
textstat_collocations.character <- function(x, method, ...) {
    textstat_collocations(tokens(x), ...)
}

#' @rdname textstat_collocations
#' @noRd
#' @export    
textstat_collocations.tokenizedTexts <- function(x, ...) {
    textstat_collocations(as.tokens(x), ...)
}


#' @rdname is.collocations
#' @export
#' @return \code{is.collocation} returns \code{TRUE} if the object is of class
#'   collocation, \code{FALSE} otherwise.
is.collocations <- function(x) {
    ifelse("collocations" %in% class(x), TRUE, FALSE)
}

#' @method "[" collocations
#' @export
#' @noRd
"[.collocations" <- function(x, i, ...) {
    x <- as.data.frame(x)[i,]
    attr(x, 'ids') <- attr(x, 'ids')[i]
    class(x) <- c("collocations", 'data.frame')
    return(x)
}

#' @export
#' @method as.tokens collocations
#' @noRd 
as.tokens.collocations <- function(x) {
    toks <- attr(x, 'ids')
    attr(toks, 'types') <- attr(x, 'types')
    class(toks) <- c("tokens", "tokenizedTexts")
    return(toks)
}



