#' select or remove tokens from a tokens object
#' 
#' This function selects or discards tokens from a \link{tokens} objects, with 
#' the shortcut \code{tokens_remove(x, pattern)} defined as a shortcut for 
#' \code{tokens_select(x, pattern, selection = "remove")}.  The most common 
#' usage for \code{tokens_remove} will be to eliminate stop words from a text or
#' text-based object, while the most common use of \code{tokens_select} will be 
#' to select tokens with only positive pattern matches from a list of regular 
#' expressions, including a dictionary.
#' @param x \link{tokens} object whose token elements will be selected
#' @inheritParams pattern
#' @param selection whether to \code{"keep"} or \code{"remove"} the tokens 
#'   matching \code{pattern}
#' @inheritParams valuetype
#' @param case_insensitive ignore case when matching, if \code{TRUE}
#' @param verbose if \code{TRUE} print messages about how many tokens were 
#'   selected or removed
#' @param padding if \code{TRUE}, leave an empty string where the removed tokens
#'   previously existed.  This is useful if a positional match is needed between
#'   the pre- and post-selected tokens, for instance if a window of adjacency 
#'   needs to be computed.
#' @return a \link{tokens} object with tokens selected or removed based on their
#'   match to \code{pattern}
#' @export
#' @examples 
#' ## tokens_select with simple examples
#' toks <- tokens(c("This is a sentence.", "This is a second sentence."), 
#'                  remove_punct = TRUE)
#' tokens_select(toks, c("is", "a", "this"), selection = "keep", padding = FALSE)
#' tokens_select(toks, c("is", "a", "this"), selection = "keep", padding = TRUE)
#' tokens_select(toks, c("is", "a", "this"), selection = "remove", padding = FALSE)
#' tokens_select(toks, c("is", "a", "this"), selection = "remove", padding = TRUE)
#' 
#' # how case_insensitive works
#' tokens_select(toks, c("is", "a", "this"), selection = "remove", case_insensitive = TRUE)
#' tokens_select(toks, c("is", "a", "this"), selection = "remove", case_insensitive = FALSE)
#' 
tokens_select <- function(x, pattern, selection = c("keep", "remove"), 
                          valuetype = c("glob", "regex", "fixed"),
                          case_insensitive = TRUE, padding = FALSE, verbose = quanteda_options("verbose")) {
    UseMethod("tokens_select")
}

#' @rdname tokens_select
#' @noRd
#' @export
#' @examples 
#' \dontshow{
#' ## tokens_select example
#' toks <- tokens(c("This is a sentence.", "This is a second sentence."), 
#'                  remove_punct = TRUE)
#' tokens_select(toks, c("is", "a", "this"), selection = "remove", 
#'               valuetype = "fixed", padding = TRUE, case_insensitive = TRUE)
#' 
#' # how case_insensitive works
#' tokens_select(toks, c("is", "a", "this"), selection = "remove", 
#'                valuetype = "fixed", padding = TRUE, case_insensitive = FALSE)
#' tokens_select(toks, c("is", "a", "this"), selection = "remove", 
#'                valuetype = "fixed", padding = TRUE, case_insensitive = TRUE)
#' tokens_select(toks, c("is", "a", "this"), selection = "remove", 
#'                valuetype = "glob", padding = TRUE, case_insensitive = TRUE)
#' tokens_select(toks, c("is", "a", "this"), selection = "remove", 
#'                valuetype = "glob", padding = TRUE, case_insensitive = FALSE)
#' 
#' # with longer texts
#' toks <- tokens(data_corpus_inaugural[1:2])
#' tokens_select(toks, stopwords("english"), "remove")
#' tokens_select(toks, stopwords("english"), "keep")
#' tokens_select(toks, stopwords("english"), "remove", padding = TRUE)
#' tokens_select(toks, stopwords("english"), "keep", padding = TRUE)
#' tokens_select(tokensdata_corpus_inaugural[2]), stopwords("english"), "remove", padding = TRUE)
#' }
tokens_select.tokenizedTexts <- function(x, pattern, selection = c("keep", "remove"), 
                                         valuetype = c("glob", "regex", "fixed"),
                                         case_insensitive = TRUE, padding = FALSE, 
                                         verbose = quanteda_options("verbose")) {
    x <- tokens_select(as.tokens(x), pattern, selection, valuetype, case_insensitive, padding, verbose)
    x <- as.tokenizedTexts(x)
    return(x)
}

#' @rdname tokens_select
#' @noRd
#' @importFrom RcppParallel RcppParallelLibs
#' @export
#' @examples
#' toksh <- tokens(c(doc1 = "This is a SAMPLE text", doc2 = "this sample text is better"))
#' feats <- c("this", "sample", "is")
#' # keeping tokens
#' tokens_select(toksh, feats, selection = "keep")
#' tokens_select(toksh, feats, selection = "keep", padding = TRUE)
#' tokens_select(toksh, feats, selection = "keep", case_insensitive = FALSE)
#' tokens_select(toksh, feats, selection = "keep", padding = TRUE, case_insensitive = FALSE)
#' # removing tokens
#' tokens_select(toksh, feats, selection = "remove")
#' tokens_select(toksh, feats, selection = "remove", padding = TRUE)
#' tokens_select(toksh, feats, selection = "remove", case_insensitive = FALSE)
#' tokens_select(toksh, feats, selection = "remove", padding = TRUE, case_insensitive = FALSE)
#' 
#' # With longer texts
#' toks <- tokens(data_corpus_inaugural)
#' tokens_select(toks, stopwords("english"), "remove")
#' tokens_select(toks, stopwords("english"), "keep")
#' tokens_select(toks, stopwords("english"), "remove", padding = TRUE)
#' tokens_select(toks, stopwords("english"), "keep", padding = TRUE)
#' 
#' # With multiple words
#' tokens_select(toks, list(c('President', '*')), "keep")
#' tokens_select(toks, 'President *', "keep") # simplified form
#' tokens_select(toks, list(c('*', 'crisis')), "keep")
#' tokens_select(toks, '* crisis', "keep") # simplified form
tokens_select.tokens <- function(x, pattern, selection = c("keep", "remove"), 
                                 valuetype = c("glob", "regex", "fixed"),
                                 case_insensitive = TRUE, padding = FALSE, 
                                 verbose = quanteda_options("verbose"), ...) {
    
    selection <- match.arg(selection)
    valuetype <- match.arg(valuetype)
    attrs <- attributes(x)
    types <- types(x)
    
    features_id <- pattern2id(pattern, types, valuetype, case_insensitive, attr(x, 'concatenator'))
    if ("" %in% pattern) features_id <- c(features_id, list(0)) # append padding index

    if (verbose) 
        message_select(selection, length(features_id), 0)
    if (selection == 'keep') {
        x <- qatd_cpp_tokens_select(x, types, features_id, 1, padding)
    } else {
        x <- qatd_cpp_tokens_select(x, types, features_id, 2, padding)
    }
    attributes(x, FALSE) <- attrs
    return(x)
}

#' @rdname tokens_select
#' @export
#' @examples
#' ## tokens_remove example
#' txt <- c(wash1 <- "Fellow citizens, I am again called upon by the voice of my country to 
#'                    execute the functions of its Chief Magistrate.",
#'          wash2 <- "When the occasion proper for it shall arrive, I shall endeavor to express
#'                    the high sense I entertain of this distinguished honor.")
#' tokens_remove(tokens(txt, remove_punct = TRUE), stopwords("english"))
#'
tokens_remove <- function(x, pattern, valuetype = c("glob", "regex", "fixed"),
                          case_insensitive = TRUE, padding = FALSE, verbose = quanteda_options("verbose")) {
    UseMethod("tokens_remove")
}

#' @noRd
#' @export
tokens_remove.tokenizedTexts <- function(x, pattern, valuetype = c("glob", "regex", "fixed"),
                          case_insensitive = TRUE, padding = FALSE, verbose = quanteda_options("verbose")) {
    tokens_select(x, pattern, selection = "remove", valuetype = valuetype, 
                  case_insensitive = case_insensitive, padding = padding, verbose = verbose)
}


