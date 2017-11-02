#' select or remove tokens from a tokens object
#' 
#' These function select or discard tokens from a \link{tokens} objects.  For 
#' convenience, the functions \code{tokens_remove} and \code{tokens_keep} 
#' are defined as shortcuts for 
#' \code{tokens_select(x, pattern, selection = "remove")} and \code{tokens_select(x, pattern, selection = "keep")}, 
#' respectively.  The most common 
#' usage for \code{tokens_remove} will be to eliminate stop words from a text or
#' text-based object, while the most common use of \code{tokens_select} will be 
#' to select tokens with only positive pattern matches from a list of regular 
#' expressions, including a dictionary.
#' @param x \link{tokens} object whose token elements will be removed or kept
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
#' @param window integer of length 1 or 2; the size of the window of tokens
#'   adjacent to \code{pattern} that will be selected. The window is symmetric
#'   unless a vector of two elements is supplied, in which case the first
#'   element will be the token length of the window before \code{pattern}, and
#'   the second will be the token length of the window after \code{pattern}.
#'   The default is \code{0}, meaning that only the pattern matched token(s) are
#'   selected, with no adjacent terms.
#'
#'   Terms from overlapping windows are never double-counted, but simply
#'   returned in the patern match. This is because \code{tokens_select} never
#'   redefines the document units; for this, see \code{\link{kwic}}.
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
#' # use window
#' tokens_select(toks, "second", selection = "keep", window = 1)
#' tokens_select(toks, "second", selection = "remove", window = 1)
#' tokens_remove(toks, "is", window = c(0, 1))
#' 
tokens_select <- function(x, pattern, selection = c("keep", "remove"), 
                          valuetype = c("glob", "regex", "fixed"),
                          case_insensitive = TRUE, padding = FALSE, window = 0, verbose = quanteda_options("verbose")) {
    UseMethod("tokens_select")
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
                                 case_insensitive = TRUE, padding = FALSE, window = 0, 
                                 verbose = quanteda_options("verbose"), ...) {
    
    selection <- match.arg(selection)
    valuetype <- match.arg(valuetype)
    attrs <- attributes(x)
    types <- types(x)
    
    features_id <- pattern2id(pattern, types, valuetype, case_insensitive, attr(x, 'concatenator'))
    if ("" %in% pattern) features_id <- c(features_id, list(0)) # append padding index

    if (verbose) 
        message_select(selection, length(features_id), 0)
    
    if (any(window < 0)) stop('window sizes cannot be negative')
    if (length(window) > 2) stop("window must be a integer vector of length 1 or 2")
    if (length(window) == 1) window <- rep(window, 2)
    if (selection == 'keep') {
        x <- qatd_cpp_tokens_select(x, types, features_id, 1, padding, window[1], window[2])
    } else {
        x <- qatd_cpp_tokens_select(x, types, features_id, 2, padding, window[1], window[2])
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
                          case_insensitive = TRUE, padding = FALSE, window = 0, verbose = quanteda_options("verbose")) {
    UseMethod("tokens_remove")
}

#' @noRd
#' @export
tokens_remove.tokens <- function(x, pattern, valuetype = c("glob", "regex", "fixed"),
                                case_insensitive = TRUE, padding = FALSE, window = 0, verbose = quanteda_options("verbose")) {
    tokens_select(x, pattern, selection = "remove", valuetype = valuetype, 
                  case_insensitive = case_insensitive, padding = padding, window = window, verbose = verbose)
}

#' @rdname tokens_select
#' @export
tokens_keep <- function(x, pattern, valuetype = c("glob", "regex", "fixed"),
                        case_insensitive = TRUE, padding = FALSE, window = 0, verbose = quanteda_options("verbose")) {
    UseMethod("tokens_keep")
}

#' @noRd
#' @export
tokens_keep.tokens <- function(x, pattern, valuetype = c("glob", "regex", "fixed"),
                                 case_insensitive = TRUE, padding = FALSE, window = 0, verbose = quanteda_options("verbose")) {
    tokens_select(x, pattern, selection = "keep", valuetype = valuetype, 
                  case_insensitive = case_insensitive, padding = padding, window = window, verbose = verbose)
}
