#' segment tokens object by patterns
#' 
#' @param x \link{tokens} object whose token elements will be segmented
#' @param what unit of segmentation.  Current options are  
#'   \code{"sentences"} (default) and \code{"other"}.
#'   
#'   Segmenting on \code{"other"} allows segmentation of a text on any 
#'   user-defined value, and must be accompanied by the \code{delimiter} 
#'   argument. 
#' @param delimiter the same as \code{pattern}
#' @inheritParams valuetype
#' @param case_insensitive ignore case when matching, if \code{TRUE}
#' @param verbose if \code{TRUE} print messages about how many tokens were 
#'   selected or removed
#' @return @return \code{tokens_segment} returns a tokens of segmented texts,
#' @export
#' @examples 
#' txts <- "Fellow citizens, I am again called upon by the voice of my country to
#' execute the functions of its Chief Magistrate. When the occasion proper for
#' it shall arrive, I shall endeavor to express the high sense I entertain of
#' this distinguished honor."
#' toks <- tokens(txts)
#' 
#' # split into sentences
#' toks_sent <- tokens_segment(toks, what = "sentences")
#' 
#' # split by any punctuation
#' toks_punc <- tokens_segment(toks, what = "other", delimiter = "[\\p{P}]", valuetype = 'regex')
#' 
tokens_segment <- function(x, what = c("sentences", "other"), 
                           delimiter = NULL,
                           valuetype = c("glob", "regex", "fixed"),
                           case_insensitive = TRUE, verbose = quanteda_options("verbose")) {
    UseMethod("tokens_segment")
}

#' @rdname tokens_segment
#' @noRd
#' @export
tokens_segment.tokenizedTexts <- function(x, what = c("sentences", "other"), 
                                          delimiter = NULL,
                                          valuetype = c("glob", "regex", "fixed"),
                                          case_insensitive = TRUE, 
                                          verbose = quanteda_options("verbose")) {
    x <- tokens_segment(as.tokens(x), delimiter, valuetype, case_insensitive, verbose)
    x <- as.tokenizedTexts(x)
    return(x)
}

#' @rdname tokens_segment
#' @noRd
#' @importFrom RcppParallel RcppParallelLibs
#' @export
tokens_segment.tokens <- function(x, what = c("sentences", "other"), 
                                  delimiter = NULL,
                                  valuetype = c("glob", "regex", "fixed"),
                                  case_insensitive = TRUE, 
                                  verbose = quanteda_options("verbose"), ...) {
    
    what <- match.arg(what)
    valuetype <- match.arg(valuetype)
    attrs <- attributes(x)
    types <- types(x)
    vars <- docvars(x)

    if (what == "sentences") {
        if (!is.null(delimiter)) warning("delimiter is only used for 'other'")
        pattern <- "^[\\p{Sterm}]$"
        valuetype <- "regex"
    } else {
        pattern <- delimiter
    }
    
    patt_id <- features2id(pattern, types, valuetype, case_insensitive, attr(x, 'concatenator'))
    if ("" %in% pattern) patt_id <- c(patt_id, list(0)) # append padding index

    x <- qatd_cpp_tokens_segment(x, types, patt_id)
    
    # create repeated docvars with document and segment IDs
    rownames(vars) <- NULL # faster to repeat rows without rownames
    vars <- cbind(vars[attr(x, 'docid'),,drop = FALSE], 
                  '_docid' = attr(x, 'docid'), 
                  '_segid' = attr(x, 'segid'))
    rownames(vars) <- stri_c(attr(x, 'document'), '.', attr(x, 'segid'), sep = '')
    
    create(x, what = 'tokens', 
           names = rownames(vars), 
           docvars = vars, 
           attrs = attrs)
}
