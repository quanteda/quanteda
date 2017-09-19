#' segment tokens object by patterns
#' 
#' @param x \link{tokens} object whose token elements will be segmented
#' @inheritParams pattern
#' @inheritParams valuetype
#' @param case_insensitive ignore case when matching, if \code{TRUE}
#' @param extract_pattern remove matched patterns from the texts and save in
#'   \link{docvars}, if \code{TRUE}
#' @param verbose if \code{TRUE} print messages about how many tokens were 
#'   selected or removed
#' @return \code{tokens_segment} returns a \link{tokens} object whose documents 
#'   have been redefined according to the segmentation type
#' @keywords tokens internal
#' @export
#' @examples 
#' txts <- "Fellow citizens, I am again called upon by the voice of my country to
#' execute the functions of its Chief Magistrate. When the occasion proper for
#' it shall arrive, I shall endeavor to express the high sense I entertain of
#' this distinguished honor."
#' toks <- tokens(txts)
#' 
#' # split by any punctuation
#' toks_punc <- tokens_segment(toks, c(".", "?", "!"), valuetype = 'fixed', 
#'                             pattern_position = 'after')
#' toks_punc <- tokens_segment(toks, "^\\p{P}$", valuetype = 'regex', extract_pattern = TRUE, 
#'                             pattern_position = 'after')
#' 
tokens_segment <- function(x, pattern,
                           valuetype = c("glob", "regex", "fixed"),
                           case_insensitive = TRUE, 
                           extract_pattern = FALSE,
                           pattern_position = c("before", "after"),
                           verbose = quanteda_options("verbose")) {
    UseMethod("tokens_segment")
}

#' @rdname tokens_segment
#' @noRd
#' @export
tokens_segment.tokenizedTexts <- function(x, pattern,
                                          valuetype = c("glob", "regex", "fixed"),
                                          case_insensitive = TRUE, 
                                          extract_pattern = FALSE,
                                          pattern_position = c("before", "after"), 
                                          verbose = quanteda_options("verbose")) {
    x <- tokens_segment(as.tokens(x), pattern, valuetype, case_insensitive, extract_pattern, verbose)
    x <- as.tokenizedTexts(x)
    return(x)
}

#' @rdname tokens_segment
#' @noRd
#' @importFrom RcppParallel RcppParallelLibs
#' @export
tokens_segment.tokens <- function(x, pattern,
                                  valuetype = c("glob", "regex", "fixed"),
                                  case_insensitive = TRUE, 
                                  extract_pattern = FALSE,
                                  pattern_position = c("before", "after"),
                                  verbose = quanteda_options("verbose"), ...) {
    
    valuetype <- match.arg(valuetype)
    pattern_position <- match.arg(pattern_position)
    
    attrs <- attributes(x)
    types <- types(x)
    vars <- get_docvars2(x, c( 'system', 'user'))

    patt_id <- features2id(pattern, types, valuetype, case_insensitive, attr(x, 'concatenator'))
    
    if ("" %in% pattern) patt_id <- c(patt_id, list(0)) # append padding index

    if (pattern_position == "before") {
        x <- qatd_cpp_tokens_segment(x, types, patt_id, extract_pattern, 1)
    } else {
        x <- qatd_cpp_tokens_segment(x, types, patt_id, extract_pattern, 2)
    }
    
    docname <- paste(attr(x, 'document'), as.character(attr(x, 'segid')), sep = '.')
    
    if (!is.null(vars)) {
        # create repeated docvars with document and segment IDs
        rownames(vars) <- NULL # faster to repeat rows without rownames
        vars <- vars[attr(x, 'docid'),,drop = FALSE]
        vars[,'_document'] <- attr(x, 'document')
        vars[,'_docid'] <- attr(x, 'docid')
        vars[,'_segid'] <- attr(x, 'segid')
        rownames(vars) <- docname
    } else {
        vars <- data.frame('_document' = attr(x, 'document'),
                           '_docid' = attr(x, 'docid'),
                           '_segid' = attr(x, 'segid'),
                           row.names = docname,
                           stringsAsFactors = FALSE)
    }
    if (extract_pattern) 
        vars[,'pattern'] = attr(x, 'pattern')
    
    create(x, what = 'tokens', names = docname, docvars = vars,  attrs = attrs)
}
