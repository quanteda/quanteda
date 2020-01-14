#' Functions to add or retrieve corpus summary metadata
#'
#' @name summary_metadata
#' @aliases add_summary_metadata
#' @param x [corpus] object
#' @param ... additional arguments passed to [tokens()] when computing the
#'   summary information
#' @return `add_summary_metadata()` returns a corpus with summary metadata added
#'   as a data.frame, with the top-level list element names `summary`.
#' @details This is provided so that a [corpus] object can be stored with
#'   summary information to avoid having to compute this every time
#'   `[summary.corpus()]` is called.
#'   
#'   So in future calls, if `!is.null(meta(x, "summary", type = "system") &&
#'   !length(list(...))`, then `summary.corpus()` will simply return
#'   `get_system_meta()` rather than compute the summary statistics on the fly,
#'   which requires tokenizing the text.
#' @keywords corpus internal
#' @examples
#' corp <- corpus(data_char_ukimmig2010)
#' corp <- quanteda:::add_summary_metadata(corp)
#' quanteda:::get_summary_metadata(corp)
add_summary_metadata <- function(x, ...) {
    meta_system(x, "summary") <- summary(x, n = ndoc(x), showmeta = FALSE, ...)
    x
}

#' @rdname summary_metadata
#' @return `get_summary_metadata()` returns the summary metadata as a data.frame.
get_summary_metadata <- function(x, ...) {
    result <- meta(x, "summary", type = "system")
    if (!identical(docnames(x), result$Text)) {
        # warning("documents have changed; computing summary")
        meta_system(x, "summary") <- NULL
        result <- summary(x, n = ndoc(x), showmeta = FALSE, ...)
    }
    result
}
