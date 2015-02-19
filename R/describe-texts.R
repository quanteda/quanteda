# @param object The texts to be described
# @param verbose set to \code{FALSE} to suppress printing, for instance
# if you simply want to assign the output to a \code{data.frame}
# @param ... additional arguments affecting the summary produced
#' @rdname summary.corpus 
#' @method summary character
#' @export
#' @examples
#' # 
#' # summarize texts
#' summary(c("testing this text", "and this one"))
#' summary(ukimmigTexts)
#' myTextSummaryDF <- summary(ukimmigTexts, verbose=FALSE)
summary.character <- function(object, verbose=TRUE, ...) {
    # need to implement subsetting here too
    if (is.null(names(object))) 
        names(object) <- paste("text", 1:length(object), sep="")
    tokenizedTexts <- tokenize(object)
    ntokens <- sapply(tokenizedTexts, length)
    temp <- lapply(tokenizedTexts, unique)
    ntypes <- sapply(temp, length)
    # because we still don't have a generic sentence segmenter
    # broken
    nsents  <- sapply(object, function(s) length(gregexpr("[.!?]", s)[[1]]))
    
    results <- data.frame(Text=names(object),
                          Types=ntypes,
                          Tokens=ntokens,
                          Sentences=nsents,
                          row.names=NULL)
    if (verbose) print(results, ...)
    return(invisible(results))
}

#' @rdname summary.corpus
#' @export
describeTexts <- function(object, verbose=TRUE, ...) {
    cat("note: describeTexts is deprecated, use summary instead.\n")
    UseMethod("summary.character")
}

