# @param object The texts to be described
# @param verbose set to \code{FALSE} to suppress printing, for instance
# if you simply want to assign the output to a \code{data.frame}
# @param ... additional arguments affecting the summary produced
#' @rdname summary.corpus 
#' @method summary character
#' @export
#' @examples
#' 
#' # summarize texts
#' summary(c("Testing this text.  Second sentence.", "And this one."))
#' summary(ukimmigTexts)
#' myTextSummaryDF <- summary(ukimmigTexts, verbose = FALSE)
#' head(myTextSummaryDF)
summary.character <- function(object, n = 100, verbose = TRUE, toLower = FALSE, ...) {
    object <- object[1 : min(c(n, length(object)))]
    if (is.null(names(object))) 
        names(object) <- paste("text", 1:length(object), sep="")
    nsents  <- nsentence(object, ...)
    if (toLower) object <- toLower(object)
    toks <- tokenize(object, ...)
    ntokens <- ntoken(toks)
    ntypes <- ntype(toks)
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
describeTexts <- function(object, n = 100, verbose=TRUE, ...) {
    catm("note: describeTexts is deprecated, use summary instead.\n")
    UseMethod("summary.character")
}
