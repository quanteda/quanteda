#' Summary statistics on a character vector
#' 
#' Internal-only function to compute summary statistics on a character object.
#' @method summary character
#' @inheritParams summary.corpus
#' @keywords internal 
#' @examples
#' # summarize texts
#' summary(c("Testing this text.  Second sentence.", "And this one."))
#' summary(data_char_ukimmig2010)
#' myTextSummaryDF <- summary(data_char_ukimmig2010)
#' head(myTextSummaryDF)
summary.character <- function(object, n = 100, tolower = FALSE, ...) {
    
    # trap the verbose argument and ignore
    thecall <- as.list(match.call())[-1]
    if (!is.na(verbose_index <- match("verbose", names(thecall)))) {
        warning("verbose argument is defunct")
        return(do.call(summary.character, thecall[-verbose_index]))
    }
    
    object <- object[1 : min(c(n, length(object)))]
    if (is.null(names(object))) 
        names(object) <- paste(quanteda_options("base_docname"), 
                               seq_along(object), sep = "")
    nsents  <- nsentence(object, ...)
    if (tolower) object <- char_tolower(object)
    toks <- tokens(object, ...)
    ntokens <- ntoken(toks)
    ntypes <- ntype(toks)
    results <- data.frame(Text = names(object),
                          Types = ntypes,
                          Tokens = ntokens,
                          Sentences = nsents,
                          row.names = NULL)
    results
}


