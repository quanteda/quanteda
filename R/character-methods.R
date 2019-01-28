#' Summary statistics on a character vector
#' 
#' Internal-only function to compute summary statistics on a character object.
#' @inheritParams summary.corpus
#' @keywords char internal 
#' @examples
#' # summarize texts
#' quanteda:::summary_character(c("Testing this text. Second sentence.", "And this one."))
#' quanteda:::summary_character(data_char_ukimmig2010)
#' summary_ukimmig2010 <- quanteda:::summary_character(data_char_ukimmig2010)
#' head(summary_ukimmig2010)
summary_character <- function(object, n = 100, tolower = FALSE, ...) {
    
    # trap the verbose argument and ignore
    thecall <- as.list(match.call())[-1]
    if (!is.na(verbose_index <- match("verbose", names(thecall)))) {
        warning("verbose argument is defunct")
        return(do.call(summary_character, thecall[-verbose_index]))
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
