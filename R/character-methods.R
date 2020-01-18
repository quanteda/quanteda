#' Summary statistics on a character vector
#' 
#' Internal-only function to compute summary statistics on a character object.
#' @inheritParams summary.corpus
#' @keywords char internal 
#' @examples
#' # summarize texts
#' quanteda:::summarize_texts(c("Testing this text. Second sentence.", "And this one."))
#' quanteda:::summarize_texts(data_char_ukimmig2010)
#' summary_ukimmig2010 <- quanteda:::summarize_texts(data_char_ukimmig2010)
#' head(summary_ukimmig2010)
summarize_texts <- function(object, tolower = FALSE, ...) {

    if (is.null(names(object))) 
        names(object) <- paste0(quanteda_options("base_docname"), 
                                seq_along(object))
    if (tolower) 
        object <- char_tolower(object)
    temp <- tokens(object, ...)
    data.frame(Text = names(object),
               Types = ntype(temp),
               Tokens = ntoken(temp),
               Sentences = suppressWarnings(nsentence(object, ...)),
               row.names = seq_along(object),
               stringsAsFactors = FALSE)
}
