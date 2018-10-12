#' Summary statistics on a character vector
#' 
#' Internal-only function to compute summary statistics on a character object.
#' @inheritParams summary.corpus
#' @keywords char internal 
#' @examples
#' # summarize texts
#' quanteda:::summary_character(c("Testing this text. Second sentence.", "And this one."))
#' quanteda:::summary_character(data_char_ukimmig2010)
#' summ <- quanteda:::summary_character(data_char_ukimmig2010)
#' head(summ)
summary_character <- function(object, n = 100, tolower = FALSE, ...) {
    
    object <- head(object, n)
    if (is.null(names(object))) 
        names(object) <- paste0(quanteda_options("base_docname"), 
                                seq_along(object))
    if (tolower) 
        object <- char_tolower(object)
    temp <- tokens(object, ...)
    data.frame(Text = names(object),
               Types = ntype(temp),
               Tokens = ntoken(temp),
               Sentences = nsentence(object, ...),
               row.names = seq_along(object),
               stringsAsFactors = FALSE)

}
