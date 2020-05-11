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

    dict <- dictionary(list(
        "number" = "\\p{N}",
        "punct" = "\\p{P}",
        "symbol" = "\\p{S}",
        "any" = "[\\p{N}\\p{P}\\p{S}]"
    ))
    is_dup <- duplicated(texts(object))
    n_sent <- ntoken(tokens(object, what = "sentence"))
    temp <- dfm(tokens(object, ...))
    if (tolower) 
        temp <- dfm_tolower(temp)
    result <- convert(
        dfm_lookup(temp, dictionary = dict, valuetype = "regex"),
        "data.frame"
    )
    result$n_sent <- n_sent
    result$n_token <- ntoken(temp)
    result$n_type <- nfeat(temp) 
    result$dupli <- is_dup
    result$noise <- result$any / result$n_token
    return(result)
}
