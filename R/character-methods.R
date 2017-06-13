# @rdname summary.corpus
# @method summary character
# @examples
# 
# # summarize texts
# summary(c("Testing this text.  Second sentence.", "And this one."))
# summary(data_char_ukimmig2010)
# myTextSummaryDF <- summary(data_char_ukimmig2010, verbose = FALSE)
# head(myTextSummaryDF)
summary.character <- function(object, n = 100, verbose = TRUE, tolower = FALSE, ...) {
    object <- object[1 : min(c(n, length(object)))]
    if (is.null(names(object))) 
        names(object) <- paste("text", seq_along(object), sep = "")
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
    if (verbose) print(results, ...)
    return(invisible(results))
}


