# messaging utilities ------------

#' Conditionally format messages
#' 
#' @inheritParams stringi::stri_sprintf()
#' @param pretty if `TRUE`, message is passed to [base::prettyNum()].
#' @keywords internal development
#' @examples 
#' \dontrun{
#' quanteda:::msg("you cannot delete %d %s", 2000, "documents")
msg <- function(format, ..., pretty = TRUE) {
    msg <- stringi::stri_sprintf(format, ...)
    if (pretty)
        msg <- prettyNum(msg, big.mark = ",")
    return(msg)
}

# rdname catm
# messages() with some of the same syntax as cat(): takes a sep argument and
# does not append a newline by default
catm <- function(..., sep = " ", appendLF = FALSE) {
    message(paste(..., sep = sep), appendLF = appendLF)
}

# used in displaying verbose messages for tokens and dfm constructors
message_create <- function(input, output) {
    message(msg("Creating a %s from a %s object...",
                output, input))
}

message_finish <- function(x, time) {
    if (is.dfm(x)) {
        message(msg(" ...complete, elapsed time: %s seconds.",
                    format((proc.time() - time)[3], digits = 3)))
        message(msg("Finished constructing a %d x %d sparse dfm.",
                    nrow(x), ncol(x)))
    } else {
        m <- length(types(x))
        n <- ndoc(x)
        message(msg(" ...%s unique %s", 
                    m, if (m > 1) "types" else "type"))
        message(msg(" ...complete, elapsed time: %s seconds.",
                    format((proc.time() - time)[3], digits = 3)))
        message(msg("Finished constructing %s from %s %s",
                    class(x)[1], n, if (n > 1) "documents" else "document"))
    }
}

# messaging methods ------------
             
#' Message parameter documentation
#' 
#' Used in printing verbose messages for message_tokens() and message_dfm()
#' @name messages
#' @param verbose if `TRUE` print the number of tokens and documents before and
#'   after the function is applied. The number of tokens does not include paddings.
#' @param before,after object statistics before and after the operation.
#' @seealso message_tokens() message_dfm()
#' @keywords internal
NULL

#' Print messages in tokens methods
#' @inheritParams messages
#' @keywords message internal
message_tokens <- function(operation, before, after) {
    message(msg("%s changed from %d tokens (%d documents) to %d tokens (%d documents)",
                operation, before$ntoken, before$ndoc, after$ntoken, after$ndoc))
}

stats_tokens <- function(x) {
    list(ndoc = ndoc(x),
         ntoken = sum(ntoken(x, remove_padding = TRUE)))
}

#' Print messages in dfm methods
#' @inheritParams messages
#' @keywords message internal
message_dfm <- function(operation, before, after) {
    message(msg("%s changed from %d features (%d documents) to %d features (%d documents)",
                operation, before$nfeat, before$ndoc, after$nfeat, after$ndoc))
}

stats_dfm <- function(x) {
    list(ndoc = ndoc(x),
         nfeat = nfeat(dfm_remove(x, "", verbose = FALSE)))
}
