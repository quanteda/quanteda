# messaging utilities ------------

#' Conditionally format messages
#'
#' @inheritParams stringi::stri_sprintf
#' @param pretty if `TRUE`, message is passed to [base::prettyNum()].
#' @keywords internal development
#' @seealso [stringi::stri_sprintf]
#' @examples
#' quanteda:::msg("you cannot delete %s %s", 2000, "documents")
msg <- function(format, ..., pretty = TRUE) {
    args <- list(...)
    if (pretty) {
        args <- lapply(args, prettyNum, big.mark = ",")
    } else {
        args <- lapply(args, as.character)
    }
    args$format <- format
    do.call(stringi::stri_sprintf, args)
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
        message(msg("Finished constructing a %s x %s sparse dfm.",
                    nrow(x), ncol(x)))
    } else {
        m <- length(types(x))
        n <- ndoc(x)
        message(msg(" ...%s unique %s",
                    m, if (m == 1) "type" else "types"))
        message(msg(" ...complete, elapsed time: %s seconds.",
                    format((proc.time() - time)[3], digits = 3)))
        message(msg("Finished constructing %s from %s %s",
                    class(x)[1],
                    n, if (n == 1) "document" else "documents"))
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

#' Print messages in corpus methods
#' @inheritParams messages
#' @keywords message internal
message_corpus <- function(operation, before, after) {
    message(msg("%s changed from %s characters (%s documents) to %s characters (%s documents)",
                operation, before$nchar, before$ndoc, after$nchar, after$ndoc))
}

stats_corpus <- function(x) {
    list(ndoc = ndoc(x),
         nchar = sum(nchar(x)))
}

#' Print messages in tokens methods
#' @inheritParams messages
#' @keywords message internal
message_tokens <- function(operation, before, after) {
    message(msg("%s changed from %s tokens (%s documents) to %s tokens (%s documents)",
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
    message(msg("%s changed from %s features (%s documents) to %s features (%s documents)",
                operation, before$nfeat, before$ndoc, after$nfeat, after$ndoc))
}

stats_dfm <- function(x) {
    list(ndoc = ndoc(x),
         nfeat = nfeat(dfm_remove(x, "", verbose = FALSE)))
}
