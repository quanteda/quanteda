# messaging utilities ------------

#' Conditionally format messages
#'
#' @inheritParams stringi::stri_sprintf
#' @param prepend text is added before the message
#' @param append text is added after the message
#' @keywords internal development
#' @seealso [stringi::stri_sprintf]
#' @examples
#' quanteda:::msg("you cannot delete %s %s", 2000, "documents")
msg <- function(format, ..., prepend = "", append = "") {
    args <- list(...)
    args <- lapply(args, function(x) {
        if (is.numeric(x)) {
            prettyNum(x, big.mark = ",")
        } else {
            as.character(x)
        }
    })
    args$format <- format
    paste0(prepend, do.call(stringi::stri_sprintf, args), append)
}

#' Wrap and print long lines
#' 
#' @param x a character string to wrap.
#' @param ... extra arguments passed to [stringi::stri_wrap]`.
#' @keywords internal development
#' @importFrom stringi stri_wrap
wrap <- function(x, ...) {
    cat(stri_wrap(x, getOption("width"), ...), sep = "\n")
}

#' Return inflected forms of words

#' @param n the number of elements.
#' @param word singular form of the word.
#' @keywords internal development
inflect <- function(word, n) {
    v <- c("document" = "documents",
           "feature" = "features",
           "type" = "types",
           "docvar" = "docvars",
           "token" = "tokens",
           "character" = "characters",
           "entry" = "entries",
           "key" = "keys",
           "match" = "matches")
    if (n == 1)
        return(word)
    return(v[word])
    
}

# rdname catm
# messages() with some of the same syntax as cat(): takes a sep argument and
# does not append a newline by default
# NOTE: consider changing to message0() with wrapping
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
        m <- count_types(x)
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
         nchar = sum(nchar(x)),
         ndocvar = ncol(docvars(x)))
}

#' Print messages in tokens methods
#' @inheritParams messages
#' @keywords message internal
message_tokens <- function(operation, before, after) {
    message(msg("%s changed from %s types (%s documents, %s tokens) to %s types (%s documents, %s tokens)",
                operation, before$ntype, before$ndoc, before$ntoken, after$ntype, after$ndoc, after$ntoken))
}

stats_tokens <- function(x) {
    list(ndoc = ndoc(x),
         ntoken = sum(ntoken(x, remove_padding = FALSE)),
         ntype = count_types(x),
         ndocvar = ncol(docvars(x)))
}

#' Print messages in dfm methods
#' @inheritParams messages
#' @keywords message internal
message_dfm <- function(operation, before, after) {
    message(msg("%s changed from %s features (%s documents) to %s features (%s documents)",
                operation, before$nfeat, before$ndoc, after$nfeat, after$ndoc))
}

stats_dfm <- function(x) {
    x <- dfm_remove(x, "", verbose = FALSE)
    list(ndoc = ndoc(x),
         ntoken = sum(x),
         nfeat = nfeat(x),
         ndocvar = ncol(docvars(x)))
}

summary_corpus <- function(x) {
    s <- stats_corpus(x)
    line <- msg("Corpus of %s %s (%s %s)",
                s$ndoc, inflect("document", s$ndoc),
                s$nchar, inflect("character", s$nchar))
    if (s$ndocvar)
        line <- msg(" and %s %s",
                    s$ndocvar, inflect("docvar", s$ndocvar),
                    prepend = line)
    wrap(paste0(line, "."))
}

summary_tokens <- function(x) {
    s <- stats_tokens(x)
    
    if (is.tokens_xptr(x)) {
        line <- msg("Tokens_xptr [%s] of %s %s (%s %s, %s %s)", 
                    address(x),
                    s$ndoc, inflect("document", s$ndoc),
                    s$ntype, inflect("type", s$ntype),
                    s$ntoken, inflect("token", s$ntoken))
    } else {
        line <- msg("Tokens of %s %s (%s %s, %s %s)", 
                    s$ndoc, inflect("document", s$ndoc),
                    s$ntype, inflect("type", s$ntype),
                    s$ntoken, inflect("token", s$ntoken))
    }
    if (s$ndocvar)
        line <- msg(" and %s %s",
                    s$ndocvar, inflect("docvar", s$ndocvar),
                    prepend = line)
    wrap(paste0(line, "."))
}

summary_dfm <- function(x) {
    s <- stats_dfm(x)
    line <- msg("Document-feature matrix of %s %s (%s %s, %s %s)",
                s$ndoc, inflect("document", s$ndoc),
                s$nfeat, inflect("feature", s$nfeat),
                s$ntoken, inflect("token", s$ntoken))
    if (s$ndocvar)
        line <- msg(" and %s %s", 
                    s$ndocvar, inflect("docvar", s$ndocvar),
                    prepend = line)
    wrap(paste0(line, "."))
}


