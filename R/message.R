# messaging utilities ------------

#' Conditionally format messages
#' 
#' @param x message template to be passed to [stringi::stri_sprintf()].
#' @param values list of values to be used in the template. Coerced to list if vector is given.
#' @param indices list of integer to specify which value to be used.
#' @param pretty if `TRUE`, message is passed to [base::prettyNum()].
#' @param ... additional arguments passed to [base::prettyNum()].
#' @keywords internal development
#' @examples 
#' \dontrun{
#' quanteda:::msg("you cannot delete %s", 
#'                c("a document", "documents"), indices = TRUE)
#' quanteda:::msg("tokens has %s", 
#'                c("sentences", "paragraphs", "documents"), indices = 2)
#' 
#' dfmat <- data_dfm_lbgexample
#' quanteda:::msg("dfm has %d %s and %d %s", 
#'      list(ndoc(dfmat), c("document", "documents"),
#'           nfeat(dfmat), c("feature", "features")), 
#'      list(1, ndoc(dfmat) > 1, 
#'           1, nfeat(dfmat) > 1))
#' }      
msg <- function(x, values = NULL, indices = NULL, pretty = TRUE, ...) {
    if (!is.null(values)) {
        if (!is.list(values))
            values <- list(values)
        if (!is.list(indices))
            indices <- as.list(indices)
        if (length(indices)) {
            values <- mapply(function(x, y) {
                if (length(x) == 1 || is.null(y))
                    return(x)
                if (is.logical(y))
                    y <- which(c(FALSE, TRUE) == y)
                return(x[y])
            }, values, indices, SIMPLIFY = FALSE)
        }
        msg <- do.call(stringi::stri_sprintf, c(list(x), values))
    } else {
        msg <- x
    }
    if (pretty)
        msg <- prettyNum(msg, big.mark = ",", ...)
    return(msg)
}

# rdname catm
# messages() with some of the same syntax as cat(): takes a sep argument and
# does not append a newline by default
catm <- function(..., sep = " ", appendLF = FALSE) {
    message(paste(..., sep = sep), appendLF = appendLF)
}

# used in displaying verbose messages for tokens_select and dfm_select
message_select <- function(selection, nfeats, ndocs, nfeatspad = 0, ndocspad = 0) {
    catm(if (selection == "keep") "kept" else "removed", " ",
         format(nfeats, big.mark = ",", scientific = FALSE),
         " feature", if (nfeats != 1L) "s" else "", sep = "")
    if (ndocs > 0) {
        catm(" and ",
             format(ndocs, big.mark = ",", scientific = FALSE),
             " document", if (ndocs != 1L) "s" else "",
             sep = "")
    }
    if ((nfeatspad + ndocspad) > 0) {
        catm(", padded ", sep = "")
    }
    if (nfeatspad > 0) {
        catm(format(nfeatspad, big.mark = ",", scientific = FALSE),
             " feature", if (nfeatspad != 1L) "s" else "",
             sep = "")
    }
    if (ndocspad > 0) {
        if (nfeatspad > 0) catm(" and ", sep = "")
        catm(format(ndocspad, big.mark = ",", scientific = FALSE),
             " document", if (ndocspad != 1L) "s" else "",
             sep = "")
    }
    catm("", appendLF = TRUE)
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
    msg <- sprintf("%s changed from %d tokens (%d documents) to %d tokens (%d documents)",
                   operation, before$ntoken, before$ndoc, after$ntoken, after$ndoc)
    msg <- prettyNum(msg, big.mark = ",")
    message(msg)
}

stats_tokens <- function(x) {
    list(ndoc = ndoc(x),
         ntoken = sum(ntoken(x, remove_padding = TRUE)))
}

#' Print messages in dfm methods
#' @inheritParams messages
#' @keywords message internal
message_dfm <- function(operation, before, after) {
    msg <- sprintf("%s changed from %d features (%d documents) to %d features (%d documents)",
                   operation, before$nfeat, before$ndoc, after$nfeat, after$ndoc)
    msg <- prettyNum(msg, big.mark = ",")
    message(msg)
}

stats_dfm <- function(x) {
    list(ndoc = ndoc(x),
         nfeat = nfeat(dfm_remove(x, "", verbose = FALSE)))
}
