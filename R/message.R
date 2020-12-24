
#' Conditionally format and print messages
#' 
#' @param x message template to be pasted to [`sprintf()`] 
#' @param values list of values to be printed. Coerced to list if vector is given.
#' @param indices list of integer to specify which value to be printed
#' @param ... passed to underlying message functions 
#' @examples 
#' \dontrun{
#' quanteda:::info("you cannot delete %s", 
#'                 c("a document", "documents"), indices = TRUE, warn = TRUE)
#' quanteda:::info("tokens has %s", 
#'                 c("sentences", "paragraphs", "documents"), indices = 2)
#' 
#' dfmt <- data_dfm_lbgexample
#' quanteda:::info("dfm has %d %s and %d %s", 
#'      list(ndoc(dfmt), c("document", "documents"),
#'           nfeat(dfmt), c("feature", "features")), 
#'      list(1, ndoc(dfmt) > 1, 
#'           1, nfeat(dfmt) > 1), warn = FALSE)
#' }      
info <- function(x, values, indices = NULL, warn = FALSE, ...) {
    if (!is.list(values))
        values <- list(values)
    if (!is.list(indices))
        indices <- list(indices)
    if (length(indices)) {
        stopifnot(all(lengths(indices) == 1))
        values <- mapply(function(x, y) {
            if (length(x) == 1)
                return(x)
            if (is.logical(y))
                y <- which(c(FALSE, TRUE) == y)
            return(x[y])
        }, values, indices, SIMPLIFY = FALSE)
    }
    msg <- do.call(sprintf, c(list(x), values))
    if (warn) {
        warning(msg, call. = FALSE, ...)
    } else {
        message(msg, ...)
    }
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

