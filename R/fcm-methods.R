#' @rdname dfm_compress
#' @note \code{fcm_compress} works only when the \link{fcm} was created with a
#' document context.
#' @return \code{fcm_compress} returns an \link{fcm} whose features have been
#' recombined by combining counts of identical features, summing their counts.
#' @export
#' @examples
#' # compress an fcm
#' fcmat1 <- fcm(tokens("A D a C E a d F e B A C E D"), 
#'              context = "window", window = 3)
#' ## this will produce an error:
#' # fcm_compress(fcmat1)
#' 
#' txt <- c("The fox JUMPED over the dog.",
#'          "The dog jumped over the fox.")
#' toks <- tokens(txt, remove_punct = TRUE)
#' fcmat2 <- fcm(toks, context = "document")
#' colnames(fcmat2) <- rownames(fcmat2) <- tolower(colnames(fcmat2))
#' colnames(fcmat2)[5] <- rownames(fcmat2)[5] <- "fox"
#' fcmat2
#' fcm_compress(fcmat2)
fcm_compress <- function(x) {
    UseMethod("fcm_compress")
}

#' @export
fcm_compress.default <- function(x) {
    stop(friendly_class_undefined_message(class(x), "fcm_compress"))
}

#' @export
fcm_compress.fcm <- function(x) {
    if (!is.fcm(x))
        stop("compress_fcm only works on a fcm object")
    if (x@context != "document")
        stop("compress_fcm invalid if fcm was created with a window context")
    matrix2fcm(dfm_compress(x, margin = "both"), attributes(x))
} 

#' Sort an fcm in alphabetical order of the features
#' 
#' Sorts an \link{fcm} in alphabetical order of the features.
#' 
#' @param x \link{fcm} object
#' @return A \link{fcm} object whose features have been alphabetically sorted. 
#'   Differs from \code{\link{fcm_sort}} in that this function sorts the fcm by
#'   the feature labels, not the counts of the features.
#' @export
#' @author Kenneth Benoit
#' @examples
#' # with tri = FALSE
#' fcmat1 <- fcm(tokens(c("A X Y C B A", "X Y C A B B")), tri = FALSE)
#' rownames(fcmat1)[3] <- colnames(fcmat1)[3] <- "Z"
#' fcmat1
#' fcm_sort(fcmat1)
#' 
#' # with tri = TRUE
#' fcmat2 <- fcm(tokens(c("A X Y C B A", "X Y C A B B")), tri = TRUE)
#' rownames(fcmat2)[3] <- colnames(fcmat2)[3] <- "Z"
#' fcmat2
#' fcm_sort(fcmat2)
fcm_sort <- function(x) {
    UseMethod("fcm_sort")    
}

#' @export
fcm_sort.default <- function(x) {
    stop(friendly_class_undefined_message(class(x), "fcm_sort"))
}

#' @export
fcm_sort.fcm <- function(x) {
    attrs <- attributes(x)
    x <- as(x, "dgTMatrix") # make a triplet
    x <- x[order(rownames(x)), order(colnames(x))]
    if (attrs$tri) {
        swap <- which(x@i > x@j)
        i <- x@i[swap]
        x@i[swap] <- x@j[swap]
        x@j[swap] <- i
        x <- matrix2fcm(x, attrs)
    }
    return(x)
}

#' @rdname dfm_select
#' @export
#' @examples 
#' toks <- tokens(c("this contains lots of stopwords",
#'                  "no if, and, or but about it: lots"),
#'                remove_punct = TRUE)
#' fcmat <- fcm(toks)
#' fcmat
#' fcm_remove(fcmat, stopwords("english"))
fcm_select <- function(x, pattern = NULL, selection = c("keep", "remove"), 
                       valuetype = c("glob", "regex", "fixed"),
                       case_insensitive = TRUE,
                       verbose = quanteda_options("verbose"), ...) {
    UseMethod("fcm_select")
}

#' @export
fcm_select.default <- function(x, pattern = NULL, 
                               selection = c("keep", "remove"), 
                               valuetype = c("glob", "regex", "fixed"),
                               case_insensitive = TRUE,
                               verbose = quanteda_options("verbose"), ...) {
    stop(friendly_class_undefined_message(class(x), "fcm_select"))
}

#' @export
fcm_select.fcm <- function(x, pattern = NULL, 
                           selection = c("keep", "remove"), 
                           valuetype = c("glob", "regex", "fixed"),
                           case_insensitive = TRUE,
                           verbose = quanteda_options("verbose"), ...) {
    
    attrs <- attributes(x)
    x <- t(dfm_select(x, pattern, selection, valuetype, 
                      case_insensitive, verbose = verbose, ...))
    x <- t(dfm_select(x, pattern, selection, valuetype, 
                      case_insensitive, verbose = FALSE, ...))
    matrix2fcm(x, attrs)
}


#' @rdname dfm_select
#' @export
fcm_remove <- function(x, pattern = NULL, ...) {
    UseMethod("fcm_remove")
}

#' @export
fcm_remove.default <- function(x, pattern = NULL, ...) {
    stop(friendly_class_undefined_message(class(x), "fcm_remove"))
}

#' @export
fcm_remove.fcm <- function(x, pattern = NULL, ...) {
    fcm_select(x, pattern, selection = "remove", ...)
}

#' @rdname dfm_select
#' @export
fcm_keep <- function(x, pattern = NULL, ...) {
    UseMethod("fcm_keep")
}

#' @export
fcm_keep.default <- function(x, pattern = NULL, ...) {
    stop(friendly_class_undefined_message(class(x), "fcm_keep"))
}

#' @noRd
#' @export
fcm_keep.fcm <- function(x, pattern = NULL, ...) {
    fcm_select(x, pattern, selection = "keep", ...)
}


#' Coercion functions for fcm objects
#' @param x an object coerced to \link{fcm}. Currently only support
#'   \link{Matrix} objects.
#' @keywords internal
as.fcm <- function(x) {
    matrix2fcm(x)
}

#' Converts a Matrix to a fcm
#' @param x a Matrix
#' @param slots slots a list of values to be assigned to slots
#' @keywords internal
matrix2fcm <- function(x, slots = NULL) {
    
    rowname <- rownames(x)
    if (nrow(x) > length(rowname))
        rowname <- paste0(quanteda_options("base_featname"), seq_len(nrow(x)))
    
    colname <- colnames(x)
    if (ncol(x) > length(colname))
        colname <- paste0(quanteda_options("base_featname"), seq_len(ncol(x)))
    
    x <- Matrix(x, sparse = TRUE)
    x <- new("fcm", as(x, 'dgCMatrix'))
    set_fcm_dimnames(x) <- list(rowname, colname)
    set_fcm_slots(x, slots)
}

#' Set values to a fcm's S4 slots
#' @param x a fcm 
#' @param slots a list of values extracted using \code{attributes} and to be assigned to slots 
#' @param exceptions names of slots to be ignored
#' @keywords internal
set_fcm_slots <- function(x, slots = NULL, exceptions = NULL) {
    if (is.null(slots)) return(x)
    sname <- slotNames("fcm")
    sname <- setdiff(sname, c("Dim", "Dimnames", "i", "p", "x", "factors", exceptions))
    for (s in sname) {
        try({
            slot(x, s) <- slots[[s]]
        }, silent = TRUE)
    }
    return(x)
}
