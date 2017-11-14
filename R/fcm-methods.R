#' @rdname dfm_compress
#' @note \code{fcm_compress} works only when the \link{fcm} was created with a
#' document context.
#' @return \code{fcm_compress} returns an \link{fcm} whose features have been
#' recombined by combining counts of identical features, summing their counts.
#' @export
#' @examples
#' # compress an fcm
#' myfcm <- fcm(tokens("A D a C E a d F e B A C E D"), 
#'              context = "window", window = 3)
#' ## this will produce an error:
#' # fcm_compress(myfcm)
#' 
#' txt <- c("The fox JUMPED over the dog.",
#'          "The dog jumped over the fox.")
#' toks <- tokens(txt, remove_punct = TRUE)
#' myfcm <- fcm(toks, context = "document")
#' colnames(myfcm) <- rownames(myfcm) <- tolower(colnames(myfcm))
#' colnames(myfcm)[5] <- rownames(myfcm)[5] <- "fox"
#' myfcm
#' fcm_compress(myfcm)
fcm_compress <- function(x) {
    UseMethod("fcm_compress")
}

#' @noRd
#' @export
fcm_compress.fcm <- function(x) {
    if (!is.fcm(x))
        stop("compress_fcm only works on a fcm object")
    if (x@context != "document")
        stop("compress_fcm invalid if fcm was created with a window context")
    
    attrs <- attributes(x)
    x <- dfm_compress(x, margin = "both")
    result <- new("fcm", as(x, 'dgCMatrix'), count = attrs$count,
                  context = attrs$context, window = attrs$window, weights = attrs$weights, tri = attrs$tri)
    names(result@Dimnames) <- c("features", "features")
    return(result)
} 

#' sort an fcm in alphabetical order of the features
#' 
#' Sorts an \link{fcm} in alphabetical order of the features.
#' 
#' @param x \link{fcm} object
#' @return A \link{fcm} object whose features have been alphabetically sorted. 
#'   Differs from \code{\link{fcm_sort}} in that this function sorts the fcm by
#'   the feature labels, not the counts of the features.
#' @export
#' @author Ken Benoit
#' @examples
#' # with tri = FALSE
#' myfcm <- fcm(tokens(c("A X Y C B A", "X Y C A B B")), tri = FALSE)
#' rownames(myfcm)[3] <- colnames(myfcm)[3] <- "Z"
#' myfcm
#' fcm_sort(myfcm)
#' 
#' # with tri = TRUE
#' myfcm <- fcm(tokens(c("A X Y C B A", "X Y C A B B")), tri = TRUE)
#' rownames(myfcm)[3] <- colnames(myfcm)[3] <- "Z"
#' myfcm
#' fcm_sort(myfcm)
fcm_sort <- function(x) {
    UseMethod("fcm_sort")    
}
    
#' @noRd
#' @export
fcm_sort.fcm <- function(x) {
    attrs <- attributes(x)
    x <- x[order(rownames(x)), order(colnames(x))]
    if (x@tri) {
        # make a triplet
        tmp <- as(x, "dgTMatrix")
        swap <- which(tmp@i > tmp@j)
        i <- tmp@i[swap]
        tmp@i[swap] <- tmp@j[swap]
        tmp@j[swap] <- i
        x <- new("fcm", as(tmp, "dgCMatrix")) 
        slots(x) <- attrs
    }
    return(x)
}

#' @rdname dfm_select
#' @export
#' @examples 
#' toks <- tokens(c("this contains lots of stopwords",
#'                  "no if, and, or but about it: lots"),
#'                remove_punct = TRUE)
#' tmpfcm <- fcm(toks)
#' tmpfcm
#' fcm_remove(tmpfcm, stopwords("english"))
fcm_select <- function(x, pattern = NULL, selection = c("keep", "remove"), 
                       valuetype = c("glob", "regex", "fixed"),
                       case_insensitive = TRUE,
                       verbose = TRUE, ...) {
    UseMethod("fcm_select")
}

#' @noRd
#' @export
fcm_select.fcm <- function(x, pattern = NULL, selection = c("keep", "remove"), 
                           valuetype = c("glob", "regex", "fixed"),
                           case_insensitive = TRUE,
                           verbose = TRUE, ...) {
    
    attrs <- attributes(x)
    x <- t(dfm_select(x, pattern, selection, valuetype, case_insensitive, verbose = verbose, ...))
    x <- t(dfm_select(x, pattern, selection, valuetype, case_insensitive, verbose = FALSE, ...))
    result <- new("fcm", as(x, 'dgCMatrix'), count = attrs$count,
                  context = attrs$context, window = attrs$window, weights = attrs$weights, tri = attrs$tri)
    names(result@Dimnames) <- c("features", "features")
    return(result)
}
    

#' @rdname dfm_select
#' @export
fcm_remove <- function(x, pattern = NULL, ...) {
    UseMethod("fcm_remove")
}

#' @noRd
#' @export
fcm_remove.fcm <- function(x, pattern = NULL, ...) {
    fcm_select(x, pattern, selection = "remove", ...)
}

#' @rdname dfm_select
#' @export
fcm_keep <- function(x, pattern = NULL, ...) {
    UseMethod("fcm_keep")
}

#' @noRd
#' @export
fcm_keep.fcm <- function(x, pattern = NULL, ...) {
    fcm_select(x, pattern, selection = "keep", ...)
}
