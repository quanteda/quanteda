#' @rdname dfm_compress
#' @note `fcm_compress` works only when the [fcm] was created with a
#' document context.
#' @return `fcm_compress` returns an [fcm] whose features have been
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
    if (x@context != "document")
        stop(message_error("fcm_context"))
    matrix2fcm(group_dfm(x, rownames(x), colnames(x), use_docvars = FALSE), 
               attributes(x))
}

#' Sort an fcm in alphabetical order of the features
#' 
#' Sorts an [fcm] in alphabetical order of the features.
#' 
#' @param x [fcm] object
#' @return A [fcm] object whose features have been alphabetically sorted. 
#'   Differs from [fcm_sort()] in that this function sorts the fcm by
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
    slots <- get_fcm_slots(x)
    x <- as(x, "dgTMatrix")
    x <- x[order(rownames(x)), order(colnames(x))]
    if (slots$tri) {
        swap <- x@i > x@j
        i <- x@i[swap]
        x@i[swap] <- x@j[swap]
        x@j[swap] <- i
    }
    matrix2fcm(x, slots)
}

#' Coercion and checking functions for fcm objects
#' 
#' Convert an eligible input object into a fcm, or check whether an object is a
#' fcm.  Current eligible inputs for coercion to a dfm are: [matrix],
#' (sparse) [Matrix][Matrix::Matrix] and other [fcm] objects.
#' @param x a candidate object for checking or coercion to [dfm]
#' @return `as.fcm` converts an input object into a [fcm].
#' @export
as.fcm <- function(x) {
    UseMethod("as.fcm")
}

#' @export
as.fcm.default <- function(x) {
    stop(friendly_class_undefined_message(class(x), "as.fcm"))
}

#' @noRd
#' @method as.fcm fcm
#' @export
as.fcm.fcm <- function(x) {
    return(x)
}

#' @noRd
#' @method as.fcm matrix
#' @export
as.fcm.matrix <- function(x) {
    if (!identical(rownames(x), colnames(x)))
        stop(message_error("matrix_mismatch"))
    matrix2fcm(x)
}

#' @noRd
#' @method as.fcm Matrix
#' @export
as.fcm.Matrix <- function(x) {
    if (!identical(rownames(x), colnames(x)))
        stop(message_error("matrix_mismatch"))
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
    set_fcm_slots(x) <- slots
    set_fcm_dimnames(x) <- list(rowname, colname)
    return(x)
}

#' Set values to a fcm's S4 slots
#' @param x a fcm 
#' @param exceptions names of slots to be ignored
#' @param value a list of values extracted using `attributes` and to be assigned to slots 
#' @keywords internal
"set_fcm_slots<-" <- function(x, exceptions = NULL, value) {
    if (is.null(value)) return(x)
    sname <- setdiff(slotNames("fcm"), c(slotNames("dgCMatrix"), exceptions))
    for (s in sname) {
        try({
            slot(x, s) <- value[[s]]
        }, silent = TRUE)
    }
    return(x)
}

#' @rdname set_fcm_slots-set
get_fcm_slots <- function(x) {
    sname <- setdiff(slotNames("fcm"), c(slotNames("dgCMatrix")))
    attributes(x)[sname]
}
