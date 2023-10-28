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
    check_class(class(x), "fcm_compress")
}

#' @export
fcm_compress.fcm <- function(x) {
    attrs <- attributes(x)
    if (field_object(attrs, "context") != "document")
        stop(message_error("fcm_context"))
    x <- group_matrix(x, rownames(x), colnames(x))
    build_fcm(x, colnames(x), meta = attrs[["meta"]])
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
    check_class(class(x), "fcm_sort")
}

#' @export
fcm_sort.fcm <- function(x) {
    x <- as.fcm(x)
    attrs <- attributes(x)
    x <- as(x, "TsparseMatrix")
    x <- x[order(rownames(x)), order(colnames(x))]
    if (field_object(attrs, "tri")) {
        swap <- x@i > x@j
        i <- x@i[swap]
        x@i[swap] <- x@j[swap]
        x@j[swap] <- i
    }
    build_fcm(x, colnames(x), meta = attrs[["meta"]])
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
    check_class(class(x), "as.fcm")
}

#' @noRd
#' @method as.fcm fcm
#' @export
as.fcm.fcm <- function(x) {
    upgrade_fcm(x)
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
#' @keywords internal
matrix2fcm <- function(x, meta = NULL) {
    rowname <- rownames(x)
    if (nrow(x) > length(rowname))
        rowname <- paste0(quanteda_options("base_featname"), seq_len(nrow(x)))

    colname <- colnames(x)
    if (ncol(x) > length(colname))
        colname <- paste0(quanteda_options("base_featname"), seq_len(ncol(x)))

    if (is.null(meta))
        meta <- make_meta("fcm")

    build_fcm(x, rowname, colname, meta = meta)
}
