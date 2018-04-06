#' @param i index for documents
#' @param j index for features
#' @param drop always set to \code{FALSE}
#' @param ... additional arguments not used here
#' @rdname dfm-class
#' @export
#' @examples 
#' # dfm subsetting
#' x <- dfm(tokens(c("this contains lots of stopwords",
#'                   "no if, and, or but about it: lots",
#'                   "and a third document is it"),
#'                 remove_punct = TRUE))
#' x[1:2, ]
#' x[1:2, 1:5]
#' 
#' # fcm subsetting
#' y <- fcm(tokens(c("this contains lots of stopwords",
#'                   "no if, and, or but about it: lots"),
#'                 remove_punct = TRUE))
#' y[1:3, ]
#' y[4:5, 1:5]
setMethod("[", 
          signature = c("dfm", i = "index", j = "index", drop = "missing"),
          function(x, i, j, ..., drop = FALSE) {
              attrs <- attributes(x)
              error <- FALSE
              if (is.character(i) && any(!i %in% rownames(x))) error <- TRUE
              if (is.character(j) && any(!j %in% colnames(x))) error <- TRUE
              if (is.numeric(i) && any(i > nrow(x))) error <- TRUE
              if (is.numeric(j) && any(j > ncol(x))) error <- TRUE
              if (error) stop("Subscript out of bounds")

              attrs$docvars <- attrs$docvars[i, , drop = FALSE]
              x <-  "["(as(x, "Matrix"), i, j, ..., drop = FALSE)
              as.dfm(x, attrs)
          })

#' @rdname dfm-class
#' @export
setMethod("[",
          signature = c("dfm", i = "index", j = "index", drop = "logical"),
          function(x, i, j, ..., drop = FALSE) {
              attrs <- attributes(x)
              error <- FALSE
              if (is.character(i) && any(!i %in% rownames(x))) error <- TRUE
              if (is.character(j) && any(!j %in% colnames(x))) error <- TRUE
              if (is.numeric(i) && any(i > nrow(x))) error <- TRUE
              if (is.numeric(j) && any(j > ncol(x))) error <- TRUE
              if (error) stop("Subscript out of bounds")
              
              if (drop) warning("drop = TRUE not supported")
              attrs$docvars <- attrs$docvars[i, , drop = FALSE]
              x <- "["(as(x, "Matrix"), i, j, ..., drop = FALSE)
              as.dfm(x, attrs)
          })

#' @rdname dfm-class
#' @export
setMethod("[",
          signature = c("dfm", i = "missing", j = "missing", drop = "missing"),
          function(x, i, j, ..., drop = FALSE) {
              x
          })

#' @rdname dfm-class
#' @export
setMethod("[",
          signature = c("dfm", i = "missing", j = "missing", drop = "logical"),
          function(x, i, j, ..., drop = FALSE) {
              if (drop) warning("drop = TRUE not supported")
              x
          })

#' @rdname dfm-class
#' @export
setMethod("[",
          signature = c("dfm", i = "index", j = "missing", drop = "missing"),
          function(x, i, j, ..., drop = FALSE) {
              attrs <- attributes(x)
              error <- FALSE
              if (is.character(i) && any(!i %in% rownames(x))) error <- TRUE
              if (is.numeric(i) && any(i > nrow(x))) error <- TRUE
              if (error) stop("Subscript out of bounds")
              
              attrs$docvars <- attrs$docvars[i, , drop = FALSE]
              x <- "["(as(x, "Matrix"), i, , ..., drop = FALSE)
              as.dfm(x, attrs)
          })

#' @rdname dfm-class
#' @export
setMethod("[",
          signature = c("dfm", i = "index", j = "missing", drop = "logical"),
          function(x, i, j, ..., drop = FALSE) {
              attrs <- attributes(x)
              error <- FALSE
              if (is.character(i) && any(!i %in% rownames(x))) error <- TRUE
              if (is.numeric(i) && any(i > nrow(x))) error <- TRUE
              if (error) stop("Subscript out of bounds")
              
              if (drop) warning("drop = TRUE not supported")
              attrs$docvars <- attrs$docvars[i, , drop = FALSE]
              x <- "["(as(x, "Matrix"), i, , ..., drop = FALSE)
              as.dfm(x, attrs)
          })

#' @rdname dfm-class
#' @export
setMethod("[",
          signature = c("dfm", i = "missing", j = "index", drop = "missing"),
          function(x, i, j, ..., drop = FALSE) {
              attrs <- attributes(x)
              error <- FALSE
              if (is.character(j) && any(!j %in% colnames(x))) error <- TRUE
              if (is.numeric(j) && any(j > ncol(x))) error <- TRUE
              if (error) stop("Subscript out of bounds")
              
              x <- "["(as(x, "Matrix"), , j, ..., drop = FALSE)
              as.dfm(x, attrs)
          })

#' @rdname dfm-class
#' @export
setMethod("[",
          signature = c("dfm", i = "missing", j = "index", drop = "logical"),
          function(x, i, j, ..., drop = FALSE) {
              attrs <- attributes(x)
              error <- FALSE
              if (is.character(j) && any(!j %in% colnames(x))) error <- TRUE
              if (is.numeric(j) && any(j > ncol(x))) error <- TRUE
              if (error) stop("Subscript out of bounds")
              
              if (drop) warning("drop = TRUE not supported")
              x <- "["(as(x, "Matrix"), , j, ..., drop = FALSE)
              as.dfm(x, attrs)
          })
