
setClassUnion("index_logical_missing", c("index", "logical", "missing"))
#setClassUnion("index_logical_missing", c("index", "logical"))

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
#' x[c(TRUE, TRUE, FALSE),]
#' x[c(TRUE, TRUE, FALSE), c(rep(TRUE, 5), rep(FALSE, 11))]
#' 
#' # fcm subsetting
#' y <- fcm(tokens(c("this contains lots of stopwords",
#'                   "no if, and, or but about it: lots"),
#'                 remove_punct = TRUE))
#' y[1:3, ]
#' y[4:5, 1:5]
setMethod("[",
          signature = c(x = "dfm", i = "index_logical_missing", j = "index_logical_missing", drop = "missing"),
          function(x, i, j, drop) {
              if(missing(i)) i <- TRUE
              if(missing(j)) j <- TRUE
              x_new <-  "["(as(x, "dgCMatrix"), i, j, drop = FALSE)
              reassign_slots(as(x_new, "dfm"), x)
          })

setMethod("[", 
          signature = c("dfm", i = "index_logical_missing", j = "index_logical_missing", drop = "logical"),
          function(x, i, j, drop) {
              if(missing(i)) i <- TRUE
              if(missing(j)) j <- TRUE
              if (drop) warning("drop = TRUE not supported")
              x_new <-  "["(as(x, "dgCMatrix"), i, j, drop = FALSE)
              reassign_slots(as(x_new, "dfm"), x)
          })

