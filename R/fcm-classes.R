#' Virtual class "fcm" for a feature co-occurrence matrix
#'
#' The fcm class of object is a special type of [fcm] object with
#' additional slots, described below.
#' @slot context the context definition
#' @slot window the size of the window, if `context = "window"`
#' @slot count how co-occurrences are counted
#' @slot weights context weighting for distance from target feature, equal in length to `window`
#' @slot margin frequencies of features in the original [dfm] or [tokens]
#' @slot tri whether the lower triangle of the symmetric \eqn{V \times V} matrix is recorded
#' @slot ordered whether a term appears before or after the target feature 
#'      are counted separately
#' @name fcm-class
#' @rdname fcm-class
#' @keywords internal fcm
#' @seealso [fcm]
setClass("fcm",
         slots = c(context = "character", window = "integer", 
                   count = "character", weights = "numeric", 
                   ordered = "logical", tri = "logical",
                   margin = "numeric"),
         contains = c("dfm", "dgCMatrix"))

## S4 method fcm objects
#' @export
#' @param x the fcm object
#' @rdname fcm-class
setMethod("t",
          signature = (x = "fcm"),
          function(x) matrix2fcm(t(as(x, "dgCMatrix"))))

#' @param e1 first quantity in "+" operation for fcm
#' @param e2 second quantity in "+" operation for fcm
#' @rdname fcm-class
setMethod("Arith", signature(e1 = "fcm", e2 = "numeric"),
          function(e1, e2) {
              switch(.Generic[[1]],
                 `+` = matrix2fcm(as(e1, "dgCMatrix") + e2, get_fcm_slots(e1)),
                 `-` = matrix2fcm(as(e1, "dgCMatrix") - e2, get_fcm_slots(e1)),
                 `*` = matrix2fcm(as(e1, "dgCMatrix") * e2, get_fcm_slots(e1)),
                 `/` = matrix2fcm(as(e1, "dgCMatrix") / e2, get_fcm_slots(e1)),
                 `^` = matrix2fcm(as(e1, "dgCMatrix") ^ e2, get_fcm_slots(e1))
              )
          })
#' @rdname fcm-class
setMethod("Arith", signature(e1 = "numeric", e2 = "fcm"),
          function(e1, e2) {
              switch(.Generic[[1]],
                 `+` = matrix2fcm(e1 + as(e2, "dgCMatrix"), get_fcm_slots(e2)),
                 `-` = matrix2fcm(e1 - as(e2, "dgCMatrix"), get_fcm_slots(e2)),
                 `*` = matrix2fcm(e1 * as(e2, "dgCMatrix"), get_fcm_slots(e2)),
                 `/` = matrix2fcm(e1 / as(e2, "dgCMatrix"), get_fcm_slots(e2)),
                 `^` = matrix2fcm(e1 ^ as(e2, "dgCMatrix"), get_fcm_slots(e2))
              )
          })
