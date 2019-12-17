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
    slots <- get_fcm_slots(x)
    x <- t(dfm_select(x, pattern, selection, valuetype, 
                      case_insensitive, verbose = verbose, ...))
    x <- t(dfm_select(x, pattern, selection, valuetype, 
                      case_insensitive, verbose = FALSE, ...))
    matrix2fcm(x, slots)
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
