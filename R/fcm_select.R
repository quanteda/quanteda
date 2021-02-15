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
    check_class(class(x), "fcm_select")
}

#' @export
fcm_select.fcm <- function(x, pattern = NULL, 
                           selection = c("keep", "remove"), 
                           valuetype = c("glob", "regex", "fixed"),
                           case_insensitive = TRUE,
                           verbose = quanteda_options("verbose"), ...) {
    
    x <- as.fcm(x)
    attrs <- attributes(x)
    x <- t(dfm_select(x, pattern, selection, valuetype, 
                      case_insensitive, verbose = verbose, ...))
    x <- t(dfm_select(x, pattern, selection, valuetype, 
                      case_insensitive, verbose = FALSE, ...))
    build_fcm(x, colnames(x), meta = attrs[["meta"]])
}

#' @rdname dfm_select
#' @export
fcm_remove <- function(x, ...) {
    if ("selection" %in% names(list(...))) {
        stop("fcm_keep cannot include selection argument")
    }
    fcm_select(x, selection = "remove", ...)
}

#' @rdname dfm_select
#' @export
fcm_keep <- function(x, ...) {
    if ("selection" %in% names(list(...))) {
        stop("fcm_keep cannot include selection argument")
    }
    fcm_select(x, ..., selection = "keep")
}
