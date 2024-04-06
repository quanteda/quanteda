#' Pipe operator
#'
#' See \code{magrittr::\link[magrittr:pipe]{\%>\%}} for details.
#'
#' @name %>%
#' @rdname pipe
#' @keywords internal
#' @export
#' @importFrom magrittr %>%
#' @usage lhs \%>\% rhs
NULL

##
## reassign the slots to an S4 dfm-like object
## necessary when some operation from the Matrix class obliterates them
## Ken B
# reassign_slots <- function(x_new, x_org, exceptions = NULL) {
#     snames <- slotNames(class(x_org))
#     snames <- setdiff(snames, c("Dim", "Dimnames", "i", "p", "x", "factors", exceptions))
#     for (sname in snames) {
#         try({
#             slot(x_new, sname) <- slot(x_org, sname)
#         }, silent = TRUE)
#     }
#     x_new
# }

#' Function extending base::attributes()
#' @param x an object
#' @param overwrite if `TRUE`, overwrite old attributes
#' @param value new attributes
#' @keywords internal
"attributes<-" <- function(x, overwrite = TRUE, value) {
    if (overwrite) {
        base::attributes(x) <- value
    } else {
        base::attributes(x) <- c(base::attributes(x),
                                 value[!(names(value) %in% names(base::attributes(x)))])
    }
    return(x)
}

# Function to assign multiple slots to a S4 object
# param x an S4 object
# param exceptions slots to ignore
# param value a list of attributes extracted by attributes()
# keywords internal
# "slots<-" <- function(x, exceptions = c("Dim", "Dimnames", "i", "p", "x", "factors"), value) {
#     slot <- methods::getSlots(head(class(x)))
#     for (name in names(value)) {
#         if (!name %in% names(slot) || name %in% exceptions) next
#         if (!identical(class(value[[name]]), unname(slot[name]))) next
#         methods::slot(x, name) <- value[[name]]
#     }
#     return(x)
# }

#' Check if a string is a regular expression
#' 
#' Internal function for `select_types()` to check if a string is a regular expression
#' @param x a character string to be tested
#' @keywords internal
is_regex <- function(x) {
    any(stri_detect_fixed(x, c(".", "(", ")", "^", "{", "}", "+", "$", "*", "?", "[", "]", "\\")))
}

#' Internal function for `select_types()` to escape regular expressions
#'
#' This function escapes glob patterns before `utils:glob2rx()`, therefore * and
#' ? are unescaped.
#' @param x character vector to be escaped
#' @keywords internal
escape_regex <- function(x) {
    stri_replace_all_regex(x, "([.()^\\{\\}+$\\[\\]\\\\])", "\\\\$1") # allow glob
}


#' Return an error message
#' @param key type of error message
#' @keywords internal
message_error <- function(key = NULL) {
    msg <- c("dfm_empty" = "dfm must have at least one non-zero value",
             "fcm_empty" = "fcm must have at least one non-zero value",
             "fcm_context" = "fcm must be created with a document context",
             "matrix_mismatch" = "matrix must have the same rownames and colnames",
             "docnames_mismatch" = "docnames must be the same length as x",
             "ndoc_mismatch" = "documents must be the same length as x",
             "docvars_mismatch" = "data.frame must have the same number of rows as documents",
             "docvars_invalid" = "document variables cannot begin with the underscore",
             "docvar_nofield" = "you must supply field name(s)",
             "docvar_nocolname" = "data.frame must have column names")
    if (is.null(key) || !key %in% names(msg)) {
        return("")
    }
    return(unname(msg[key]))
}

#' Sample a vector
#'
#' Return a sample from a vector within a grouping variable if specified.
#' @param x numeric vector
#' @param size the number of items to sample within each group, as a positive
#'   number or a vector of numbers equal in length to the number of groups. If
#'   `NULL`, the sampling is stratified by group in the original group
#'   sizes.
#' @param replace if `TRUE`, sample with replacement
#' @param prob a vector of probability weights for values in `x`
#' @param by a grouping vector equal in length to `length(x)`
#' @return `x` resampled within groups
#' @keywords internal
#' @examples
#' set.seed(100)
#' grvec <- c(rep("a", 3), rep("b", 4), rep("c", 3))
#' quanteda:::resample(1:10, replace = FALSE, by = grvec)
#' quanteda:::resample(1:10, replace = TRUE, by = grvec)
#' quanteda:::resample(1:10, size = 2, replace = TRUE, by = grvec)
#' quanteda:::resample(1:10, size = c(1, 1, 3), replace = TRUE, by = grvec)
resample <- function(x, size = NULL, replace = FALSE, prob = NULL, by = NULL) {
    
    x <- check_integer(x, min_len = 0, max_len = Inf)
    replace <- check_logical(replace)
    
    if (is.null(by)) {
        if (!is.null(size)) {
            size <- check_integer(size, max_len = Inf, min = 0)
        } else {
            size <- length(x)
        }
        if (size > length(x) && !replace)
            stop("size cannot exceed the number of items when replace = FALSE", call. = FALSE)
        result <- x[sample.int(length(x), size = size, replace = replace, prob = prob)]
    } else {
        if (!is.null(prob)) 
            stop("prob cannot be used with by", call. = FALSE)
        if (length(x) != length(by))
            stop("x and by must have the same length", call. = FALSE)
        x <- split(x, by)
        if (!is.null(size)) {
            size <- check_integer(size, max_len = Inf, min = 0)
        } else {
            size <- lengths(x)
        }
        if (length(size) > 1 && length(size) != length(x))
            stop("size and by must have the same length", call. = FALSE)
        temp <- mapply(function(x, size, replace) {
                     if (size > length(x) && !replace)
                        stop("size cannot exceed the number of items within group when replace = FALSE", call. = FALSE)
                     x[sample.int(length(x), size = size, replace = replace)]
                }, x, size, replace, SIMPLIFY = FALSE)
        result <- unlist(temp, use.names = FALSE)
    }
    return(result)
}

#' Get the package version that created an object
#'
#' Return the the \pkg{quanteda} package version in which a [dfm],
#' [tokens], or [corpus] object was created.
#' @return A three-element integer vector of class "package_version". For
#'   versions of the package < 1.5 for which no version was recorded in the
#'   object, `c(1, 4, 0)` is returned.
#' @keywords internal utils
get_object_version <- function(x) {
    if (is_pre2(x)) {
        as.package_version("1.4.0")
    } else {
        meta(x, field = "package-version", type = "system")
    }
}

#' @rdname get_object_version
#' @return `ispre2()` returns `TRUE` if the object was created before
#' \pkg{quanteda} version 2, or `FALSE` otherwise
is_pre2 <- function(x) {
    if (is.corpus(x) || is.tokens(x) || (is.dfm(x) && !is.fcm(x))) {
        is.null(attributes(x)[["meta"]][["object"]]) || is.null(docid(x))
    } else {
        is.null(attributes(x)[["meta"]][["object"]])
    }
}

# internal function to rbind data.frames that have different columns
rbind_fill <- function(x, y) {
    name1 <- names(x)
    name2 <- names(y)
    if (!identical(name1, name2)) {
        name <- union(name1, name2)
        name1_missing <- setdiff(name, name1)
        if (length(name1_missing)) {
            # generate NA with appropriate class
            fill1 <- lapply(name1_missing, function(m) y[[m]][0][seq_len(nrow(x))])
            names(fill1) <- name1_missing
            x <- cbind(x, fill1)
        }

        name2_missing <- setdiff(name, name2)
        if (length(name2_missing)) {
            # generate NA with appropriate class
            fill2 <- lapply(name2_missing, function(m) x[[m]][0][seq_len(nrow(y))])
            names(fill2) <- name2_missing
            y <- cbind(y, fill2)
        }
    }
    return(rbind(x, y))
}

#' Get information on TBB library
#' @keywords internal
#' @export
info_tbb <- function() {
    list("enabled" = cpp_tbb_enabled(),
         "max_threads" = cpp_get_max_thread())
}
