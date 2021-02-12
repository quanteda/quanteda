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

#' Function to assign multiple slots to a S4 object
#' @param x an S4 object
#' @param exceptions slots to ignore
#' @param value a list of attributes extracted by attributes()
#' @keywords internal
# "slots<-" <- function(x, exceptions = c("Dim", "Dimnames", "i", "p", "x", "factors"), value) {
#     slot <- methods::getSlots(head(class(x)))
#     for (name in names(value)) {
#         if (!name %in% names(slot) || name %in% exceptions) next
#         if (!identical(class(value[[name]]), unname(slot[name]))) next
#         methods::slot(x, name) <- value[[name]]
#     }
#     return(x)
# }

#' Convert various inputs as pattern to a vector
#' 
#' Convert various inputs as pattern to a vector.  Used in [tokens_select],
#' [tokens_compound], and [kwic].
#' @inheritParams pattern
#' @inheritParams valuetype
#' @param concatenator concatenator that joins multi-word expressions in a
#'   tokens object
#' @param levels only used when pattern is a dictionary
#' @param remove_unigram ignore single-word patterns if `TRUE`
#' @keywords internal
#' @seealso [pattern2id()]
pattern2list <- function(x, types, valuetype, case_insensitive,
                         concatenator = "_", levels = 1, remove_unigram = FALSE,
                         keep_nomatch = FALSE) {
    
    if (is.dfm(x))
        stop("dfm cannot be used as pattern")
    
    case_insensitive <- check_logical(case_insensitive)
    concatenator <- check_character(concatenator)
    levels <- check_integer(levels, min = 1, max_len = Inf)
    remove_unigram <- check_logical(remove_unigram)
    keep_nomatch <- check_logical(keep_nomatch)
    
    if (is.collocations(x)) {
        if (nrow(x) == 0) return(list())
        temp <- stri_split_charclass(x$collocation, "\\p{Z}")
        names(temp) <- x$collocation
        if (case_insensitive) {
            result <- pattern2id(temp, types, valuetype = "fixed", TRUE)
        } else {
            temp <- lapply(temp, function(x) fastmatch::fmatch(x, types))
            result <- temp[unlist(lapply(temp, function(x) all(!is.na(x))), use.names = FALSE)]
        }
        attr(result, "pattern") <- match(names(result), names(temp))
    } else {
        if (length(x) == 0) return(list())
        if (is.dictionary(x)) {
            x <- as.dictionary(x)
            temp <- flatten_dictionary(x, levels)
            key <- names(temp)
            temp <- split_values(temp, " ", concatenator)
        } else if (is.list(x)) {
            temp <- x
            names(temp) <- stri_c_list(x, " ")
        } else {
            temp <- as.list(x)
            names(temp) <- x
        }
        if (remove_unigram)
            temp <- temp[lengths(temp) > 1] # drop single-word patterns
        result <- pattern2id(temp, types, valuetype, case_insensitive, keep_nomatch)
        attr(result, "pattern") <- match(names(result), names(temp))
        if (is.dictionary(x))
            attr(result, "key") <- key
    }
    return(result)
}

#' Internal function for `select_types()` to check if a string is a regular expression
#' @param x a character string to be tested
#' @keywords internal
is_regex <- function(x) {
    any(stri_detect_fixed(x, c(".", "(", ")", "^", "{", "}", "+", "$", "*", "?", "[", "]", "\\")))
}

#' Internal function for `select_types()` to escape regular expressions
#'
#' This function escapes glob patterns before `utils:glob2rx()`, therefore * and ?
#' are unescaped.
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
             "docnames_mismatch" = "docnames must the the same length as x",
             "ndoc_mismatch" = "documents must the the same length as x",
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
    is.null(attributes(x)[["meta"]][["object"]])
}

# internal function to rbind data.frames that have different columns
rbind_fill <- function(x, y) {
    name1 <- names(x)
    name2 <- names(y)
    if (!identical(name1, name2)) {
        name <- union(name1, name2)
        name1_missing <- setdiff(name, name1)
        if (length(name1_missing)) {
            fill1 <- rep(list(rep(NA, nrow(x))), length(name1_missing))
            names(fill1) <- name1_missing
            x <- cbind(x, fill1)
        }

        name2_missing <- setdiff(name, name2)
        if (length(name2_missing)) {
            fill2 <- rep(list(rep(NA, nrow(y))), length(name2_missing))
            names(fill2) <- name2_missing
            y <- cbind(y, fill2)
        }
    }
    return(rbind(x, y))
}
