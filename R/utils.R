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

# rdname catm
# messages() with some of the same syntax as cat(): takes a sep argument and
# does not append a newline by default
catm <- function(..., sep = " ", appendLF = FALSE) {
    message(paste(..., sep = sep), appendLF = appendLF)
}

# used in displaying verbose messages for tokens_select and dfm_select
message_select <- function(selection, nfeats, ndocs, nfeatspad = 0, ndocspad = 0) {
    catm(if (selection == "keep") "kept" else "removed", " ",
         format(nfeats, big.mark = ",", scientific = FALSE),
         " feature", if (nfeats != 1L) "s" else "", sep = "")
    if (ndocs > 0) {
        catm(" and ",
             format(ndocs, big.mark = ",", scientific = FALSE),
             " document", if (ndocs != 1L) "s" else "",
             sep = "")
    }
    if ((nfeatspad + ndocspad) > 0) {
        catm(", padded ", sep = "")
    }
    if (nfeatspad > 0) {
        catm(format(nfeatspad, big.mark = ",", scientific = FALSE),
             " feature", if (nfeatspad != 1L) "s" else "",
             sep = "")
    }
    if (ndocspad > 0) {
        if (nfeatspad > 0) catm(" and ", sep = "")
        catm(format(ndocspad, big.mark = ",", scientific = FALSE),
             " document", if (ndocspad != 1L) "s" else "",
             sep = "")
    }
    catm("", appendLF = TRUE)
}

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

#' Utility function to create a object with new set of attributes
#' @param x an underlying R object of a new object
#' @param attrs attributes of a new object
#' @param overwrite_attributes overwrite attributes of the input object, if `TRUE`
#' @keywords internal
create <- function(x, what, attrs = NULL, overwrite_attributes = FALSE, ...) {
    if (what == "tokens") {
        class <- c("tokens", "list")
    }
    x <- structure(x, class = class, ...)
    if (!is.null(attrs)) {
        attributes(x, overwrite_attributes) <- attrs
    }
    return(x)
}

#' Convert various input as pattern to a vector used in tokens_select,
#' tokens_compound and kwic.
#' @inheritParams pattern
#' @inheritParams valuetype
#' @param concatenator concatenator that join multi-word expression in tokens object
#' @param levels only used when pattern is a dictionary
#' @param remove_unigram ignore single-word patterns if `TRUE`
#' @seealso [pattern2id()]
#' @keywords internal
pattern2list <- function(x, types, valuetype, case_insensitive,
                         concatenator = "_", levels = 1, remove_unigram = FALSE,
                         keep_nomatch = FALSE) {

    if (is.dfm(x))
        stop("dfm cannot be used as pattern")

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

#' Print friendly object class not defined message
#'
#' Checks valid methods and issues a friendlier error message in case the method is
#' undefined for the supplied object type.
#' @param object_class character describing the object class
#' @param function_name character which is the function name
#' @keywords internal
#' @examples
#' # as.tokens.default <- function(x, concatenator = "", ...) {
#' #     stop(quanteda:::friendly_class_undefined_message(class(x), "as.tokens"))
#' # }
friendly_class_undefined_message <- function(object_class, function_name) {
    valid_object_types <-
        utils::methods(function_name) %>%
        as.character() %>%
        stringi::stri_replace_first_fixed(paste0(function_name, "."), "")
    valid_object_types <- valid_object_types[valid_object_types != "default"]
    paste0(function_name, "() only works on ",
         paste(valid_object_types, collapse = ", "),
         " objects.")
}

#' Check if font is available on the system
#'
#' This function checks if custom font is available to \pkg{ggplot} and
#' \pkg{graphics} APIs.
#' @param font name of a font to be checked if available on the system.
#' @return character string
#' @keywords internal
check_font <- function(font) {

    if (is.null(font)) {
        font <- ""
    } else {
        msg <- paste0(font, " is not found on your system.")
        if (.Platform$OS.type == "windows") {
            if (!font %in% names(grDevices::windowsFonts()))
                stop(msg, " Run extrafont::font_import() and ",
                     "extrafont::loadfonts(device = \"win\") to use custom fonts.")
        } else {
            if (!font %in% c("sans", "serif", "mono", extrafont::fonts()))
                stop(msg, " Run extrafont::font_import() to use custom fonts.")
        }
    }
    return(font)
}

#' Raise warning of unused dots
#' @param ... dots to check
#' @keywords internal
unused_dots <- function(...) {
    arg <- names(list(...))
    if (length(arg) == 1) {
        warning(arg[1], " argument is not used.", call. = FALSE)
    } else if (length(arg) > 1) {
        warning(paste0(arg, collapse = ", "), " arguments are not used.", call. = FALSE)
    }
}

# function to check dots arguments against a list of permissible arguments
# needed for tokens.R only
# because (...) evaluated in parent fn is different from being passed through
check_dots <-  function(dots, permissible_args = NULL) {
    if (length(dots) == 0) return()
    args <- names(dots)
    arg <-  setdiff(args, permissible_args)
    if (length(arg) == 1) {
        warning(arg[1], " argument is not used.", call. = FALSE)
    } else if (length(arg) > 1) {
        warning(paste0(arg, collapse = ", "), " arguments are not used.", call. = FALSE)
    }
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
             "docvars_mismatch" = "data.frame must have the same number of rows as documents",
             "docvars_invalid" = "document variables cannot begin with the underscore",
             "docvar_nofield" = "you must supply field name(s)",
             "docvar_nocolname" = "data.frame must have column names")
    if (is.null(key) || !key %in% names(msg)) {
        return("")
    }
    return(unname(msg[key]))
}

#' Sample a vector by a group
#'
#' Return a sample from a vector within a grouping variable.
#' @param x any vector
#' @param size the number of items to sample within each group, as a positive
#'   number or a vector of numbers equal in length to the number of groups. If
#'   `NULL`, the sampling is stratified by group in the original group
#'   sizes.
#' @param group a grouping vector equal in length to `length(x)`
#' @param replace logical; should sampling be with replacement?
#' @return `x` resampled within groups
#' @keywords internal
#' @examples
#' set.seed(100)
#' grvec <- c(rep("a", 3), rep("b", 4), rep("c", 3))
#' quanteda:::sample_bygroup(1:10, group = grvec, replace = FALSE)
#' quanteda:::sample_bygroup(1:10, group = grvec, replace = TRUE)
#' quanteda:::sample_bygroup(1:10, group = grvec, size = 2, replace = TRUE)
#' quanteda:::sample_bygroup(1:10, group = grvec, size = c(1, 1, 3), replace = TRUE)
sample_bygroup <- function(x, group, size = NULL, replace = FALSE) {
    if (length(x) != length(group))
        stop("group not equal in length of x")
    x <- split(x, group)
    if (is.null(size))
        size <- lengths(x)
    if (length(size) > 1 && length(size) != length(x))
        stop("size not equal in length to the number of groups")
    result <- mapply(function(x, size, replace) {
                 x[sample.int(length(x), size = size, replace = replace)]
              }, x, size, replace, SIMPLIFY = FALSE)
    unlist(result, use.names = FALSE)

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


get_cache <- function(x, field, ...) {
    if (Sys.info()[["sysname"]] == "SunOS") 
        return(NULL)
    meta <- meta(x, type = "all")
    hash <- hash_object(x, ...)
    #print(hash)
    if (identical(meta$object[[field]][["hash"]], hash)) {
        result <- meta$object[[field]][["data"]]
    } else {
        result <- NULL
    }
    return(result)
}

set_cache <- function(x, field, object, ...) {
    if (Sys.info()[["sysname"]] == "SunOS") 
        return()
    meta <- meta(x, type = "all")
    hash <- hash_object(x, ...)
    #print(hash)
    meta$object[[field]] <- list("hash" = hash, "data" = object)
    qatd_cpp_set_meta(x, meta)
}

clear_cache <- function(x, field) {
    if (Sys.info()[["sysname"]] == "SunOS") 
        return()
    meta <- meta(x, type = "all")
    if (field %in% names(meta$object)) {
        meta$object[[field]] <- list()
        qatd_cpp_set_meta(x, meta)
    }
}

hash_object <- function(x, ...) {
    attr(x, "meta") <- NULL
    digest::digest(list(x, utils::packageVersion("quanteda"), ...),
                   algo = "sha256")
}
