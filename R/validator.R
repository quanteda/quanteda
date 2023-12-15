#' Validate input vectors 
#' 
#' Check the range of values and the length of input vectors
#' before used in control flow or passed to C++ functions.
#' @param min_len minimum length of the vector
#' @param max_len maximum length of the vector
#' @param min minimum value in the vector
#' @param max maximum value in the vector
#' @param strict raise error when `x` is a different type
#' @param allow_null if `TRUE`, returns `NULL` when `is.null(x)`
#' @details Note that value checks are performed after coercion to expected input types. 
#' @keywords internal development
#' @export
#' @examples 
#' \dontrun{
#' check_integer(0, min = 1) # error
#' check_integer(-0.1, min = 0) # return 0
#' check_double(-0.1, min = 0) # error
#' check_double(numeric(), min_len = 0) # return numeric()
#' check_double("1.1", min = 1) # returns 1.1
#' check_double("1.1", min = 1, strict = TRUE) # error
#' check_double("xyz", min = 1) # error
#' check_logical(c(TRUE, FALSE), min_len = 3) # error
#' check_character("_", min_nchar = 1) # return "_"
#' check_character("", min_nchar = 1) # error
#' }
check_integer <- function(x, min_len = 1, max_len = 1, min = -Inf, max = Inf, 
                          strict = FALSE, allow_null = FALSE) {
    arg <- deparse(substitute(x))
    if (allow_null) {
        if (is.null(x)) return(NULL)
    } else {
        x <- check_null(x, arg)
    }
    if (strict && !is.integer(x))
        stop("The type of ", arg, " must be integer", call. = FALSE)
    fun <- function(e) stop(arg, " must be coercible to integer", call. = FALSE)
    tryCatch({
        x <- as.integer(x)
    }, warning = fun, error = fun)
    x <- check_length(x, min_len, max_len, arg)
    x <- check_na(x, arg)
    x <- check_range(x, min, max, arg)
    return(x)
}

#' @rdname check_integer
#' @export
check_double <- function(x, min_len = 1, max_len = 1, min = -Inf, max = Inf, 
                         strict = FALSE, allow_null = FALSE) {
    arg <- deparse(substitute(x))
    if (allow_null) {
        if (is.null(x)) return(NULL)
    } else {
        x <- check_null(x, arg)
    }
    if (strict && !is.double(x))
        stop("The type of ", arg, " must be double", call. = FALSE)
    fun <- function(e) stop(arg, " must be coercible to double", call. = FALSE)
    tryCatch({
        x <- as.double(x)
    }, warning = fun, error = fun)
    x <- check_length(x, min_len, max_len, arg)
    x <- check_na(x, arg)
    x <- check_range(x, min, max, arg)
    return(x)
}

#' @rdname check_integer
#' @param allow_na if `TRUE`, convert `NA` to `FALSE`
#' @export
check_logical <- function(x, min_len = 1, max_len = 1, strict = FALSE, 
                          allow_null = FALSE, allow_na = FALSE) {
    arg <- deparse(substitute(x))
    if (allow_null) {
        if (is.null(x)) return(NULL)
    } else {
        x <- check_null(x, arg)
    }
    if (strict && !is.logical(x))
        stop("The type of ", arg, " must be logical", call. = FALSE)
    fun <- function(e) stop(arg, " must be coercible to logical", call. = FALSE)
    tryCatch({
        x <- as.logical(x)
    }, warning = fun, error = fun)
    x <- check_length(x, min_len, max_len, arg)
    if (allow_na) {
        x[is.na(x)] <- FALSE
    } else {
        x <- check_na(x, arg)    
    }
    return(x)
}

#' @param min_nchar minimum character length of values in the vector
#' @param max_nchar maximum character length of values in the vector
#' @rdname check_integer
#' @export
check_character <- function(x, min_len = 1, max_len = 1, min_nchar = 0, max_nchar = Inf, 
                            strict = FALSE, allow_null = FALSE) {
    arg <- deparse(substitute(x))
    if (allow_null) {
        if (is.null(x)) return(NULL)
    } else {
        x <- check_null(x, arg)
    }
    if (strict && !is.character(x))
        stop("The type of ", arg, " must be character", call. = FALSE)
    fun <- function(e) stop(arg, " must be coercible to character", call. = FALSE)
    tryCatch({
        x <- as.character(x)
    }, warning = fun, error = fun)
    x <- check_length(x, min_len, max_len, arg)
    x <- check_na(x, arg)
    n <- stringi::stri_length(x)
    if (any(n < min_nchar) || any(max_nchar < n)) {
        if (min_nchar == max_nchar) {
            stop("The value of ", arg, " must be ", max_nchar, " character\n", call. = FALSE)    
        } else {
            stop("The value of ", arg, " must be between " , min_nchar, " and ", max_nchar, " character\n", call. = FALSE)    
        }
    }
    return(x)
}

check_na <- function(x, arg) {
    if (any(is.na(x)))
        stop("The value of ", arg, " cannot be NA\n", call. = FALSE)
    return(x)
}

check_null <- function(x, arg) {
    if (is.null(x))
        stop(arg, " cannot be NULL\n", call. = FALSE)
    return(x)
}

check_range <- function(x, min, max, arg) {
    if (any(x < min) || any(max < x))
        stop("The value of ", arg, " must be between " , min, " and ", max, "\n", call. = FALSE)
    return(x)
}

check_length <- function(x, min_len, max_len, arg) {
    len <- length(x)
    if (len < min_len || max_len < len) {
        if (min_len == max_len) {
            stop("The length of ", arg, " must be ", max_len, "\n", call. = FALSE)
        } else {
            stop("The length of ", arg, " must be between " , min_len, " and ", max_len, "\n", call. = FALSE)    
        }
    }
    return(x)
}


#' Check object class for functions
#'
#' Checks if the method is defined for the class.
#' @param class the object class to check
#' @param method the name of functions to be called
#' @keywords internal development
#' @examples
#' \dontrun{
#' quanteda:::check_class("tokens", "dfm_select")
#' }
check_class <- function(class, method, defunct_methods = NULL) {
    class_valid <- rownames(attr(utils::methods(method), "info"))
    class_valid <- stringi::stri_replace_first_fixed(class_valid, paste0(method, "."), "")
    class_valid <- class_valid[! class_valid %in% c("default", defunct_methods)]
    if (!length(intersect(class, class_valid)))
        stop(method, "() only works on ", paste(class_valid, collapse = ", "), " objects.", call. = FALSE)
}
friendly_class_undefined_message <- check_class # for compatibility

#' Check arguments passed to other functions via ...
#' @param ... dots to check
#' @param method the names of functions `...` is passed to
#' @keywords internal development
check_dots <- function(..., method = NULL) {
    arg <- setdiff(names(list(...)), "")
    if (!is.null(method)) {
        arg_used <- unlist(lapply(method, function(x) names(formals(x))))
        arg <- setdiff(arg, arg_used)
    }
    if (length(arg) > 1) {
        warning(paste0(arg, collapse = ", "), " arguments are not used.", call. = FALSE)
    } else if (length(arg) == 1) {
        warning(arg, " argument is not used.", call. = FALSE)
    }
}
unused_dots <- check_dots # for compatibility
