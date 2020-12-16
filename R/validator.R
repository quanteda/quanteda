#' Validate input vectors 
#' 
#' Check the range of values and the length of input vectors
#' before used in control flow or passed to C++ functions.
#' @param min_len minimum length of the vector
#' @param max_len maximum length of the vector
#' @param min minimum value in the vector
#' @param max maximum value in the vector
#' @keywords internal
check_integer <- function(x, min_len = 1, max_len = 1, min = -Inf, max = Inf) {
    arg <- deparse(substitute(x))
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
check_double <- function(x, min_len = 1, max_len = 1, min = -Inf, max = Inf) {
    arg <- deparse(substitute(x))
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
check_logical <- function(x, min_len = 1, max_len = 1) {
    arg <- deparse(substitute(x))
    fun <- function(e) stop(arg, " must be coercible to logical", call. = FALSE)
    tryCatch({
        x <- as.logical(x)
    }, warning = fun, error = fun)
    x <- check_length(x, min_len, max_len, arg)
    x <- check_na(x, arg)
    return(x)
}

#' @param min_nchar minimum character length of values in the vector
#' @param max_nchar maximum character length of values in the vector
#' @rdname check_integer
check_character <- function(x, min_len = 1, max_len = 1, min_nchar = 0, max_nchar = Inf) {
    arg <- deparse(substitute(x))
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

