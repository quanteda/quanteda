check_integer <- function(x, len_min = 1, len_max = 1, min = -Inf, max = Inf) {
    arg <- deparse(substitute(x))
    fun <- function(e) stop(arg, " must be coercible to integer", call. = FALSE)
    tryCatch({
        x <- as.integer(x)
    }, warning = fun, error = fun)
    x <- check_length(x, len_min, len_max, arg)
    x <- check_na(x, arg)
    x <- check_range(x, min, max, arg)
    return(x)
}

check_double <- function(x, len_min = 1, len_max = 1, min = -Inf, max = Inf) {
    arg <- deparse(substitute(x))
    fun <- function(e) stop(arg, " must be coercible to double", call. = FALSE)
    tryCatch({
        x <- as.double(x)
    }, warning = fun, error = fun)
    x <- check_length(x, len_min, len_max, arg)
    x <- check_na(x, arg)
    x <- check_range(x, min, max, arg)
    return(x)
}

check_logical <- function(x, len_min = 1, len_max = 1, min = -Inf, max = Inf) {
    arg <- deparse(substitute(x))
    fun <- function(e) stop(arg, " must be coercible to logical", call. = FALSE)
    tryCatch({
        x <- as.logical(x)
    }, warning = fun, error = fun)
    x <- check_length(x, len_min, len_max, arg)
    x <- check_na(x, arg)
    x <- check_range(x, min, max, arg)
    return(x)
}

check_character <- function(x, len_min = 1, len_max = 1, nchar_min = 0, nchar_max = Inf) {
    arg <- deparse(substitute(x))
    fun <- function(e) stop(arg, " must be coercible to character", call. = FALSE)
    tryCatch({
        x <- as.character(x)
    }, warning = fun, error = fun)
    x <- check_length(x, len_min, len_max, arg)
    x <- check_na(x, arg)
    n <- stringi::stri_length(x)
    if (any(n < nchar_min) || any(nchar_max < n)) {
        if (nchar_min == nchar_max) {
            stop("The value of ", arg, " must be ", nchar_max, " character\n", call. = FALSE)    
        } else {
            stop("The value of ", arg, " must be between " , nchar_min, " and ", nchar_max, " character\n", call. = FALSE)    
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

check_length <- function(x, len_min, len_max, arg) {
    len <- length(x)
    if (len < len_min || len_max < len) {
        if (len_min == len_max) {
            stop("The length of ", arg, " must be ", len_max, "\n", call. = FALSE)
        } else {
            stop("The length of ", arg, " must be between " , len_min, " and ", len_max, "\n", call. = FALSE)    
        }
    }
    return(x)
}

