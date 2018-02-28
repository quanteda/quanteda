#' @importFrom magrittr %>%
#' @export
magrittr::`%>%`


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
             format(ndocs, big.mark=",", scientific = FALSE),
             " document", if (ndocs != 1L) "s" else "",
             sep = "")
    }
    if ((nfeatspad + ndocspad) > 0) {
        catm(", padded ", sep = "")
    }
    if (nfeatspad > 0) {
        catm(format(nfeatspad, big.mark=",", scientific = FALSE), 
             " feature", if (nfeatspad != 1L) "s" else "",
             sep = "")
    }
    if (ndocspad > 0) {
        if (nfeatspad > 0) catm(" and ", sep = "")
        catm(format(ndocspad, big.mark=",", scientific = FALSE), 
             " document", if (ndocspad != 1L) "s" else "",
             sep = "")
    }
    catm("", appendLF = TRUE)
}

##
## reassign the slots to an S4 dfm-like object
## necessary when some operation from the Matrix class obliterates them
## Ken B
reassign_slots <- function(x_new, x_org, exceptions = NULL) {
    snames <- slotNames(class(x_org))
    snames <- setdiff(snames, c("Dim", "Dimnames", "i", "p", "x", "factors", exceptions))
    for (sname in snames) {
        try({
            slot(x_new, sname) <- slot(x_org, sname)
        }, silent = TRUE)
    }
    x_new
}


#' Function extending base::attributes()
#' @param x an object
#' @param overwrite if \code{TRUE}, overwrite old attributes
#' @param value new attributes
#' @keywords internal
"attributes<-" <- function(x, overwrite = TRUE, value) {
    if (overwrite) {
        base::attributes(x) <- value
    } else {
        base::attributes(x) <- c(base::attributes(x), value[!(names(value) %in% names(base::attributes(x)))])
    }
    return(x)
}

#' Function to assign multiple slots to a S4 object
#' @param x an S4 object
#' @param exceptions slots to ignore
#' @param value a list of attributes extracted by attributes()
#' @keywords internal
"slots<-" <- function(x, exceptions = c("Dim", "Dimnames", "i", "p", "x", "factors"), value) {
    slots <- methods::getSlots(class(x)[1])
    for (sname in names(value)) {
        if (!sname %in% names(slots) || sname %in% exceptions) next
        if (!identical(typeof(value[[sname]]), slots[[sname]])) next
        methods::slot(x, sname) <- value[[sname]]
    }
    return(x)
}

#' Utility function to create a object with new set of attributes
#' @param x an underlying R object of a new object
#' @param attrs attributes of a new object
#' @param overwrite_attributes overwrite attributes of the input object, if \code{TRUE}
#' @keywords internal
create <- function(x, what, attrs = NULL, overwrite_attributes = FALSE, ...) {
    if (what == 'tokens') {
        class <- c('tokens', 'list')
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
#' @param case_insensitive ignore the case of dictionary values if \code{TRUE}
#' @param concatenator concatenator that join multi-word expression in tokens object
#' @param remove_unigram ignore single-word patterns if \code{TRUE}
#' @seealso regex2id
#' @keywords internal
pattern2id <- function(pattern, types, valuetype, case_insensitive, 
                       concatenator = '_', remove_unigram = FALSE) {
    
    if (is.dfm(pattern)) 
        stop('dfm cannot be used as pattern')
    
    if (is.collocations(pattern)) {
        if (nrow(pattern) == 0) return(list())
        pattern <- stri_split_charclass(pattern$collocation, "\\p{Z}")
        pattern_id <- lapply(pattern, function(x) fastmatch::fmatch(x, types))
        pattern_id <- pattern_id[vapply(pattern_id, function(x) all(!is.na(x)), logical(1))]
    } else {
        if (length(pattern) == 0) return(list())
        if (is.dictionary(pattern)) {
            pattern <- unlist(pattern, use.names = FALSE)
            pattern <- split_dictionary_values(pattern, concatenator)
        } else {
            pattern <- as.list(pattern)
        }
        if (remove_unigram)
            pattern <- pattern[lengths(pattern) > 1] # drop single-word pattern
        pattern_id <- regex2id(pattern, types, valuetype, case_insensitive)
    }
    attr(pattern_id, 'pattern') <- stri_c_list(pattern, sep = ' ')
    return(pattern_id)
}


#' Internal function for \code{select_types()} to check if a string is a regular expression
#' @param x a character string to be tested
#' @keywords internal
is_regex <- function(x){
    any(stri_detect_fixed(x, c(".", "(", ")", "^", "{", "}", "+", "$", "*", "?", "[", "]", "\\")))
}

#' Internal function for \code{select_types()} to escape regular expressions 
#' 
#' This function escapes glob patterns before \code{utils:glob2rx()}, therefore * and ?
#' are unescaped.
#' @param x character vector to be escaped
#' @keywords internal
escape_regex <- function(x){
    #stri_replace_all_regex(x, "([.()^\\{\\}+$*\\[\\]\\\\])", "\\\\$1") # escape any
    stri_replace_all_regex(x, "([.()^\\{\\}+$\\[\\]\\\\])", "\\\\$1") # allow glob
}

# function to check dots arguments against a list of permissible arguments
check_dots <-  function(dots, permissible_args = NULL) {
    if (length(dots) == 0) return()
    args <- names(dots)
    impermissible_args <-  setdiff(args, permissible_args)
    if (length(impermissible_args))
        warning("Argument", if (length(impermissible_args) > 1) "s " else " ", 
                paste(impermissible_args, collapse = ', '), " not used.", 
                noBreaks. = TRUE, call. = FALSE)
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
#' #     stop(friendly_class_undefined_message(class(x), "as.tokens"))
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
        msg <- paste0(font, ' is not found on your system.')
        if (.Platform$OS.type == 'windows') {
            if (!font %in% names(grDevices::windowsFonts()))
                stop(msg, ' Run extrafont::import_font() and extrafont::loadfonts(device = "win") to use custom fonts.')
        } else {
            if (!font %in% c('sans', 'serif', 'mono', extrafont::fonts()))
                stop(msg, ' Run extrafont::import_font() to use custom fonts.')
        }
    }
    return(font)
}
