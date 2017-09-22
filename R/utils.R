#' @importFrom magrittr %>%
#' @export
magrittr::`%>%`

# rdname catm
# make temporary files and directories in a more reasonable way than tempfile()
# or tempdir(): here, the filename is different each time you call mktemp()
mktemp <- function(prefix='tmp.', base_path = NULL, directory = FALSE) {
    #  Create a randomly-named temporary file or directory, sort of like
    #  https://www.mktemp.org/manual.html
    if (is.null(base_path))
        base_path <- tempdir()
    
    alphanumeric <- c(0:9, LETTERS, letters)
    
    filename <- paste0(sample(alphanumeric, 10, replace=T), collapse='')
    filename <- paste0(prefix, filename)
    filename <- file.path(base_path, filename)
    while (file.exists(filename) || dir.exists(filename)) {
        filename <- paste0(sample(alphanumeric, 10, replace=T), collapse='')
        filename <- paste0(prefix, filename)
        filename <- file.path(base_path, filename)
    }
    
    if (directory) {
        dir.create(filename)
    }
    else {
        file.create(filename)
    }
    
    return(filename)
}


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
reassign_slots <- function(x_new, x_orig, exceptions = NULL) {
    snames <- slotNames(x_orig)
    snames <- snames[!snames %in% 
                         c("Dim", "Dimnames", "i", "p", "x", "factors", exceptions)]
    for (s in snames) {
        slot(x_new, s) <- slot(x_orig, s)
    }
    x_new
}


#' function extending base::attributes()
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

#' function to assign multiple slots to a S4 object
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

#' utility function to remove all attributes
#' @keywords internal
remove_attributes <- function(x) {
    base::attributes(x) <- NULL
    return(x)
}

#' utility function to create a object with new set of attributes
#' @param x an underlying R object of a new object
#' @param attrs attributes of a new object
#' @param overwrite_attributes overwrite attributes of the input object, if \code{TRUE}
#' @keywords internal
create <- function(x, what, attrs = NULL, overwrite_attributes = FALSE, ...) {
    if (what == 'tokens') {
        class <- c('tokens', 'tokenizedTexts', 'list')
    }
    x <- structure(x, class = class, ...)
    if (!is.null(attrs)) {
        attributes(x, overwrite_attributes) <- attrs
    }
    return(x)
}

# This function generates random texts from English alphabets or any other characters.

# @param n_doc the number of documents generated
# @param n_word the number of words in documents
# @param len_word the size of longest words
# @param n_type the number of tyeps of tokens appear in the documents
# @param fast if true, words are generated from uniform random distribution over characters to speed up
# @param code if true, the function return R code instead of vector
# @param seed a integer passed to set.seed() to generate replicable random texts 
# @param characters a vector of letters for random word generation
#
# texts_random(5, 20, seed=1234)
# texts_random(5, 20, seed=1234, code=TRUE)
# texts_random(5, 20, seed=1234, fast=TRUE)
# 
# texts_random(5, 10, seed=1234, characters = LETTERS)

texts_random <- function(n_doc=10, 
                         n_word=100, 
                         len_word=5, 
                         n_type=1000, 
                         fast=FALSE, 
                         code=FALSE,
                         seed, characters){
    
    if (!missing(seed)) set.seed(seed)
    if (missing(characters)){
        # Empirical distribution in English (https://en.wikipedia.org/wiki/Letter_frequency)
        chars <- letters
        prob_chars <-c(0.08167, 0.01492, 0.02782, 0.04253, 0.12702, 0.02228, 0.02015, 
                       0.06094, 0.06966, 0.00153, 0.00772, 0.04025, 0.02406, 0.06749,
                       0.07507, 0.01929, 0.00095, 0.05987, 0.06327, 0.09056, 0.02758, 
                       0.00978, 0.02360, 0.00150, 0.01974, 0.00074)
    } else {
        # Log-normal distribution
        chars <- characters
        dist_chars <- stats::rlnorm(length(chars))
        prob_chars <- sort(dist_chars / sum(dist_chars), decreasing = TRUE)
    }
    if (n_type > length(chars) ^ len_word) 
        stop('n_type is too large')
    
    # Generate unique types
    type <- c()
    if (fast) {
        pat <- stri_flatten(c('[', chars, ']'))
        while(n_type > length(type)){
            type <- unique(c(type, stri_rand_strings(n_type, 1:len_word, pat)))
        }
    } else {
        while(n_type > length(type)){
            type <- unique(c(type, word_random(chars, sample(len_word, 1), prob_chars)))
        }
    }
    type <- head(type, n_type)
    
    # Generate random text from the types
    texts <- c()
    prob_words <- zipf(n_type)
    texts <- replicate(n_doc, {
        words <- sample(type, size=n_word, replace = TRUE, prob=prob_words)
        stri_c(words, collapse = ' ')
    })
    if (code) {
        return(code(texts))
    } else {
        return(texts)
    }
}

word_random <- function(chars, len_word, prob){
    stri_flatten(sample(chars, len_word, replace = TRUE, prob = prob)) 
}

zipf <- function(n_type){
    (1 / 1:n_type) / n_type
}

code <- function(texts){
    len <- length(texts)
    cat(paste0('txt <- c("', texts[1], '",\n'))
    for (text in texts[2:(len-1)]) {
        cat(paste0('         "', text, '",\n'))
    }
    cat(paste0('         "', texts[len], '")\n'))
}


#' convert various input as pattern to a vector used in tokens_select, 
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
    
    if (is.sequences(pattern) || is.collocations(pattern)) {
        pattern <- stri_split_charclass(pattern$collocation, "\\p{Z}")
        pattern_id <- lapply(pattern, function(x) fastmatch::fmatch(x, types))
        pattern_id <- pattern_id[sapply(pattern_id, function(x) all(!is.na(x)))]
    } else {
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


#' internal function for \code{select_types()} to check if a string is a regular expression
#' @param x a character string to be tested
#' @keywords internal
is_regex <- function(x){
    any(stri_detect_fixed(x, c(".", "(", ")", "^", "{", "}", "+", "$", "*", "?", "[", "]", "\\")))
}

#' internal function for \code{select_types()} to escape regular expressions 
#' 
#' This function escapes glob patterns before \code{utils:glob2rx()}, therefore * and ?
#' are unescaped.
#' @param x character vector to be escaped
#' @keywords internal
escape_regex <- function(x){
    #stri_replace_all_regex(x, "([.()^\\{\\}+$*\\[\\]\\\\])", "\\\\$1") # escape any
    stri_replace_all_regex(x, "([.()^\\{\\}+$\\[\\]\\\\])", "\\\\$1") # allow glob
}

# which object class started the call stack?
# @param x sys.calls() from inside a function
# @param function_name the base name of the method of the function
# @examples
# who_called_me_first(sys.calls(), "dfm")
who_called_me_first <- function(x, function_name) {
    x <- as.character(x)
    base_call_index <- which(stringi::stri_startswith_fixed(x, function_name))
    base_call_index <- which(stringi::stri_detect_regex(x, paste0("^(quanteda::){0,1}", function_name, "(\\.\\w+){0,1}\\(")))
    x <- x[base_call_index[-1]]
    x <- stringi::stri_replace_all_regex(x, paste0(function_name, "\\.(\\w+)\\(.+$"), "$1")
    x[1]
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

