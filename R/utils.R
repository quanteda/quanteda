# @rdname catm
# make temporary files and directories in a more reasonable way than tempfile()
# or tempdir(): here, the filename is different each time you call mktemp()
mktemp <- function(prefix = 'tmp.', base_path = NULL, directory = FALSE) {
    #  Create a randomly-named temporary file or directory, sort of like
    #  https://www.mktemp.org/manual.html
    if (is.null(base_path))
        base_path <- tempdir()
    
    alphanumeric <- c(0:9, LETTERS, letters)
    
    filename <- paste0(sample(alphanumeric, 10, replace = TRUE), collapse='')
    filename <- paste0(prefix, filename)
    filename <- file.path(base_path, filename)
    while (file.exists(filename) || dir.exists(filename)) {
        filename <- paste0(sample(alphanumeric, 10, replace = TRUE), collapse='')
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


# @rdname catm
# messages() with some of the same syntax as cat(): takes a sep argument and
# does not append a newline by default
catm <- function(..., sep = " ", appendLF = FALSE) {
    message(paste(..., sep = sep), appendLF = appendLF)
}

##
## reassign the slots to an S4 dfm-like object
## necessary when some operation from the Matrix class obliterates them
## Ken B
reassign_slots <- function(x_new, x_org, exceptions = NULL) {
    names_slot <- slotNames(x_org)
    exceptions <- c("Dim", "Dimnames", "i", "p", "x", "factors", exceptions)
    names_slot <- names_slot[!names_slot %in% exceptions]
    for (name_slot in names_slot) {
        slot(x_new, name_slot) <- slot(x_org, name_slot)
    }
    return(x_new)
}


#' R-like alternative to reassign_attributes()
#' @keywords internal
#' @param x an object
#' @param overwrite if \code{TRUE}, overwrite old attributes
#' @param value new attributes
#' @author Kohei Watanabe
"attributes<-" <- function(x, overwrite = TRUE, value) {
    if (overwrite) {
        base::attributes(x) <- value
    } else {
        base::attributes(x) <- c(base::attributes(x), value[!(names(value) %in% names(base::attributes(x)))])
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

texts_random <- function(n_doc = 10, 
                         n_word = 100, 
                         len_word = 5, 
                         n_type = 1000, 
                         fast = FALSE, 
                         code = FALSE,
                         seed, characters){
    
    if(!missing(seed)) set.seed(seed)
    if(missing(characters)){
        # Empirical distribution in English (https://en.wikipedia.org/wiki/Letter_frequency)
        chars <- letters
        prob_chars <-c(0.08167, 0.01492, 0.02782, 0.04253, 0.12702, 0.02228, 0.02015, 
                       0.06094, 0.06966, 0.00153, 0.00772, 0.04025, 0.02406, 0.06749,
                       0.07507, 0.01929, 0.00095, 0.05987, 0.06327, 0.09056, 0.02758, 
                       0.00978, 0.02360, 0.00150, 0.01974, 0.00074)
    }else{
        # Log-normal distribution
        chars <- characters
        dist_chars <- stats::rlnorm(length(chars))
        prob_chars <- sort(dist_chars / sum(dist_chars), decreasing = TRUE)
    }
    if(n_type > length(chars) ^ len_word) 
        stop('n_type is too large')
    
    # Generate unique types
    type <- c()
    if(fast){
        pat <- stri_flatten(c('[', chars, ']'))
        while(n_type > length(type)){
            type <- unique(c(type, stri_rand_strings(n_type, 1:len_word, pat)))
        }
    }else{
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
    if(code){
        return(code(texts))
    }else{
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
    for(text in texts[2:(len-1)]){
        cat(paste0('         "', text, '",\n'))
    }
    cat(paste0('         "', texts[len], '")\n'))
}

#' convert various input as features to a simple list
#' 
#' Convert various input as features into a simple list, for input as a sequence in e.g. 
#' \code{\link{tokens_compound}}.
#' @param features the input features, one of: \itemize{ \item{character vector,
#'   }{whose elements will be split on whitespace;} \item{list of characters,
#'   }{consisting of a list of token patterns, separated by white space}; 
#'   \item{\link{tokens} object;} \item{\link{dictionary} object}{;} 
#'   \item{\link{collocations} object.}{} }
#' @return an unnamed list of features, with each element of the list a
#'   character vector with the split sequence.
#' @keywords internal utilities
features2list <- function(features) {
    

    # convert the input into a simple, unnamed list of split characters
    if (is.dictionary(features)) {
        result <- stringi::stri_split_fixed(unlist(features, use.names = FALSE), 
                                            attr(features, 'concatenator'))
    } else if (is.collocations(features)) {
        result <- stringi::stri_split_fixed(features$collocation, " ")
    } else if (is.character(features)) {
        result <- stringi::stri_split_fixed(features, " ")
    } else if (is.tokens(features)) {
        result <- as.list(features)
    } else if (is.list(features)) {
        result <- features
    } else {
        stop("features must be a character vector, a list of character elements, a dictionary, or collocations")
    }
    
    # make sure the resulting list is all character
    if (!all(is.character(unlist(result, use.names = FALSE))))
        stop("sequences must be a list of character elements or a dictionary")
    return(as.list(result))
}

#' convert various input as features to a vector
#' 
#' Convert various input as features to a vector for fucntions that 
#' do not support multi-word features e.g.
#' \code{\link{dfm_select}, \link{sequences}, \link{collocations}}.
#' @inheritParams features2list
#' @return an unnamed vector of features
#' @keywords internal utilities
features2vector <- function(features) {
    
    temp <- features2list(features)
    if (any(lengths(temp) > 1)) {
        warning(as.character(sys.calls())[1], ' does not support multi-word features')
    }
    return(unlist(temp, use.names = FALSE))
}


#' issue warning for deprecrated function arguments
#' 
#' Sets a new argument name to an older one, and issues a deprecation warning.
#' @keywords internal
#' @examples 
#' fn <- function(remove_numbers = TRUE, ...) {
#'     args <- as.list(match.call())
#'     remove_numbers <- quanteda:::deprecate_argument("remove_numbers", "remove_numbers", args)
#'     cat("remove_numbers =", remove_numbers, "\n")
#' }
#' fn(remove_numbers = FALSE)
#' fn(remove_numbers = FALSE)
deprecate_argument <- function(old, new, args){
    if (!is.null(args[[old]])) {
        warning("argument \"", old, "\" is deprecated: use \"", new , "\" instead.", call. = FALSE)
        return(args[[old]])
    } else if (!is.null(args[[new]])) {
        return(args[[new]])
    } else {
        return(formals('tokens')[[new]])
    }
}
