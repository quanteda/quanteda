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

#' utility function to remove all attributes
#' @keywords internal
remove_attributes <- function(x) {
    base::attributes(x) <- NULL
    return(x)
}

#' utility function to create a object with new set of attributes
#' @keywords internal
create <- function(x, what, attrs = NULL, ...) {
    base::attributes(x) <- NULL
    if (what == 'tokens') {
        class <- c('tokens', 'tokenizedTexts', 'list')
    }
    x <- structure(x, class = class, ...)
    if (!is.null(attrs)) {
        attributes(x, FALSE) <- attrs
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


# convert various input as features to a vector used in tokens_select, 
# tokens_compound and kwic.
features2id <- function(features, types, valuetype, case_insensitive, 
                        concatenator = '_', remove_unigram = FALSE) {
    
    if (is.sequences(features) || is.collocations(features)) {
        features <- stri_split_charclass(features$collocation, "\\p{Z}")
        features_id <- lapply(features, function(x) fastmatch::fmatch(x, types))
        features_id <- features_id[sapply(features_id, function(x) all(!is.na(x)))]
    } else {
        if (is.dictionary(features)) {
            features <- unlist(features, use.names = FALSE)
            features <- split_dictionary_values(features, concatenator)
        } else {
            features <- as.list(features)
        }
        if (remove_unigram)
            features <- features[lengths(features) > 1] # drop single-word features
        features_id <- regex2id(features, types, valuetype, case_insensitive)
    }
    attr(features_id, 'features') <- stri_c_list(features, sep = ' ')
    return(features_id)
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

