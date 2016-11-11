
#' tokenize a set of texts
#'
#' Tokenize the texts from a character vector or from a corpus.
#' @rdname tokens
#' @param x objet to be tokenized
#' @param ... additional arguments not used
#' @export
tokens <- function(x, ...) {
    UseMethod("tokens")
}

#' @rdname tokens
#' @param what the unit for splitting the text, available alternatives are: 
#'   \describe{ \item{\code{"word"}}{(recommended default) smartest, but 
#'   slowest, word tokenization method; see 
#'   \link[stringi]{stringi-search-boundaries} for details.} 
#'   \item{\code{"fasterword"}}{dumber, but faster, word tokenizeation method, 
#'   uses {\link[stringi]{stri_split_charclass}(x, "\\\\p{WHITE_SPACE}")}} 
#'   \item{\code{"fastestword"}}{dumbest, but fastest, word tokenization method,
#'   calls \code{\link[stringi]{stri_split_fixed}(x, " ")}} 
#'   \item{\code{"character"}}{tokenization into individual characters} 
#'   \item{\code{"sentence"}}{sentence segmenter, smart enough to handle some 
#'   exceptions in English such as "Prof. Plum killed Mrs. Peacock." (but far 
#'   from perfect).} }
#' @param removeNumbers remove tokens that consist only of numbers, but not 
#'   words that start with digits, e.g. \code{2day}
#' @param removePunct if \code{TRUE}, remove all characters in the Unicode 
#'   "Punctuation" [P] class
#' @param removeSymbols if \code{TRUE}, remove all characters in the Unicode 
#'   "Symbol" [S] class
#' @param removeTwitter remove Twitter characters \code{@@} and \code{#}; set to
#'   \code{TRUE} if you wish to eliminate these.
#' @param removeURL if \code{TRUE}, find and eliminate URLs beginning with 
#'   http(s) -- see section "Dealing with URLs".
#' @param removeHyphens if \code{TRUE}, split words that are connected by 
#'   hyphenation and hyphenation-like characters in between words, e.g. 
#'   \code{"self-storage"} becomes \code{c("self", "storage")}.  Default is 
#'   \code{FALSE} to preserve such words as is, with the hyphens.  Only applies 
#'   if \code{what = "word"}.
#' @param removeSeparators remove Separators and separator characters (spaces 
#'   and variations of spaces, plus tab, newlines, and anything else in the 
#'   Unicode "separator" category) when \code{removePunct=FALSE}.  Only 
#'   applicable for \code{what = "character"} (when you probably want it to be 
#'   \code{FALSE}) and for \code{what = "word"} (when you probably want it to be
#'   \code{TRUE}).  Note that if \code{what = "word"} and you set 
#'   \code{removePunct = TRUE}, then \code{removeSeparators} has no effect.  Use
#'   carefully.
#' @param ngrams integer vector of the \emph{n} for \emph{n}-grams, defaulting 
#'   to \code{1} (unigrams). For bigrams, for instance, use \code{2}; for 
#'   bigrams and unigrams, use \code{1:2}.  You can even include irregular 
#'   sequences such as \code{2:3} for bigrams and trigrams only.  See 
#'   \code{\link{ngrams}}.
#' @param skip integer vector specifying the skips for skip-grams, default is 0 
#'   for only immediately neighbouring words. Only applies if \code{ngrams} is 
#'   different from the default of 1.  See \code{\link{skipgrams}}.
#' @param concatenator character to use in concatenating \emph{n}-grams, default
#'   is "\code{_}", which is recommended since this is included in the regular 
#'   expression and Unicode definitions of "word" characters
#' @param simplify no longer active: `tokens()` always returns a tokens object, 
#'   whose basic structure is a list (even for a single document)
#' @param hash if \code{TRUE} (default), return a hashed tokens object, 
#'   otherwise, return a classic \code{tokenizedTexts} object.  (This will be 
#'   phased out soon in coming versions.)
#' @param verbose if \code{TRUE}, print timing messages to the console; off by 
#'   default
#' @import stringi
#' @details The tokenizer is designed to be fast and flexible as well as to 
#'   handle Unicode correctly. Most of the time, users will construct \link{dfm}
#'   objects from texts or a corpus, without calling \code{tokens()} as an 
#'   intermediate step.  Since \code{tokens()} is most likely to be used by more
#'   technical users, we have set its options to default to minimal 
#'   intervention. This means that punctuation is tokenized as well, and that 
#'   nothing is removed by default from the text being tokenized except 
#'   inter-word spacing and equivalent characters.
#' @section Dealing with URLs: URLs are tricky to tokenize, because they contain
#'   a number of symbols and punctuation characters.  If you wish to remove 
#'   these, as most people do, and your text contains URLs, then you should set 
#'   \code{what = "fasterword"} and \code{removeURL = TRUE}.  If you wish to 
#'   keep the URLs, but do not want them mangled, then your options are more 
#'   limited, since removing punctuation and symbols will also remove them from 
#'   URLs.  We are working on improving this behaviour.
#'   
#'   See the examples below.
#' @return \pkg{quanteda} \code{tokens} class object, by default a hashed list 
#'   of integers corresponding to a vector of types.
#' @note For accessing the tokens themselves, use `get_tokens()`; to access the 
#'   types, use `types()`, which also works with as an assignment operation
#'   (\code{types(x) <- newtypes}).  Note as well that 
#'   \code{\link{as.tokenizedTexts}} or \code{\link{as.list.tokens}} will do the
#'   same things as `get_tokens()`.  The purpose of these extractor functions is
#'   \emph{encapulation}, so that if the structure of the \code{tokens} object 
#'   changes, code built on extracting these objects will still work.
#' @export
#' @seealso \code{\link{ngrams}}, \code{\link{skipgrams}}
#' @examples 
#' txt <- c(doc1 = "This is a sample: of tokens.",
#'          doc2 = "Another sentence, to demonstrate how tokens works.")
#' tokens(txt)
#' # removing punctuation marks and lowecasing texts
#' tokens(toLower(txt), removePunct = TRUE)
#' # keeping versus removing hyphens
#' tokens("quanteda data objects are auto-loading.", removePunct = TRUE)
#' tokens("quanteda data objects are auto-loading.", removePunct = TRUE, removeHyphens = TRUE)
#' # keeping versus removing symbols
#' tokens("<tags> and other + symbols.", removeSymbols = FALSE)
#' tokens("<tags> and other + symbols.", removeSymbols = TRUE)
#' tokens("<tags> and other + symbols.", removeSymbols = FALSE, what = "fasterword")
#' tokens("<tags> and other + symbols.", removeSymbols = TRUE, what = "fasterword")
#' 
#' ## examples with URLs - hardly perfect!
#' txt <- "Repo https://githib.com/kbenoit/quanteda, and www.stackoverflow.com."
#' tokens(txt, removeURL = TRUE, removePunct = TRUE)
#' tokens(txt, removeURL = FALSE, removePunct = TRUE)
#' tokens(txt, removeURL = FALSE, removePunct = TRUE, what = "fasterword")
#' tokens(txt, removeURL = FALSE, removePunct = FALSE, what = "fasterword")
#' 
#' 
#' ## MORE COMPARISONS
#' txt <- "#textanalysis is MY <3 4U @@myhandle gr8 #stuff :-)"
#' tokens(txt, removePunct = TRUE)
#' tokens(txt, removePunct = TRUE, removeTwitter = TRUE)
#' #tokens("great website http://textasdata.com", removeURL = FALSE)
#' #tokens("great website http://textasdata.com", removeURL = TRUE)
#' 
#' txt <- c(text1="This is $10 in 999 different ways,\n up and down; left and right!", 
#'          text2="@@kenbenoit working: on #quanteda 2day\t4ever, http://textasdata.com?page=123.")
#' tokens(txt, verbose = TRUE)
#' tokens(txt, removeNumbers = TRUE, removePunct = TRUE)
#' tokens(txt, removeNumbers = FALSE, removePunct = TRUE)
#' tokens(txt, removeNumbers = TRUE, removePunct = FALSE)
#' tokens(txt, removeNumbers = FALSE, removePunct = FALSE)
#' tokens(txt, removeNumbers = FALSE, removePunct = FALSE, removeSeparators = FALSE)
#' tokens(txt, removeNumbers = TRUE, removePunct = TRUE, removeURL = TRUE)
#' 
#' # character level
#' tokens("Great website: http://textasdata.com?page=123.", what = "character")
#' tokens("Great website: http://textasdata.com?page=123.", what = "character", 
#'          removeSeparators = FALSE)
#' 
#' # sentence level         
#' tokens(c("Kurt Vongeut said; only assholes use semi-colons.", 
#'            "Today is Thursday in Canberra:  It is yesterday in London.", 
#'            "Today is Thursday in Canberra:  \nIt is yesterday in London.",
#'            "To be?  Or\nnot to be?"), 
#'           what = "sentence")
#' tokens(inaugTexts[c(2,40)], what = "sentence")
#' 
#' # removing features (stopwords) from tokenized texts
#' txt <- toLower(c(mytext1 = "This is a short test sentence.",
#'                  mytext2 = "Short.",
#'                  mytext3 = "Short, shorter, and shortest."))
#' tokens(txt, removePunct = TRUE)
#' ### removeFeatures(tokens(txt, removePunct = TRUE), stopwords("english"))
#' 
#' # ngram tokenization
#' ### tokens(txt, removePunct = TRUE, ngrams = 2)
#' ### tokens(txt, removePunct = TRUE, ngrams = 2, skip = 1, concatenator = " ")
#' ### tokens(txt, removePunct = TRUE, ngrams = 1:2)
#' # removing features from ngram tokens
#' ### removeFeatures(tokens(txt, removePunct = TRUE, ngrams = 1:2), stopwords("english"))
tokens.character <- function(x, what=c("word", "sentence", "character", "fastestword", "fasterword"),
                               removeNumbers = FALSE,
                               removePunct = FALSE,
                               removeSymbols = FALSE,
                               removeSeparators = TRUE,
                               removeTwitter = FALSE,
                               removeHyphens = FALSE,
                               removeURL = FALSE,
                               ngrams = 1L,
                               skip = 0L,
                               concatenator = "_",
                               simplify = FALSE,
                               hash = TRUE,
                               verbose = FALSE,  ## FOR TESTING
                               ...) {
    
    what <- match.arg(what)
    
    if (simplify)
        warning("simplify no longer available")
    
    if (length(addedArgs <- list(...)))
        warning("Argument", ifelse(length(addedArgs)>1, "s ", " "), names(addedArgs), " not used.", sep = "")
    
    if (!is.integer(ngrams)) ngrams <- as.integer(ngrams)
    
    if (verbose) catm("Starting tokenization...\n")
    result <- x
    
    if (removeTwitter == FALSE & !(what %in% c("fastword", "fastestword"))) {
        if (verbose) catm("  ...preserving Twitter characters (#, @)")
        startTimeClean <- proc.time()
        result <- stringi::stri_replace_all_fixed(result, c("#", "@"), c("_ht_", "_as_"), vectorize_all = FALSE)
        if (verbose) catm("...total elapsed:", (proc.time() - startTimeClean)[3], "seconds.\n")
    }
    
    if (verbose) catm("  ...tokenizing texts")
    startTimeTok <- proc.time()
    
    if (grepl("word$", what)) {
        
        # to preserve intra-word hyphens, replace with _hy_
        if (!removeHyphens & removePunct)
            result <- stri_replace_all_regex(result, "(\\b)[\\p{Pd}](\\b)", "$1_hy_$2")
        else if (removeHyphens)
            result <- stri_replace_all_regex(result, "(\\b)[\\p{Pd}](\\b)", "$1 $2")
        
        if (removeURL) {
            if (verbose & removeURL) catm(", removing URLs")
            URLREGEX <- "https?:\\/\\/(www\\.)?[-a-zA-Z0-9@:%._\\+~#=]{2,256}\\.[a-z]{2,4}\\b([-a-zA-Z0-9@:%_\\+.~#?&//=]*)"
            result <- stri_replace_all_regex(result, URLREGEX, "")
        }
        
        if (what == "fasterword" | what == "fastestword") {
            
            if (verbose & removeNumbers==TRUE) catm(", removing numbers")
            if (verbose & removePunct==TRUE) catm(", removing punctuation")
            if (verbose & removeSymbols==TRUE) catm(", removing symbols")
            regexToEliminate <- paste(ifelse(removeNumbers, "\\b\\d+\\b", ""),
                                      ifelse(removePunct, paste0("(?![", ifelse(removeTwitter, "_", "@#_"),  "])[\\p{P}]"), ""),
                                      ifelse(removeSymbols, "[\\p{S}]", ""),
                                      sep = "|")
            # catm("\n..", regexToEliminate, "..\n", sep = "")
            regexToEliminate <- gsub("^\\|+", "", regexToEliminate)
            regexToEliminate <- gsub("\\|+$", "", regexToEliminate)
            # catm("\n..", regexToEliminate, "..\n", sep = "")
            if (gsub("|", "", regexToEliminate, fixed = TRUE) != "")
                result <- stri_replace_all_regex(result, regexToEliminate, "")
            
            if (verbose & removePunct==TRUE) catm(", ", what, " tokenizing", sep="")
            if (what=="fastestword")
                result <- stringi::stri_split_fixed(result, " ")
            else if (what=="fasterword")
                result <- stringi::stri_split_charclass(result, "\\p{WHITE_SPACE}")
            #result <- lapply(result, function(x) x <- x[which(x != "")]) # this is dealy slow
            result <- qatd_cpp_remove_string_list(result, "")
            
            # if (removeURL)
            #     result <- lapply(result, function(x) x <- x[-which(substring(x, 1, 4) == "http")])
            
        } else {
            result <- stringi::stri_split_boundaries(result, 
                                                     type = "word", 
                                                     skip_word_none = (removePunct | removeSymbols), # this is what obliterates currency symbols, Twitter tags, and URLs
                                                     skip_word_number = removeNumbers) # but does not remove 4u, 2day, etc.
            # remove separators if option is TRUE
            if (removeSeparators & !removePunct) {
                if (verbose) catm("\n  ...removing separators.")
                result <- lapply(result, function(x) x[!stri_detect_regex(x, "^\\s$")])
            }
        }
        
        # put hyphens back the fast way
        if (!removeHyphens & removePunct)
            result <- lapply(result, stri_replace_all_fixed, "_hy_", "-")
        
    } else if (what == "character") {
        
        # note: does not implement removeNumbers
        result <- stringi::stri_split_boundaries(result, type = "character")
        if (removePunct) {
            if (verbose) catm("   ...removing punctuation.\n")
            result <- lapply(result, stringi::stri_replace_all_charclass, "[\\p{P}]", "")
            result <- lapply(result, function(x) x <- x[which(x != "")])
        } 
        if (removeSymbols) {
            if (verbose) catm("   ...removing symbols.\n")
            result <- lapply(result, stringi::stri_replace_all_charclass, "[\\p{S}]", "")
            result <- lapply(result, function(x) x <- x[which(x != "")])
        } 
        if (removeSeparators) {
            if (verbose) catm("   ...removing separators.\n")
            result <- lapply(result, function(x) x[!stringi::stri_detect_regex(x, "^\\p{Z}$")])
        }
        
    } else if (what == "sentence") {
        if (verbose) catm("\n   ...separating into sentences.")
        
        # replace . delimiter from common title abbreviations, with _pd_
        exceptions <- c("Mr", "Mrs", "Ms", "Dr", "Jr", "Prof", "Ph.D", "M", "MM", "St", "etc")
        findregex <- paste0("\\b(", exceptions, ")\\.")
        result <- stri_replace_all_regex(result, findregex, "$1_pd_", vectorize_all = FALSE)
        
        ## remove newline chars 
        result <- lapply(result, stringi::stri_replace_all_fixed, "\n", " ")
        
        ## perform the tokenization
        result <- stringi::stri_split_boundaries(result, type = "sentence")
        # remove any "sentences" that were completely blanked out
        result <- lapply(result, function(x) x <- x[which(x != "")])
        
        # trim trailing spaces
        result <- lapply(result, stringi::stri_trim_right)
        
        # replace the non-full-stop "." characters
        result <- lapply(result, stri_replace_all_fixed, "_pd_", ".")
        
    } else {
        stop(what, " not implemented in tokens().")
    }
    
    if (verbose) catm("...total elapsed: ", (proc.time() - startTimeTok)[3], "seconds.\n")
    
    if (removeTwitter == FALSE & !(what %in% c("fastword", "fastestword"))) {
        if (verbose) catm("  ...replacing Twitter characters (#, @)")
        startTimeClean <- proc.time()
        result <- lapply(result, stringi::stri_replace_all_fixed, c("_ht_", "_as_"), c("#", "@"), vectorize_all = FALSE)
        if (verbose) catm("...total elapsed:", (proc.time() - startTimeClean)[3], "seconds.\n")
    }
    
    # make this an S3 class item, if a list
    #if (simplify == FALSE) {
    class(result) <- c("tokenizedTexts", class(result))
    #}
    
    # hash the tokens
    if (hash == TRUE) {
        if (verbose) 
            catm("...hashing tokens\n")
        result <- tokens_hash(result)
    } 
    
    if (!identical(ngrams, 1L)) {
        if (verbose) {
            catm("  ...creating ngrams")
            startTimeClean <- proc.time()
        }
        result <- ngrams(result, n = ngrams, skip = skip, concatenator = concatenator)
        # is the ngram set serial starting with 1? use single call if so (most efficient)
        # if (sum(1:length(ngrams)) == sum(ngrams)) {
        #     result <- lapply(result, ngram, n = length(ngrams), concatenator = concatenator, include.all = TRUE)
        # } else {
        #             result <- lapply(result, function(x) {
        #                 xnew <- c()
        #                 for (n in ngrams) 
        #                     xnew <- c(xnew, ngram(x, n, concatenator = concatenator, include.all = FALSE))
        #                 xnew
        #             })
        # }
        if (verbose) catm("...total elapsed:", (proc.time() - startTimeClean)[3], "seconds.\n")
    }
    
    
    # stri_* destroys names, so put them back
    startTimeClean <- proc.time()
    if (verbose) catm("  ...replacing names")
    names(result) <- names(x)
    if (verbose) catm("...total elapsed: ", (proc.time() - startTimeClean)[3], "seconds.\n")
    # make this an S3 class item, if a list
    # if (!is.tokens(result))
    #     class(result) <- c("tokens", class(result))
    attr(result, "what") <- what
    attr(result, "ngrams") <- ngrams
    attr(result, "concatenator") <- ifelse(all.equal(ngrams, 1L)==TRUE, "", concatenator)
    
    if (verbose) 
        catm("Finished tokenizing and cleaning", format(length(result), big.mark=","), "texts.\n") 
    #, with a total of", format(length(unlist(result)), big.mark=","), "tokens.\n")
    
    result
}

#' @rdname tokens
#' @export
tokens.corpus <- function(x, ...) {
    # get the settings for clean from the corpus and use those, 
    # unless more specific arguments are passed -- ADD THE ABILITY TO PASS THESE
    # need to include sep in this list too 
    tokens(texts(x), ...)
}


#' @export
#' @description \code{is.tokens} returns \code{TRUE} if the object is of class
#'   tokens, \code{FALSE} otherwise.
#' @rdname tokens
is.tokens <- function(x) {
    ifelse("tokens" %in% class(x), TRUE, FALSE)
}

#' @export
#' @description coerces a list or tokens object into a (hashed) \link{tokens} object
#' @rdname tokens
as.tokens <- function(x) {
    UseMethod("as.tokens")
}

#' @export
#' @noRd
as.tokens.list <- function(x) {
    tokens_hash(x)
}

#' @export
#' @noRd
as.tokens.tokenizedTexts <- function(x) {
    tokens_hash(x)
}


#' Function to hash list-of-character tokens
#' 
#' Creates a hashed object of tokens, called by \code{\link{tokens}}.
#' @param x a source of tokenizedText
#' @param types optional pre-existing types for mapping of tokens
#' @param ... additional arguments
#' @return a list the hashed tokens found in each text
#' @importFrom fastmatch fmatch
#' @details This was formerly used to create a \code{tokenizedTextsHashed}
#'   object, but all tokens objects are now hashed, so this is just exported for
#'   testing until it will become internal only.
#' @note This will be internal only soon.
#' @export
#' @seealso \code{\link{tokenize}}
#' @examples 
#' txt <- c(doc1 = "The quick brown fox jumped over the lazy dog.",
#'          doc2 = "The dog jumped and ate the fox.")
#' toks <- tokenize(toLower(txt), removePunct = TRUE)
#' toksHashed <- tokens_hash(toks)
#' toksHashed
#' # returned as a list
#' as.list(toksHashed)
#' # returned as a tokenized Text
#' as.tokenizedTexts(toksHashed)
#' 
#' # change case
#' toksh <- tokens_hash(tokenize(c(one = "a b c d A B C D",
#'                                 two = "A B C d")))
#' 
#' # wordstem
tokens_hash <- function(x, types, ...) {
    
    attr_input <- attributes(x)
    # if (!is.tokenizedTexts(x)) 
    #     stop("Input must be tokenizedTexts")
    
    types_found <- unique(unlist(x, use.names = FALSE))
    # types_found <- sort(types_found)
    types_found <- types_found[types_found != '']  # remove empty "tokens"
    
    if (missing(types)) {
        types <- types_found
    } else {
        types <- c(types, setdiff(types_found, types))
    }

    # "hash" tokens 
    x_hashed <- lapply(x, fastmatch::fmatch, types)
    
    # restore and add additional attributes
    attributes(x_hashed) <- attr_input
    attr(x_hashed, "types") <- types
    class(x_hashed) <- c("tokens", class(x_hashed))
    x_hashed
}


#' @rdname tokens
#' @details \code{as.tokenizedTexts} coerces tokenizedTextsHashed to a
#'   tokenizedText class object, making the methods available for this object
#'   type available to this object.
#' @export
as.tokenizedTexts.tokens <- function(x, ...) {
    types <- types(x)
    attrs_orig <- attributes(x)
    class(x) <- "list"
    x_unhashed <- lapply(x, function(y) types[y])
    attributes(x_unhashed) <- attributes(x)
    # remove types attribute
    attr(x_unhashed, "types") <- NULL
    # replace class tag
    class(x_unhashed) <- c("tokenizedTexts", "list")
    x_unhashed
}

#' @rdname tokens
#' @export
as.list.tokens <- function(x, ...){
    result <- as.tokenizedTexts(x)
    attributes(result) <- NULL
    names(result) <- names(x)
    result
}

#' print a tokens objects
#' print method for a tokenizedTextsHashed object
#' @param x a tokens object created by \code{\link{tokens}}
#' @param ... further arguments passed to base print method
#' @export
#' @method print tokens
print.tokens <- function(x, ...) {
    cat(class(x)[1], " from ", ndoc(x), " document", 
        ifelse(ndoc(x) > 1, "s", ""), ".\n", sep = "")
    x <- lapply(unclass(x), function(y) types(x)[y])
    class(x) <- "listof"
    print(x, ...)
}

# @details \code{tokenizeHashed} creates tokenizedTextsHashed object from characters vactors 
# without creating a large intermediate tokenizedTexts object.
# @param size_chunk size for batches of conversion of texts (number of documents)
# @examples 
# txt <- c('a b c d e', 'd e f g h', 'f g h i j', 'i j k l m')
# tokenizeHashed(txt, size_chunk=2)
#
# \dontrun{data(SOTUCorpus, package = "quantedaData")
# txt <- rep(unlist(tokenize(SOTUCorpus, what='sentence')), 20)
# system.time(toks <- tokens_hash(tokenize(txt)))
# system.time(toks2 <- tokenizeHashed(txt))
# }
# @rdname tokens_hash
# @export
tokenizeHashed <- function(x, size_chunk = 1000, ...) {
    
    xSubs <- split(x, ceiling(seq_along(x) / size_chunk))
    xTokSubs <- list()
    for (i in 1:length(xSubs)) {
        #cat('Tokenizing and hashing ...\n')
        if (i == 1) {
            xTokSubs[[i]] <- tokens_hash(tokenize(xSubs[[i]], ...))
        } else {
            xTokSubs[[i]] <- tokens_hash(tokenize(xSubs[[i]], ...), attr(xTokSubs[[i-1]], "vocabulary"))
        }
    }
    xTok <- unlist(xTokSubs, recursive = FALSE)
    class(xTok) <- c("tokenizedTextsHashed", "tokenizedTexts", class(xTok))
    attr(xTok, "vocabulary") <- attr(xTokSubs[[length(xTokSubs)]], "vocabulary")
    return(xTok)
    
}

#' @export
#' @rdname ndoc
ndoc.tokens <- function(x) {
    length(x)
}

#' @export
#' @rdname ntoken
ntoken.tokens <- function(x, ...) {
    lengths(x)
}

#' @export
#' @rdname ntoken
ntype.tokens <- function(x, ...) {
    length(types(x))
}





##
## ============== INTERNAL FUNCTIONS =======================================
##

# recompile a hashed tokens object
# 
# This function recompiles a hashed tokens object when the vocabulary has been changed in 
# a way that makes some of its types identical, such as lowercasing when a lowercased 
# version of the type already exists in the hash table.
# @param x the \link[=tokens_hash]{tokenizedTexts} object to be recompiled
# @examples 
# toksh <- tokens_hash(tokenize(c(one = "a b c d A B C D",
#                                two = "A B C d")))
# vocabulary(toksh) <- toLower(vocabulary(toksh))
# tokens_hashed_recompile(toksh)
tokens_hashed_recompile <- function(x) {
    
    attrs_input <- attributes(x)
    v_unique_index <- unique(unlist(x, use.names = FALSE))
    
    # remove gaps in the type index, if any, remap index
    if (any(is.na(match(seq_along(v_unique_index), v_unique_index)))) { 
        v_unique_index <- unique(unlist(x, use.names = FALSE))
        v_new <- types(x)[v_unique_index]
        new_types <- seq_along(v_unique_index)
        x_new <- lapply(unclass(x), function(y) new_types[match(y, v_unique_index)])
        attributes(x_new) <- attrs_input
        types(x_new) <- v_new
        attrs_input <- attributes(x_new)
        x <- x_new
    }
        
    # reindex duplicates, if any
    if (any(duplicated(types(x)))) {
        v <- types(x)
        v_unique <- unique(v)
        index_mapping <- match(v, v_unique)
        x_new <- lapply(unclass(x), function(y) index_mapping[y])
        attributes(x_new) <- attrs_input
        types(x_new) <- v_unique
        attrs_input <- attributes(x_new)
        x <- x_new
    }

    return(x)
}

get_tokens <- function(x) {
    UseMethod("get_tokens")
}

get_tokens.tokens <- function(x) {
    as.list(x)
}

types <- function(x) {
    UseMethod("types")
}

types.tokens <- function(x) {
    as.character(attr(x, "types"))
}

"types<-" <- function(x, value) {
    UseMethod("types<-")
}

"types<-.tokens" <- function(x, value) {
    # if (length(unique(unlist(get_tokens(x)))) != length(value))
    #     stop("replacement value must equal unique elements of tokens")
    if (!is.character(value))
        stop("replacement value must be character")
    attr(x, "types") <- value
    x
}


