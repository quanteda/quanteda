
#' tokenize a set of texts
#'
#' Tokenize the texts from a character vector or from a corpus.
#' @rdname tokens
#' @param x a character or \link{corpus} object to be tokenized
#' @keywords tokens
#' @export
#' @param what the unit for splitting the text, available alternatives are: 
#'   \describe{ \item{\code{"word"}}{(recommended default) smartest, but 
#'   slowest, word tokenization method; see 
#'   \link[stringi]{stringi-search-boundaries} for details.} 
#'   \item{\code{"fasterword"}}{dumber, but faster, word tokenization method, 
#'   uses \code{{\link[stringi]{stri_split_charclass}(x, "\\\\p{WHITE_SPACE}")}}} 
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
#'   \code{\link{tokens_ngrams}}.
#' @param skip integer vector specifying the skips for skip-grams, default is 0 
#'   for only immediately neighbouring words. Only applies if \code{ngrams} is 
#'   different from the default of 1.  See \code{\link{tokens_skipgrams}}.
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
#' @param ... additional arguments not used
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
#' @seealso \code{\link{tokens_ngrams}}, \code{\link{tokens_skipgrams}}
#' @keywords tokens
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
#' tokens(data_char_inaugural[c(2,40)], what = "sentence")
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
tokens <-  function(x, what = c("word", "sentence", "character", "fastestword", "fasterword"),
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
                    verbose = FALSE,  
                    ...) {
    UseMethod("tokens")
}

#' @rdname tokens
#' @noRd
#' @export
tokens.character <- function(x, what = c("word", "sentence", "character", "fastestword", "fasterword"),
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
                             verbose = FALSE, ...) {
    
    what <- match.arg(what)
    names_org <- names(x)
    attrs_org <- attributes(x)
    
    if (simplify)
        warning("simplify no longer available")
    
    if (length(addedArgs <- list(...)))
        warning("Argument", ifelse(length(addedArgs)>1, "s ", " "), names(addedArgs), " not used.", sep = "")
    
    if (!is.integer(ngrams)) ngrams <- as.integer(ngrams)
    
    if (verbose) catm("Starting tokenization...\n")
    
    if (removeTwitter == FALSE & !(what %in% c("fastword", "fastestword"))) {
        if (verbose) catm("...preserving Twitter characters (#, @)\n")
        x <- stringi::stri_replace_all_fixed(x, c("#", "@"), c("_ht_", "_as_"), vectorize_all = FALSE)
    }
    
    startTimeTok <- proc.time()
    
    # Split x into smaller blocks to reducre peak memory consumption
    x_blocks <- split(x, ceiling(seq_along(x) / 10000))
    result_blocks <- list()
    for (i in 1:length(x_blocks)) {
        
        if (verbose) catm("...tokenizing", i, "of" , length(x_blocks), "blocks...\n")
        
        if (what %in% c("word", "fastestword", "fasterword")) {
            result_temp <- tokens_word(x_blocks[[i]], what, removeNumbers, removePunct, removeSymbols, 
                                       removeSeparators, removeTwitter, removeHyphens, removeURL, verbose)
        } else if (what == "character") {
            result_temp <- tokens_character(x_blocks[[i]], what, removeNumbers, removePunct, removeSymbols, 
                                            removeSeparators, removeTwitter, removeHyphens, removeURL, verbose)
        } else if (what == "sentence") {
            result_temp <- tokens_sentence(x_blocks[[i]], what, removeNumbers, removePunct, removeSymbols, 
                                           removeSeparators, removeTwitter, removeHyphens, removeURL, verbose)
        } else {
            stop(what, " not implemented in tokens().")
        }
        
        if (removeTwitter == FALSE & !(what %in% c("fastword", "fastestword"))) {
            if (verbose) catm("...replacing Twitter characters (#, @)\n")
            result_temp <- lapply(result_temp, stringi::stri_replace_all_fixed, c("_ht_", "_as_"), c("#", "@"), vectorize_all = FALSE)
        }
        
        # Hash the tokens
        if (hash == TRUE) {
            if (verbose) catm("...hashing tokens\n")
            if (i == 1) {
                result_blocks[[i]] <- tokens_hash(result_temp)
            }else{
                result_blocks[[i]] <- tokens_hash(result_temp, attr(result_blocks[[i - 1]], 'types'))
            }
        }else{
            result_blocks[[i]] <- result_temp
        }
    }
    
    # Put all the blocked results togather
    result <- unlist(result_blocks, recursive = FALSE)
    
    if (hash == TRUE){
        class(result) <- c("tokens", "tokenizedTexts")
        types(result) <- attr(result_blocks[[length(result_blocks)]], 'types') # last block has all the types
    }else{
        class(result) <- "tokenizedTexts"
    }  
    if (!identical(ngrams, 1L)) {
        if (verbose) catm("...creating ngrams\n")
        result <- tokens_ngrams(result, n = ngrams, skip = skip, concatenator = concatenator)
    }
    if (verbose){
        catm("...total elapsed: ", (proc.time() - startTimeTok)[3], "seconds.\n")
        catm("Finished tokenizing and cleaning", format(length(result), big.mark=","), "texts.\n")
    }
    
    names(result) <- names_org # stri_* destroys names, so put them back
    attr(result, "what") <- what
    attr(result, "ngrams") <- ngrams
    attr(result, "concatenator") <- ifelse(all.equal(ngrams, 1L)==TRUE, "", concatenator)
    
    return(result)
}

#' @rdname tokens
#' @export
#' @noRd
tokens.corpus <- function(x, ...) {
    tokens(texts(x), ...)
}


#' coercion and checking functions for tokens objects
#' 
#' Coerce a list of character elements to a quanteda \link{tokens} object, or check whether 
#' an object is a \link{tokens} object.
#' @param x list of character elements
#' @return \code{as.tokens} returns a quanteda \link{tokens} object
#' @export
as.tokens <- function(x) {
    UseMethod("as.tokens")
}

#' @rdname as.tokens
#' @export
as.tokens.list <- function(x) {
    tokens_hash(x)
}

#' @export
#' @noRd
as.tokens.tokenizedTexts <- function(x) {
    tokens_hash(x)
}

#' @rdname as.tokens
#' @param ... unused
#' @return \code{as.list.tokens} returns a simple list of characters from a
#'   \link{tokens} object
#' @export
as.list.tokens <- function(x, ...) {
    result <- as.tokenizedTexts(x)
    attributes(result) <- NULL
    names(result) <- names(x)
    result
}

#' @rdname as.tokens
#' @param use.names logical; preserve names if \code{TRUE}.  For
#'   \code{as.character.tokens} only.
#' @return \code{as.character.tokens} returns a character vector from a 
#'   \link{tokens} object
#' @export
as.character.tokens <- function(x, use.names = FALSE, ...) {
    unlist(as.list(x), use.names = use.names)
} 

#' @rdname as.tokens
#' @export
#' @return \code{is.tokens} returns \code{TRUE} if the object is of class
#'   tokens, \code{FALSE} otherwise.
is.tokens <- function(x) {
    ifelse("tokens" %in% class(x), TRUE, FALSE)
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
#' @keywords internal tokens
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
#' toks <- tokens_hash(tokenize(c(one = "a b c d A B C D",
#'                                 two = "A B C d")))
#' 
tokens_hash <- function(x, types_reserved, ...) {
    
    types <- unique(unlist(x, use.names = FALSE))
    types <- types[types != '']  # remove empty tokens
    
    if (missing(types_reserved)) {
        types <- types
    } else {
        types <- c(types, setdiff(types, types_reserved))
    }
    tokens <- lapply(x, fastmatch::fmatch, types) # serialize tokens 
    
    # Restore and add additional attributes
    attributes(tokens) <- attributes(x)
    attr(tokens, "types") <- types
    class(tokens) <- c("tokens", class(x))
    return(tokens)
}


#' @rdname tokenize
#' @details \code{as.tokenizedTexts} coerces tokenizedTextsHashed to a
#'   tokenizedText class object, making the methods available for this object
#'   type available to this object.
#' @keywords internal tokens
#' @export
as.tokenizedTexts.tokens <- function(x, ...) {
    
    types <- c("", types(x))
    tokens <- lapply(unclass(x), function(y) types[y + 1]) # shift index to show padding 
    tokens <- reassign_attributes(tokens, x, exceptions = 'class')
    class(tokens) <- c("tokenizedTexts", "list")
    return(tokens)
}

#' print a tokens objects
#' print method for a tokenizedTextsHashed object
#' @param x a tokens object created by \code{\link{tokens}}
#' @param ... further arguments passed to base print method
#' @export
#' @method print tokens
#' @noRd
print.tokens <- function(x, ...) {
    cat(class(x)[1], " from ", ndoc(x), " document", 
        ifelse(ndoc(x) > 1, "s", ""), ".\n", sep = "")
    types <- c("", types(x))
    x <- lapply(unclass(x), function(y) types[y + 1]) # shift index to show padding 
    class(x) <- "listof"
    print(x, ...)
}


#' @method "[" tokens
#' @export
#' @noRd
#' @examples 
#' toks <- tokens(c(d1 = "one two three", d2 = "four five six", d3 = "seven eight"))
#' str(toks)
#' toks[c(1,3)]
"[.tokens" <- function(x, i, ...) {
    tokens <- unclass(x)[i]
    if(length(tokens) == 1 && is.null(tokens[[1]])) return(tokens)
    tokens <- reassign_attributes(tokens, x, exceptions = 'names')
    tokens_hashed_recompile(tokens)
}

#' @method "[[" tokens
#' @export
#' @noRd
#' @examples 
#' toks <- tokens(c(d1 = "one two three", d2 = "four five six", d3 = "seven eight"))
#' str(toks)
#' toks[[2]]
"[[.tokens" <- function(x, i, ...) {
    types <- c("", types(x))
    types[unclass(x)[[i]] + 1] # shift index to show padding 
}

#' @method "$" tokens
#' @export
#' @noRd
#' @examples 
#' toks <- tokens(c(d1 = "one two three", d2 = "four five six", d3 = "seven eight"))
#' str(toks)
#' toks$d3
"$.tokens" <- function(x, i, ...) {
    x[[i]]
}

#' @method lengths tokens
#' @noRd
#' @export
lengths.tokens <- function(x, use.names = TRUE) {
    NextMethod()
}



##
## ============== INTERNAL FUNCTIONS =======================================
##

tokens_word <- function(txt, what, removeNumbers, removePunct, removeSymbols, removeSeparators, 
                        removeTwitter, removeHyphens, removeURL, verbose){
    
    # to preserve intra-word hyphens, replace with _hy_
    if (!removeHyphens & removePunct)
        txt <- stri_replace_all_regex(txt, "(\\b)[\\p{Pd}](\\b)", "$1_hy_$2")
    else if (removeHyphens)
        txt <- stri_replace_all_regex(txt, "(\\b)[\\p{Pd}](\\b)", "$1 $2")
    
    if (removeURL) {
        if (verbose & removeURL) catm(", removing URLs")
        URLREGEX <- "https?:\\/\\/(www\\.)?[-a-zA-Z0-9@:%._\\+~#=]{2,256}\\.[a-z]{2,4}\\b([-a-zA-Z0-9@:%_\\+.~#?&//=]*)"
        txt <- stri_replace_all_regex(txt, URLREGEX, "")
    }
    
    if (what == "fasterword" | what == "fastestword") {
        
        regexToEliminate <- paste(ifelse(removeNumbers, "\\b\\d+\\b", ""),
                                  ifelse(removePunct, paste0("(?![", ifelse(removeTwitter, "_", "@#_"),  "])[\\p{P}]"), ""),
                                  ifelse(removeSymbols, "[\\p{S}]", ""),
                                  sep = "|")
        
        # catm("\n..", regexToEliminate, "..\n", sep = "")
        regexToEliminate <- gsub("^\\|+", "", regexToEliminate)
        regexToEliminate <- gsub("\\|+$", "", regexToEliminate)
        # catm("\n..", regexToEliminate, "..\n", sep = "")
        if (gsub("|", "", regexToEliminate, fixed = TRUE) != "")
            txt <- stri_replace_all_regex(txt, regexToEliminate, "")
        
        if (verbose & removePunct==TRUE) catm(", ", what, " tokenizing", sep="")
        if (what=="fastestword")
            tok <- stringi::stri_split_fixed(txt, " ")
        else if (what=="fasterword")
            tok <- stringi::stri_split_charclass(txt, "\\p{WHITE_SPACE}")
        tok <- qatd_cpp_remove_chr_list(tok, "")
        
        
    } else {
        tok <- stringi::stri_split_boundaries(txt, 
                                              type = "word", 
                                              skip_word_none = (removePunct | removeSymbols), # this is what obliterates currency symbols, Twitter tags, and URLs
                                              skip_word_number = removeNumbers) # but does not remove 4u, 2day, etc.
        # Remove separators if option is TRUE
        if (removeSeparators & !removePunct) {
            tok <- lapply(tok, function(x) x[!stri_detect_regex(x, "^\\s$")])
        }
    }
    
    # Put hyphens back the fast way
    if (!removeHyphens & removePunct)
        tok <- lapply(tok, stri_replace_all_fixed, "_hy_", "-")
    
    return(tok)
}

tokens_sentence <- function(txt, what, removeNumbers, removePunct, removeSymbols, removeSeparators, 
                            removeTwitter, removeHyphens, removeURL, verbose){
    
    if (verbose) catm("...separating into sentences.\n")
    
    # Replace . delimiter from common title abbreviations, with _pd_
    exceptions <- c("Mr", "Mrs", "Ms", "Dr", "Jr", "Prof", "Ph.D", "M", "MM", "St", "etc")
    findregex <- paste0("\\b(", exceptions, ")\\.")
    txt <- stri_replace_all_regex(txt, findregex, "$1_pd_", vectorize_all = FALSE)
    
    ## Remove newline chars 
    txt <- lapply(txt, stringi::stri_replace_all_fixed, "\n", " ")
    
    ## Perform the tokenization
    tok <- stringi::stri_split_boundaries(txt, type = "sentence")
    
    ## Cleaning
    tok <- lapply(tok, function(x){
        x <- x[which(x != "")] # remove any "sentences" that were completely blanked out
        x <- stringi::stri_trim_right(x) # trim trailing spaces
        x <- stri_replace_all_fixed(x, "_pd_", ".") # replace the non-full-stop "." characters
        return(x)
    } )
    
    return(tok)
}

tokens_character <- function(txt, what, removeNumbers, removePunct, removeSymbols, removeSeparators, 
                             removeTwitter, removeHyphens, removeURL, verbose){
    
    # note: does not implement removeNumbers
    tok <- stringi::stri_split_boundaries(txt, type = "character")
    if (removePunct) {
        if (verbose) catm("...removing punctuation.\n")
        tok <- lapply(tok, function(x){
            x <- stringi::stri_replace_all_charclass(x, "[\\p{P}]", "")
            x <- x[which(x != "")]
            return(x)    
        })
    } 
    if (removeSymbols) {
        if (verbose) catm("...removing symbols.\n")
        tok <- lapply(tok, function(x){
            x <- stringi::stri_replace_all_charclass(x, "[\\p{S}]", "")
            x <- x[which(x != "")]
            return(x)    
        })
    } 
    if (removeSeparators) {
        if (verbose) catm("...removing separators.\n")
        tok <- lapply(tok, function(x){
            x <- stringi::stri_subset_regex(x, "^\\p{Z}$")
            x <- x[which(x != "")]
            return(x)    
        })
    }
    return(tok)
    
}



# recompile a hashed tokens object
# 
# This function recompiles a hashed tokens object when the vocabulary has been changed in 
# a way that makes some of its types identical, such as lowercasing when a lowercased 
# version of the type already exists in the hash table.
# @param x the \link[=tokens_hash]{tokenizedTexts} object to be recompiled
# @examples 
# toks <- tokens_hash(tokenize(c(one = "a b c d A B C D",
#                                two = "A B C d")))
# tokens_hashed_recompile(toks)
tokens_hashed_recompile <- function(x) {
    
    attrs_input <- attributes(x)
    index_unique <- unique(unlist(x, use.names = FALSE))
    index_unique <- index_unique[index_unique != 0] # exclude padding
    
    # Remove gaps in the type index, if any, remap index
    if (any(is.na(match(seq_len(length(types(x))), index_unique)))) { 
        types_new <- types(x)[index_unique]
        index_new <- c(0, seq_along(index_unique)) # padding index is zero but not in types
        index_unique <- c(0, index_unique) # padding index is zero but not in types
        x <- lapply(unclass(x), function(y) index_new[fastmatch::fmatch(y, index_unique)]) # shift index for padding
        attributes(x) <- attrs_input
        types(x) <- types_new
    }
    
    # Reindex duplicates, if any
    if (any(duplicated(types(x)))) {
        types <- types(x)
        types_unique <- unique(types)
        index_mapping <- match(types, types_unique)
        index_mapping <- c(0, index_mapping) # padding index is zero but not in types
        x <- lapply(unclass(x), function(y) index_mapping[y + 1]) # shift index for padding
        attributes(x) <- attrs_input
        types(x) <- types_unique
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
    attr(x, "types")
}

"types<-" <- function(x, value) {
    UseMethod("types<-")
}

"types<-.tokens" <- function(x, value) {
    if (!is.character(value))
        stop("replacement value must be character")
    attr(x, "types") <- value
    return(x)
}


# # check for deprecated argument names
# ellipsis_args <- list(...)
# if (!is.null(ellipsis_args$toLower)) {
#     warning("argument \"toLower\" is deprecated: use \"tolower\" instead.")
#     if (missing(tolower)) tolower <- toLower
# }
# if (!is.null(ellipsis_args$ignoredFeatures)) {
#     warning("argument \"ignoredFeatures\" is deprecated: use \"remove\" instead.")
#     if (missing(remove)) remove <- ignoredFeatures
# }
# if (!is.null(ellipsis_args$keptFeatures)) {
#     warning("argument \"keptFeatures\" is deprecated: use \"select\" instead.")
#     if (missing(select)) select <- ignoredFeatures
# }