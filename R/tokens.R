
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
#' @param remove_numbers remove tokens that consist only of numbers, but not 
#'   words that start with digits, e.g. \code{2day}
#' @param remove_punct if \code{TRUE}, remove all characters in the Unicode 
#'   "Punctuation" [P] class
#' @param remove_symbols if \code{TRUE}, remove all characters in the Unicode 
#'   "Symbol" [S] class
#' @param remove_twitter remove Twitter characters \code{@@} and \code{#}; set to
#'   \code{TRUE} if you wish to eliminate these.  Note that this will always be set 
#'   to \code{FALSE} if \code{remove_punct = FALSE}.
#' @param remove_url if \code{TRUE}, find and eliminate URLs beginning with 
#'   http(s) -- see section "Dealing with URLs".
#' @param remove_hyphens if \code{TRUE}, split words that are connected by 
#'   hyphenation and hyphenation-like characters in between words, e.g. 
#'   \code{"self-storage"} becomes \code{c("self", "storage")}.  Default is 
#'   \code{FALSE} to preserve such words as is, with the hyphens.  Only applies 
#'   if \code{what = "word"}.
#' @param remove_separators remove Separators and separator characters (spaces 
#'   and variations of spaces, plus tab, newlines, and anything else in the 
#'   Unicode "separator" category) when \code{remove_punct=FALSE}.  Only 
#'   applicable for \code{what = "character"} (when you probably want it to be 
#'   \code{FALSE}) and for \code{what = "word"} (when you probably want it to be
#'   \code{TRUE}).  Note that if \code{what = "word"} and you set 
#'   \code{remove_punct = TRUE}, then \code{remove_separators} has no effect.  Use
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
#' @param hash if \code{TRUE} (default), return a hashed tokens object, 
#'   otherwise, return a classic \code{tokenizedTexts} object.  (This will be 
#'   phased out soon in coming versions.)
#' @param verbose if \code{TRUE}, print timing messages to the console; off by 
#'   default
#' @param include_docvars if \code{TRUE}, pass docvars and metadoc fields through to 
#'   the tokens object.  Only applies when tokenizing \link{corpus} objects.
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
#'   \code{what = "fasterword"} and \code{remove_url = TRUE}.  If you wish to 
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
#' tokens(char_tolower(txt), remove_punct = TRUE)
#' # keeping versus removing hyphens
#' tokens("quanteda data objects are auto-loading.", remove_punct = TRUE)
#' tokens("quanteda data objects are auto-loading.", remove_punct = TRUE, remove_hyphens = TRUE)
#' # keeping versus removing symbols
#' tokens("<tags> and other + symbols.", remove_symbols = FALSE)
#' tokens("<tags> and other + symbols.", remove_symbols = TRUE)
#' tokens("<tags> and other + symbols.", remove_symbols = FALSE, what = "fasterword")
#' tokens("<tags> and other + symbols.", remove_symbols = TRUE, what = "fasterword")
#' 
#' ## examples with URLs - hardly perfect!
#' txt <- "Repo https://githib.com/kbenoit/quanteda, and www.stackoverflow.com."
#' tokens(txt, remove_url = TRUE, remove_punct = TRUE)
#' tokens(txt, remove_url = FALSE, remove_punct = TRUE)
#' tokens(txt, remove_url = FALSE, remove_punct = TRUE, what = "fasterword")
#' tokens(txt, remove_url = FALSE, remove_punct = FALSE, what = "fasterword")
#' 
#' 
#' ## MORE COMPARISONS
#' txt <- "#textanalysis is MY <3 4U @@myhandle gr8 #stuff :-)"
#' tokens(txt, remove_punct = TRUE)
#' tokens(txt, remove_punct = TRUE, remove_twitter = TRUE)
#' #tokens("great website http://textasdata.com", remove_url = FALSE)
#' #tokens("great website http://textasdata.com", remove_url = TRUE)
#' 
#' txt <- c(text1="This is $10 in 999 different ways,\n up and down; left and right!", 
#'          text2="@@kenbenoit working: on #quanteda 2day\t4ever, http://textasdata.com?page=123.")
#' tokens(txt, verbose = TRUE)
#' tokens(txt, remove_numbers = TRUE, remove_punct = TRUE)
#' tokens(txt, remove_numbers = FALSE, remove_punct = TRUE)
#' tokens(txt, remove_numbers = TRUE, remove_punct = FALSE)
#' tokens(txt, remove_numbers = FALSE, remove_punct = FALSE)
#' tokens(txt, remove_numbers = FALSE, remove_punct = FALSE, remove_separators = FALSE)
#' tokens(txt, remove_numbers = TRUE, remove_punct = TRUE, remove_url = TRUE)
#' 
#' # character level
#' tokens("Great website: http://textasdata.com?page=123.", what = "character")
#' tokens("Great website: http://textasdata.com?page=123.", what = "character", 
#'          remove_separators = FALSE)
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
#' txt <- char_tolower(c(mytext1 = "This is a short test sentence.",
#'                       mytext2 = "Short.",
#'                       mytext3 = "Short, shorter, and shortest."))
#' tokens(txt, remove_punct = TRUE)
#' ### removeFeatures(tokens(txt, remove_punct = TRUE), stopwords("english"))
#' 
#' # ngram tokenization
#' ### tokens(txt, remove_punct = TRUE, ngrams = 2)
#' ### tokens(txt, remove_punct = TRUE, ngrams = 2, skip = 1, concatenator = " ")
#' ### tokens(txt, remove_punct = TRUE, ngrams = 1:2)
#' # removing features from ngram tokens
#' ### removeFeatures(tokens(txt, remove_punct = TRUE, ngrams = 1:2), stopwords("english"))
tokens <-  function(x, what = c("word", "sentence", "character", "fastestword", "fasterword"),
                    remove_numbers = FALSE,
                    remove_punct = FALSE,
                    remove_symbols = FALSE,
                    remove_separators = TRUE,
                    remove_twitter = FALSE,
                    remove_hyphens = FALSE,
                    remove_url = FALSE,
                    ngrams = 1L,
                    skip = 0L,
                    concatenator = "_",
                    hash = TRUE,
                    verbose = quanteda_options("verbose"),
                    include_docvars = TRUE,
                    ...) {
    UseMethod("tokens")
}

#' @rdname tokens
#' @noRd
#' @export
tokens.character <- function(x, what = c("word", "sentence", "character", "fastestword", "fasterword"),
                             remove_numbers = FALSE,
                             remove_punct = FALSE,
                             remove_symbols = FALSE,
                             remove_separators = TRUE,
                             remove_twitter = FALSE,
                             remove_hyphens = FALSE,
                             remove_url = FALSE,
                             ngrams = 1L,
                             skip = 0L,
                             concatenator = "_",
                             hash = TRUE,
                             verbose = getOption("verbose"),  
                             include_docvars = TRUE, 
                             ...) {
    
    # deprecate older versions of the arguments
    args <- as.list(match.call())
    # get the arguments that match the old function calls
    args <- args[stringi::stri_detect_regex(names(args), "^remove")]
    # get any values of the arguments from the parent environment
    args <- lapply(args, function(a) eval(a, enclos = parent.frame()))
    # translate old into new
    remove_numbers <- deprecate_argument("removeNumbers", "remove_numbers", args)
    remove_punct <- deprecate_argument("removePunct", "remove_punct", args)
    remove_symbols <- deprecate_argument("removeSymbols", "remove_symbols", args)
    remove_separators <- deprecate_argument("removeSeparators", "remove_separators", args)
    remove_twitter <- deprecate_argument("removeTwitter", "remove_twitter", args)
    remove_hyphens <- deprecate_argument("removeHyphens", "remove_hyphens", args)
    remove_url <- deprecate_argument("removeURL", "remove_url", args)
    
    what <- match.arg(what)
    names_org <- names(x)
    attrs_org <- attributes(x)
    
    # disable remove_twitter if remove_punct = FALSE
    if (!remove_punct & remove_twitter) {
        remove_twitter <- FALSE
        warning("remove_twitter reset to FALSE when remove_punct = FALSE")
    }
    
    # warn about unused arguments
    if (length(added_args <- list(...)) & 
        !all(names(added_args) %in% paste0("remove", c("Numbers", "Punct", "Symbols", "Separators", "Twitter", "Hyphens", "URL", "simplify")))) {
        warning("Argument", ifelse(length(added_args) > 1, "s ", " "), names(added_args), " not used.", sep = "")
    }

    # deprecate "simplify"
    if ("simplify" %in% names(added_args)) warning("simplify no longer available")

    if (!is.integer(ngrams)) ngrams <- as.integer(ngrams)
    
    if (verbose) catm("Starting tokenization...\n")
    
    if (remove_twitter == FALSE & !(what %in% c("fastword", "fastestword"))) {
        if (verbose) catm("...preserving Twitter characters (#, @)\n")
        x <- stringi::stri_replace_all_fixed(x, c("#", "@"), c("_ht_", "_as_"), vectorize_all = FALSE)
    }
    
    time_start <- proc.time()
    
    # Split x into smaller blocks to reducre peak memory consumption
    blocks <- split(x, ceiling(seq_along(x) / 10000))
    result_blocks <- list()
    for (i in seq_along(blocks)) {
        
        if (verbose) catm("...tokenizing", i, "of" , length(blocks), "blocks\n")
        
        if (what %in% c("word", "fastestword", "fasterword")) {
            result_temp <- tokens_word(blocks[[i]], what, remove_numbers, remove_punct, remove_symbols, 
                                       remove_separators, remove_twitter, remove_hyphens, remove_url, verbose)
        } else if (what == "character") {
            result_temp <- tokens_character(blocks[[i]], what, remove_numbers, remove_punct, remove_symbols, 
                                            remove_separators, remove_twitter, remove_hyphens, remove_url, verbose)
        } else if (what == "sentence") {
            result_temp <- tokens_sentence(blocks[[i]], what, remove_numbers, remove_punct, remove_symbols, 
                                           remove_separators, remove_twitter, remove_hyphens, remove_url, verbose)
        } else {
            stop(what, " not implemented in tokens().")
        }
        
        if (remove_twitter == FALSE & !(what %in% c("fastword", "fastestword"))) {
            if (verbose) catm("...replacing Twitter characters (#, @)\n")
            result_temp <- lapply(result_temp, stringi::stri_replace_all_fixed, c("_ht_", "_as_"), c("#", "@"), vectorize_all = FALSE)
        }
        
        # Hash the tokens
        if (hash == TRUE) {
            if (verbose) catm("...serializing tokens ")
            if (i == 1) {
                result_blocks[[i]] <- tokens_hash(result_temp)
            } else {
                result_blocks[[i]] <- tokens_hash(result_temp, attr(result_blocks[[i - 1]], 'types'))
            }
            if (verbose) catm(length(attr(result_blocks[[i]], 'types')), 'unique types\n')
        } else {
            result_blocks[[i]] <- result_temp
        }
    }
    
    # Put all the blocked results togather
    result <- unlist(result_blocks, recursive = FALSE)
    
    if (hash == TRUE){
        class(result) <- c("tokens", "tokenizedTexts")
        types(result) <- attr(result_blocks[[length(result_blocks)]], 'types') # last block has all the types
    } else {
        class(result) <- c("tokenizedTexts", "list")
    }  
    if (!identical(ngrams, 1L)) {
        if (verbose) catm("...creating ngrams\n")
        result <- tokens_ngrams(result, n = ngrams, skip = skip, concatenator = concatenator)
    }
    if (verbose){
        catm("...total elapsed: ", (proc.time() - time_start)[3], "seconds.\n")
        catm("Finished tokenizing and cleaning", format(length(result), big.mark=","), "texts.\n")
    }
    
    names(result) <- names_org # stri_* destroys names, so put them back
    attr(result, "what") <- what
    attr(result, "ngrams") <- ngrams
    attr(result, "concatenator") <- ifelse(all.equal(ngrams, 1L)==TRUE, "", concatenator)
    attr(result, 'padding') <- FALSE

    # issue #607: remove @ # only if not part of Twitter names
    if (remove_punct & !remove_twitter) {
        if (hash) {
            result <- tokens_remove(result, "^#+$|^@+$", valuetype = "regex")
        } else {
            result <- suppressWarnings(removeFeatures(result, "^#+$|^@+$", valuetype = "regex"))
        }
    }

    return(result)
}


#' @rdname tokens
#' @export
#' @noRd
tokens.corpus <- function(x, ..., include_docvars = TRUE) {
    result <- tokens(texts(x), ...)
    if (include_docvars) {
        docvars(result) <- documents(x)[, which(names(documents(x)) != "texts"), drop = FALSE]
    } else {
        docvars(result) <- data.frame(matrix(nrow = ndoc(result), ncol = 1)[, -1, drop = FALSE],
                                      row.names = docnames(result))
    }
    return(result)
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
    result <- tokens_hash(x)
    attr(result, "what") <- "word"
    attr(result, "ngrams") <- 1L
    attr(result, "concatenator") <- ""
    attr(result, 'padding') <- FALSE
    class(result)[2] <- "tokenizedTexts"
    result
}

#' @export
#' @noRd
as.tokens.tokenizedTexts <- function(x) {
    NextMethod("as.tokens")
}

#' @rdname as.tokens
#' @param ... unused
#' @return \code{as.list} returns a simple list of characters from a
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
#'   \code{as.character} only.
#' @return \code{as.character} returns a character vector from a 
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
#' toks <- tokenize(char_tolower(txt), remove_punct = TRUE)
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
        types <- c(types_reserved, setdiff(types, types_reserved))
    }
    tokens <- lapply(x, fastmatch::fmatch, types) # serialize tokens 
    
    # Restore and add additional attributes
    attributes(tokens) <- attributes(x)
    attr(tokens, "types") <- stringi::stri_trans_nfc(types) # unicode normalization
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
    attributes(tokens) <- attributes(x)
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
    if (is.data.frame(attr(x, "docvars"))) {
        attr(tokens, "docvars") <- attr(x, "docvars")[i,]
    }
    if (length(tokens) == 1 && is.null(tokens[[1]])) return(tokens)
    attributes(tokens, FALSE) <- attributes(x)
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

#' @noRd
#' @export
docnames.tokens <- function(x) {
    names(x)
}



##
## ============== INTERNAL FUNCTIONS =======================================
##

tokens_word <- function(txt, what, remove_numbers, remove_punct, remove_symbols, remove_separators, 
                        remove_twitter, remove_hyphens, remove_url, verbose){
    
    # to preserve intra-word hyphens, replace with _hy_
    if (!remove_hyphens & remove_punct)
        txt <- stri_replace_all_regex(txt, "(\\b)[\\p{Pd}](\\b)", "$1_hy_$2")
    else if (remove_hyphens)
        txt <- stri_replace_all_regex(txt, "(\\b)[\\p{Pd}](\\b)", "$1 $2")
    
    if (remove_url) {
        if (verbose & remove_url) catm(", removing URLs")
        URLREGEX <- "https?:\\/\\/(www\\.)?[-a-zA-Z0-9@:%._\\+~#=]{1,256}\\.[a-z]{2,4}\\b([-a-zA-Z0-9@:%_\\+.~#?&//=]*)"
        txt <- stri_replace_all_regex(txt, URLREGEX, "")
    }
    
    if (what == "fasterword" | what == "fastestword") {
        
        regexToEliminate <- paste(ifelse(remove_numbers, "\\b\\d+\\b", ""),
                                  ifelse(remove_punct, paste0("(?![", ifelse(remove_twitter, "_", "@#_"),  "])[\\p{P}]"), ""),
                                  ifelse(remove_symbols, "[\\p{S}]", ""),
                                  sep = "|")
        
        # catm("\n..", regexToEliminate, "..\n", sep = "")
        regexToEliminate <- gsub("^\\|+", "", regexToEliminate)
        regexToEliminate <- gsub("\\|+$", "", regexToEliminate)
        # catm("\n..", regexToEliminate, "..\n", sep = "")
        if (gsub("|", "", regexToEliminate, fixed = TRUE) != "")
            txt <- stri_replace_all_regex(txt, regexToEliminate, "")
        
        if (verbose & remove_punct==TRUE) catm(", ", what, " tokenizing", sep="")
        if (what=="fastestword")
            tok <- stringi::stri_split_fixed(txt, " ")
        else if (what=="fasterword")
            tok <- stringi::stri_split_charclass(txt, "\\p{WHITE_SPACE}")

    } else {
        tok <- stringi::stri_split_boundaries(txt, 
                                              type = "word", 
                                              skip_word_none = (remove_punct | remove_symbols), # this is what obliterates currency symbols, Twitter tags, and URLs
                                              skip_word_number = remove_numbers) # but does not remove 4u, 2day, etc.
        # Remove separators if option is TRUE
        if (remove_separators & !remove_punct) {
            tok <- lapply(tok, function(x) x[!stri_detect_regex(x, "^\\s$")])
        }
    }
    
    # Put hyphens back the fast way
    if (!remove_hyphens & remove_punct)
        tok <- lapply(tok, stri_replace_all_fixed, "_hy_", "-")
    
    tok <- qatd_cpp_chars_remove(tok, "")
    return(tok)
}

tokens_sentence <- function(txt, what, remove_numbers, remove_punct, remove_symbols, remove_separators, 
                            remove_twitter, remove_hyphens, remove_url, verbose){
    
    if (remove_numbers || remove_punct || remove_symbols || remove_twitter || remove_hyphens || remove_url) 
        warning("remove_numbers, remove_punct, remove_symbols, remove_twitter, remove_hyphens or remove_url is not used for \"sentence\" segmentation")
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

tokens_character <- function(txt, what, remove_numbers, remove_punct, remove_symbols, remove_separators, 
                             remove_twitter, remove_hyphens, remove_url, verbose){
    if (remove_numbers || remove_twitter || remove_hyphens || remove_url) 
        warning("remove_numbers, remove_twitter, remove_hyphens or remove_url is not used for \"character\" tokenization")
    
    # note: does not implement remove_numbers
    tok <- stringi::stri_split_boundaries(txt, type = "character")
    if (remove_punct) {
        if (verbose) catm("...removing punctuation.\n")
        tok <- lapply(tok, function(x){
            x <- stringi::stri_replace_all_charclass(x, "[\\p{P}]", "")
            x <- x[which(x != "")]
            return(x)    
        })
    } 
    if (remove_symbols) {
        if (verbose) catm("...removing symbols.\n")
        tok <- lapply(tok, function(x){
            x <- stringi::stri_replace_all_charclass(x, "[\\p{S}]", "")
            x <- x[which(x != "")]
            return(x)    
        })
    } 
    if (remove_separators) {
        if (verbose) catm("...removing separators.\n")
        tok <- lapply(tok, function(x){
            x <- stringi::stri_subset_regex(x, "^\\p{Z}$", negate = TRUE)
            x <- x[which(x != "")]
            return(x)    
        })
    }
    return(tok)
    
}



#' recompile a hashed tokens object
#' 
#' This function recompiles a hashed tokens object when the vocabulary has been
#' changed in a way that makes some of its types identical, such as lowercasing
#' when a lowercased version of the type already exists in the hash table, or
#' introduces gaps in the integer map of the types.  It also reindexes the types
#' atttribute to account for types that may have become duplicates, through a
#' procedure such as stemming or lowercasing; or the addition of new tokens
#' through compounding.
#' @param x the \link{tokens} object to be recompiled
#' @param method \code{"C++"} for C++ implementation or \code{"R"} for an older
#'   R-based method
#' @examples 
#' # lowercasing
#' toks1 <- tokens(c(one = "a b c d A B C D",
#'                  two = "A B C d"))
#' attr(toks1, "types") <- char_tolower(attr(toks1, "types"))
#' unclass(toks1)
#' unclass(quanteda:::tokens_hashed_recompile(toks1))
#' 
#' # stemming
#' toks2 <- tokens("Stemming stemmed many word stems.")
#' unclass(toks2)
#' unclass(quanteda:::tokens_hashed_recompile(tokens_wordstem(toks2)))
#' 
#' # compounding
#' toks3 <- tokens("One two three four.")
#' unclass(toks3)
#' unclass(tokens_compound(toks3, "two three"))
#' 
#' # lookup
#' dict <- dictionary(list(test = c("one", "three")))
#' unclass(tokens_lookup(toks3, dict))
#'
#' # empty pads
#' unclass(tokens_select(toks3, dict))
#' unclass(tokens_select(toks3, dict, pad = TRUE))
#' 
#' # ngrams
#' unclass(tokens_ngrams(toks3, n = 2:3))
#' 
#' @keywords internal tokens
#' @author Kenneth Benoit and Kohei Watanabe
tokens_hashed_recompile <- function(x, method = c("C++", "R")) {
    
    method <- match.arg(method)
    attrs_input <- attributes(x)
    
    if (method == "C++") {
        x <- qatd_cpp_tokens_recompile(x, types(x))
        attributes(x, FALSE) <- attrs_input
        return(x)
    }
    
    index_unique <- unique(unlist(x, use.names = FALSE))
    padding <- (index_unique == 0)
    attrs_input$padding <- any(padding) # add padding flag
    index_unique <- index_unique[!padding] # exclude padding
    
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
    Encoding(types(x)) <- "UTF-8"
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


#' @rdname tokens
#' @param t1 tokens one to be added
#' @param t2 tokens two to be added
#' @examples 
#' toks1 <- tokens(data_corpus_inaugural[1:5])
#' toks2 <- tokens(data_corpus_inaugural[21:25])
#' toks3 <- toks1 + toks2
#' 
#' @export
`+.tokens` <- function(t1, t2) {
    if (length(intersect(docnames(t1), docnames(t2))))
        stop('Document names are duplicated')
    docvars(t1) <- docvars(t2) <- NULL
    types2 <- types(t2)
    types1 <- types(t1)
    t2 <- unclass(t2)
    t1 <- unclass(t1)
    t2 <- lapply(t2, function(x, y) x + y, length(types1)) # shift IDs
    t1 <- c(t1, t2)
    class(t1) <- c('tokens', 'tokenizedTexts')
    types(t1) <- c(types1, types2)
    tokens_hashed_recompile(t1)
}

#' @rdname tokens
#' @param recursive logical used by `c()` method, always set to `FALSE`
#' @examples 
#' 
#' toks1 <- tokens(data_corpus_inaugural[1:5])
#' toks2 <- tokens(data_corpus_inaugural[21:25])
#' toks3 <- tokens(data_corpus_inaugural[41:45])
#' summary(c(toks1, toks2, toks3))
#' 
#' @export
c.tokens <- function(..., recursive = FALSE) {
    x <- list(...)
    if (length(x) == 1) return(x[[1]])
    result <- x[[1]] + x[[2]]
    if (length(x) == 2) return(result)
    for (i in 3:length(x))
        result <- result + x[[i]]
    return(result)
}


