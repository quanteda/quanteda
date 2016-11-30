
#' tokenize a set of texts
#'
#' Tokenize the texts from a character vector or from a corpus.
#' @rdname tokenize
#' @aliases tokenise
#' @param x text(s) or corpus to be tokenized
#' @param ... additional arguments not used
#' @return A list of length \code{\link{ndoc}(x)} of the tokens found in each text.
#' @author Ken Benoit and Paul Nulty
#' @export
#' @examples 
#' # same for character vectors and for lists
#' tokensFromChar <- tokenize(data_char_inaugural[1:3])
#' tokensFromCorp <- tokenize(corpus_subset(data_corpus_inaugural, Year<1798))
#' identical(tokensFromChar, tokensFromCorp)
#' str(tokensFromChar)
#' @export
tokenize <- function(x, ...) {
    UseMethod("tokenize")
}

#' @rdname tokenize
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
#' @param simplify if \code{TRUE}, return a character vector of tokens rather 
#'   than a list of length \code{\link{ndoc}(texts)}, with each element of the 
#'   list containing a character vector of the tokens corresponding to that 
#'   text.
#' @param verbose if \code{TRUE}, print timing messages to the console; off by 
#'   default
#' @import stringi
#' @details The tokenizer is designed to be fast and flexible as well as to 
#'   handle Unicode correctly. Most of the time, users will construct \link{dfm}
#'   objects from texts or a corpus, without calling \code{tokenize()} as an 
#'   intermediate step.  Since \code{tokenize()} is most likely to be used by 
#'   more technical users, we have set its options to default to minimal 
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
#' @return a \strong{tokenizedText} (S3) object, essentially a list of character
#'   vectors. If \code{simplify = TRUE} then return a single character vector.
#' @export
#' @seealso \code{\link{ngrams}}
#' @examples 
#' # returned as a list
#' head(tokenize(data_char_inaugural[57])[[1]], 10)
#' # returned as a character vector using simplify=TRUE
#' head(tokenize(data_char_inaugural[57], simplify = TRUE), 10)
#' 
#' # removing punctuation marks and lowecasing texts
#' head(tokenize(toLower(data_char_inaugural[57]), simplify = TRUE, removePunct = TRUE), 30)
#' # keeping case and punctuation
#' head(tokenize(data_char_inaugural[57], simplify = TRUE), 30)
#' # keeping versus removing hyphens
#' tokenize("quanteda data objects are auto-loading.", removePunct = TRUE)
#' tokenize("quanteda data objects are auto-loading.", removePunct = TRUE, removeHyphens = TRUE)
#' # keeping versus removing symbols
#' tokenize("<tags> and other + symbols.", removeSymbols = FALSE)
#' tokenize("<tags> and other + symbols.", removeSymbols = TRUE)
#' tokenize("<tags> and other + symbols.", removeSymbols = FALSE, what = "fasterword")
#' tokenize("<tags> and other + symbols.", removeSymbols = TRUE, what = "fasterword")
#' 
#' ## examples with URLs - hardly perfect!
#' txt <- "Repo https://githib.com/kbenoit/quanteda, and www.stackoverflow.com."
#' tokenize(txt, removeURL = TRUE, removePunct = TRUE)
#' tokenize(txt, removeURL = FALSE, removePunct = TRUE)
#' tokenize(txt, removeURL = FALSE, removePunct = TRUE, what = "fasterword")
#' tokenize(txt, removeURL = FALSE, removePunct = FALSE, what = "fasterword")
#' 
#' 
#' ## MORE COMPARISONS
#' txt <- "#textanalysis is MY <3 4U @@myhandle gr8 #stuff :-)"
#' tokenize(txt, removePunct = TRUE)
#' tokenize(txt, removePunct = TRUE, removeTwitter = TRUE)
#' #tokenize("great website http://textasdata.com", removeURL = FALSE)
#' #tokenize("great website http://textasdata.com", removeURL = TRUE)
#' 
#' txt <- c(text1="This is $10 in 999 different ways,\n up and down; left and right!", 
#'          text2="@@kenbenoit working: on #quanteda 2day\t4ever, http://textasdata.com?page=123.")
#' tokenize(txt, verbose = TRUE)
#' tokenize(txt, removeNumbers = TRUE, removePunct = TRUE)
#' tokenize(txt, removeNumbers = FALSE, removePunct = TRUE)
#' tokenize(txt, removeNumbers = TRUE, removePunct = FALSE)
#' tokenize(txt, removeNumbers = FALSE, removePunct = FALSE)
#' tokenize(txt, removeNumbers = FALSE, removePunct = FALSE, removeSeparators = FALSE)
#' tokenize(txt, removeNumbers = TRUE, removePunct = TRUE, removeURL = TRUE)
#' 
#' # character level
#' tokenize("Great website: http://textasdata.com?page=123.", what = "character")
#' tokenize("Great website: http://textasdata.com?page=123.", what = "character", 
#'          removeSeparators = FALSE)
#' 
#' # sentence level         
#' tokenize(c("Kurt Vongeut said; only assholes use semi-colons.", 
#'            "Today is Thursday in Canberra:  It is yesterday in London.", 
#'            "Today is Thursday in Canberra:  \nIt is yesterday in London.",
#'            "To be?  Or\nnot to be?"), 
#'           what = "sentence")
#' tokenize(data_char_inaugural[c(2,40)], what = "sentence", simplify = TRUE)
#' 
#' # removing features (stopwords) from tokenized texts
#' txt <- toLower(c(mytext1 = "This is a short test sentence.",
#'                  mytext2 = "Short.",
#'                  mytext3 = "Short, shorter, and shortest."))
#' tokenize(txt, removePunct = TRUE)
#' removeFeatures(tokenize(txt, removePunct = TRUE), stopwords("english"))
#' 
#' # ngram tokenization
#' tokenize(txt, removePunct = TRUE, ngrams = 2)
#' tokenize(txt, removePunct = TRUE, ngrams = 2, skip = 1, concatenator = " ")
#' tokenize(txt, removePunct = TRUE, ngrams = 1:2)
#' # removing features from ngram tokens
#' removeFeatures(tokenize(txt, removePunct = TRUE, ngrams = 1:2), stopwords("english"))
tokenize.character <- function(x, what=c("word", "sentence", "character", "fastestword", "fasterword"),
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
                               verbose = FALSE,  ## FOR TESTING
                               ...) {
    
    what <- match.arg(what)

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
            result <- lapply(result, function(x) x <- x[which(x != "")])
            
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
        stop(what, " not implemented in tokenize().")
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

        
    if (simplify == FALSE) {
        # stri_* destroys names, so put them back
        startTimeClean <- proc.time()
        if (verbose) catm("  ...replacing names")
        names(result) <- names(x)
        if (verbose) catm("...total elapsed: ", (proc.time() - startTimeClean)[3], "seconds.\n")
        # make this an S3 class item, if a list
        if (!is.tokenizedTexts(result))
            class(result) <- c("tokenizedTexts", class(result))
        attr(result, "what") <- what
        attr(result, "ngrams") <- ngrams
        attr(result, "concatenator") <- ifelse(all.equal(ngrams, 1L)==TRUE, "", concatenator)
    } else {
        # or just return the tokens as a single character vector
        if (verbose) catm("  ...unlisting results.\n")
        result <- unlist(result, use.names = FALSE)
        # clear attributes
        attributes(result) <- NULL
    }

    if (verbose) 
        catm("Finished tokenizing and cleaning", format(length(result), big.mark=","), "texts.\n") 
        #, with a total of", format(length(unlist(result)), big.mark=","), "tokens.\n")
        
    result
}

#' @rdname tokenize
#' @export
tokenize.corpus <- function(x, ...) {
    # get the settings for clean from the corpus and use those, 
    # unless more specific arguments are passed -- ADD THE ABILITY TO PASS THESE
    # need to include sep in this list too 
    tokenize(texts(x), ...)
}


#' @export
#' @description \code{is.tokenizedTexts} returns \code{TRUE} if the object is of class tokenizedTexts, \code{FALSE} otherwise.
#' @rdname tokenize
is.tokenizedTexts <- function(x) {
    ifelse("tokenizedTexts" %in% class(x), TRUE, FALSE)
}

#' print a tokenizedTexts objects
#' 
#' print method for a \link{tokenize}dText object
#' @param x a tokenizedText object created by \link{tokenize}
#' @param ... further arguments passed to base print method
#' @export
#' @method print tokenizedTexts
print.tokenizedTexts <- function(x, ...) {
    cat(class(x)[1], " from ", ndoc(x), " document", 
        ifelse(ndoc(x) > 1, "s", ""), ".\n", sep = "")
    class(x) <- "listof"
    print(x, ...)
}

#' @rdname tokenize
#' @details \code{as.tokenizedTexts} coerces a list of character tokens to a tokenizedText class object, 
#' making the methods available for this object type available to this object.
#' @export
as.tokenizedTexts <- function(x, ...) {
    UseMethod("as.tokenizedTexts")
}

#' @rdname tokenize
#' @export
as.tokenizedTexts.list <- function(x, ...) {
    if (!is.list(x) || (!all(sapply(x, function(l) all(is.character(l))))))
            stop("input must be a list of character types")
    class(x) <- c("tokenizedTexts", class(x))
    attr(x, "what") <- "user"
    attr(x, "ngrams") <- 1L
    attr(x, "concatenator") <- ""
    x
}

#' @export
#' @rdname ndoc
ndoc.tokenizedTexts <- function(x) {
    length(x)
}


