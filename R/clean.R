## SHOULD IMPLEMENT PRESERVATION OF URLs

#' simple cleaning of text before processing
#' 
#' \code{clean} removes punctuation and digits from text, using the regex 
#' character classes for punctuation and digits. \code{clean} uses the standard R
#' function \code{tolower} to convert the text to lower case. Each of these 
#' steps is optional, but switched on by default, so for example, to remove 
#' punctuation and convert to lower, but keep digits, the command would be: 
#' \code{clean(mytexts, removeDigits=FALSE)}
#' @rdname clean
#' @param x The object to be cleaned. Can be either a character vector or a 
#'   corpus object. If x is a corpus, \code{clean} returns the corpus containing 
#'   the cleaned texts.
#' @param removeDigits remove numbers if \code{TRUE}
#' @param removePunct remove punctuation if \code{TRUE}
#' @param toLower convert text to lower case \code{TRUE}
#' @param removeTwitter if \code{FALSE}, do not remove \code{@@} or \code{#}
#' @param removeURL removes URLs (web addresses starting with \code{http:} or \code{https:}), based 
#' on a regular expression from \url{http://daringfireball.net/2010/07/improved_regex_for_matching_urls}
#' @param removeAdditional additional characters to remove (\link[=regex]{regular expression})
#' @param ... additional parameters
#' @return A character vector equal in length to the original texts, after cleaning.
#' @import stringi
#' @useDynLib quanteda
#' @export
#' @examples
#' clean("This is 1 sentence with 2.0 numbers in it, and one comma.", removeDigits=FALSE)
#' clean("This is 1 sentence with 2.0 numbers in it, and one comma.", toLower=FALSE)
#' clean("We are his Beliebers, and him is #ourjustin @@justinbieber we luv u", removeTwitter=TRUE)
#' clean("Collocations can be represented as inheritance_tax using the _ character.")
#' clean("But under_scores can be removed with removeAdditional.", removeAdditional="[_]")
#' clean("This is a $1,500,000 budget and $20bn cash plus 50¢.")
#' clean("This is a $1,500,000 budget and $20bn cash plus 50¢.", removeDigits=FALSE)
#' clean("URL regex from http://daringfireball.net/2010/07/improved_regex_for_matching_urls.")
#' 
#' # for a vector of texts
#' clean(c("This is 1 sentence with 2.0 numbers in it, and one comma.", 
#'         "$1.2 billion was spent on text analysis in 2014."))
#' @export
clean <- function(x, ...) {
    UseMethod("clean")
}


#' @rdname clean
#' @export
clean.character <- function(x, removeDigits=TRUE, removePunct=TRUE, toLower=TRUE, 
                            removeAdditional=NULL, removeTwitter=FALSE, removeURL=TRUE, ...) {
    ## THIS NEEDS TO LOOK AT SETTINGS BEFORE MAKING A DECISION
    #if (!(removeDigits | removePunct | toLower) & is.null(removeAdditional)) {
    #    warning("  clean: text unchanged")
    #}
    
    # convert "curly quotes"
    x <- stri_replace_all_regex(x, "[\u201C\u201D]", "\"")
    x <- stri_replace_all_regex(x, "[\u2018\u2019]", "\'")
    
    urlregex <- "(?i)\\b((?:[a-z][\\w-]+:(?:/{1,3}|[a-z0-9%])|www\\d{0,3}[.]|[a-z0-9.\\-]+[.][a-z]{2,4}/)(?:[^\\s()<>]+|\\(([^\\s()<>]+|(\\([^\\s()<>]+\\)))*\\))+(?:\\(([^\\s()<>]+|(\\([^\\s()<>]+\\)))*\\)|[^\\s`!()\\[\\]{};:\\'\".,<>?]))"
    # see http://daringfireball.net/2010/07/improved_regex_for_matching_urls
    if (removeURL) {
        x <- stri_replace_all_regex(x, urlregex, "", perl=TRUE)
    } else {
        # NEED TO PRESERVE THESE SOMEHOW
    }
    
    # change typographic dash variations to a hyphen: There Can Be Only One
    x <- stri_replace_all_regex(x, "[\u2013\u2014]", "-")
    
    # remove tabs, newlines, and common cruft from word-processors
    x <- stri_replace_all_regex(x, "\\f|[\u2026\u22EF]|\\t|\\n", " ")
    
    if (removePunct) {
        # use "negative lookahead" to keep Twitter symbols, always keep "_"
        # remove other punctuation from POSIX [:punct:]
        remove <- paste("(?![",
                        ifelse(removeTwitter, "_", "@#_"),
                        "])[[:punct:]]",  
                        ifelse(!is.null(removeAdditional), paste("|", removeAdditional, sep=""), ""),
                        sep="")
        x <- stri_replace_all_regex(x, remove, "", perl=TRUE)
    }
    
    if (removeDigits) 
        # amended regex removes currency stuff better, e.g.
        # clean("This is a $5m watch and $20bn budget and $100,000 in cash plus a $5 cigar.")
        #
        # 2nd part in alternation removes 1st 2nd 31st 43rd 3bis 101th etc.
        #
        # third part in alternation means don't remove digits if in a word, e.g. 4sure, crazy8
        # the third stuff is to remove thousands separators, e.g. 1,000,000 or 1.000.000
        # clean("nodigits crazy8 4sure 67 89b 1,000,000 1.023.496")
        # note: \u00A3 is pound sign, \u20AC is euro sign, \u00A2 is the cent sign
        x <- stri_replace_all_regex(x, "[$\u00A3\u20AC\u00A2][[:digit:]]\\w*|\\b[[:digit:]]+(st|nd|rd|d|th|bis)\\b|\\b([[:digit:]]+[,.]?)+\\b", "")
    if (toLower) 
        x <- stri_trans_tolower(x)
    
    #     if (!is.null(removeAdditional))
    #         x <- gsub(removeAdditional, "", x)
    
    # convert 2+ multiple whitespaces into one
    x <- stri_replace_all_regex(x, "\\s{2,}", " ",  perl=TRUE)
    # remove leading and trailing whitespace and return
    x <- stri_trim_both(x)
    return(x)
}


#' @rdname clean
#' @export
clean.corpus <- function(x, removeDigits=TRUE, removePunct=TRUE, toLower=TRUE,
                         removeAdditional=NULL, removeTwitter=FALSE, ...) {
    clean(texts(x), removeDigits=removeDigits, removePunct=removePunct, 
          toLower=toLower, removeAdditional=removeAdditional, ...)
}

#' stem words
#' 
#' Apply a stemmer to words.  This is a wrapper to \link[SnowballC]{wordStem} 
#' designed to allow this function to be called without loading the entire 
#' \pkg{SnowballC} package.  \link[SnowballC]{wordStem}  uses Martin Porter's 
#' stemming algorithm and the C libstemmer library generated by Snowball.
#' @param x a character vector or corpus, whose word stems are to be removed
#' @param language the name of a recognized language, as returned by 
#'   \link[SnowballC]{getStemLanguages}, or a two- or three-letter ISO-639 code 
#'   corresponding to one of these languages (see references for the list of 
#'   codes)
#' @return A character vector with as many elements as there are in the input 
#'   vector with the corresponding elements being the stem of the word. Elements
#'   of the vector are converted to UTF-8 encoding before the stemming is 
#'   performed, and the returned elements are marked as such when they contain 
#'   non-ASCII characters.
#' @seealso \link[SnowballC]{wordStem}
#'   
#' @references \url{http://snowball.tartarus.org/}
#'   
#'   \url{http://www.iso.org/iso/home/standards/language_codes.htm} for the 
#'   ISO-639 language codes
#' @export
#' @examples
#' # Simple example
#' wordstem(c("win", "winning", "wins", "won", "winner"))
wordstem <- function(x, language = "porter") {
    UseMethod("wordstem")
}

#' @rdname wordstem
#' @export
wordstem.character <- function(x, language = "porter") {
    SnowballC::wordStem(x, language)
}

## Not clear whether this should return a character vector
## or a stemmed corpus
# @rdname wordstem
# @export
# wordstem.corpus <- function(x, language = "porter") {
#     SnowballC::wordStem(texts(x), language)
# }


# rdname wordstem
# export
#  from https://sites.google.com/site/motazsite/arabic/arlightstemmerlucene.jar
#  source: https://sites.google.com/site/motazsite/arabic/arlightstemmerlucene-src.7z
# wordstemArabic <- function(x) {
#     require(rJava)
#     .jinit("java/ArLightStemmerLucene.jar")
#     hjw <- .jnew("ArLightStemmerLucene")     # create instance of ArLightStemmerLucene class
#     out <- .jcall(hjw, "S", "main", x)  # invoke sayHello method
#     return(out)
# }
# 


# clean("This is a $10m watch and \u20ac20bn budget and $100,000 in cash plus \u00a250.")

urlregex <- "(?i)\\b((?:[a-z][\\w-]+:(?:/{1,3}|[a-z0-9%])|www\\d{0,3}[.]|[a-z0-9.\\-]+[.][a-z]{2,4}/)(?:[^\\s()<>]+|\\(([^\\s()<>]+|(\\([^\\s()<>]+\\)))*\\))+(?:\\(([^\\s()<>]+|(\\([^\\s()<>]+\\)))*\\)|[^\\s`!()\\[\\]{};:\\'\".,<>?]))"


#' @rdname clean
#' @details \code{cleanC} is KB's adaptation from KW's code for tokenization and cleaning, 
#' for testing.
#' @importFrom Rcpp evalCpp
#' @useDynLib quanteda
#' @examples
#' cleanC("This is 1 sentence with 2.0 numbers in it, and one comma.", removeDigits=FALSE)
#' cleanC("This is 1 sentence with 2.0 numbers in it, and one comma.", toLower=FALSE)
#' cleanC("We are his Beliebers, and him is #ourjustin @@justinbieber we luv u", removeTwitter=TRUE)
#' cleanC("Collocations can be represented as inheritance_tax using the _ character.")
#' cleanC("But under_scores can be removed with removeAdditional.", removeAdditional="[_]")
#' cleanC("This is a $1,500,000 budget and $20bn cash and a $5 cigar.")
#' cleanC("This is a $1,500,000 budget and $20bn cash and a $5 cigar.", removeDigits=FALSE)
#' clean("URL regex from http://daringfireball.net/2010/07/improved_regex_for_matching_urls.")
#' 
#' # for a vector of texts
#' clean(c("This is 1 sentence with 2.0 numbers in it, and one comma.", 
#'         "$1.2 billion was spent on text analysis in 2014."))
#' 
#' \donttest{# on a single long text
#' mobydick <- texts(corpus(textfile("~/Dropbox/QUANTESS/corpora/project_gutenberg/pg2701.txt")))
#' system.time(tmp <- cleanC(mobydick)) # .218 seconds
#' system.time(tmp <- clean(mobydick))  # .776 seconds
#' 
#' # on a longer set of texts (34,070 texts)
#' load('~/Dropbox/QUANTESS/Manuscripts/Collocations/Corpora/lauderdaleClark/Opinion_files.RData')
#' txts <- unlist(Opinion_files[1]); names(txts) <- NULL
#' system.time(tmp <- sapply(txts, cleanC)) # about 20.5 seconds
#' \dontrun{system.time(tmp <- sapply(txts, clean))  # about forever: 647.502 seconds}
#' }
#' @export
cleanC <- function(x, removeDigits=TRUE, removePunct=TRUE, toLower=TRUE, 
                   removeAdditional=NULL, removeTwitter=FALSE, removeURL=TRUE, ...) {
    UseMethod("clean")
    
    ## EVENTUALLY, MOVE THIS TO stringi::stri_replace_all()
    
    # to match the NULL default in clean()
#    if (is.null(removeAdditional)) removeAdditional <- "" 

#     sapply(x, cleancpp, removeDigits=removeDigits, removePunct=removePunct, 
#            toLower=toLower, 
#            removeAdditional=removeAdditional, 
#            removeTwitter=removeTwitter, removeURL=removeURL,
#            USE.NAMES = FALSE)
}


