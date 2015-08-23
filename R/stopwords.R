.stopwords <- NULL

#' remove features from an object
#' 
#' This function removes features from a variety of objects, such as text, a dfm, or
#' a list of collocations.  The most common usage for \code{removeFeatures} will be
#' to eliminate stop words from a text or text-based object.  Some commonly used
#' built-in stop words can be accessed through \code{\link{stopwords}}.
#' 
#' Because we believe the user should take full responsibility for any features
#' that are removed, we do not provide a default list.  Use \code{\link{stopwords}} instead.
#' 
#' @param x object from which stopwords will be removed
#' @param stopwords character vector of features to remove.  Now requires an explicit
#' list to be supplied, for instance \code{stopwords("english")}.
#' @param verbose if \code{TRUE} print message about how many features were removed
#' @param pos indexes of word position if called on collocations: remove if word \code{pos}
#' is a stopword
#' @param ... additional arguments for some methods (such as \code{pos} for \link{collocations})
#' @return an object with stopwords removed
#' @name removeFeatures
#' @export
#' @author Kenneth Benoit
#' @seealso \link{stopwords}
#' @examples
#' ## examples for character objects
#' someText <- tokenize(c(text1 = "Here's some text containing words we want to remove."))
#' removeFeatures(someText, stopwords("english"))
#' removeFeatures(someText, stopwords("SMART"))
#' removeFeatures(someText, c("some", "want"))
#' someText <- tokenize(c(text1 = "Here's some text containing words we want to remove.",
#'                        text2 = "A second sentence with a few stopwords."))
#' removeFeatures(someText, stopwords("english"))
#' 
#' ## for tokenized texts 
#' txt <- c(wash1 <- "Fellow citizens, I am again called upon by the voice of my country to 
#'                    execute the functions of its Chief Magistrate.",
#'          wash2 <- "When the occasion proper for it shall arrive, I shall endeavor to express
#'                    the high sense I entertain of this distinguished honor.")
#' removeFeatures(tokenize(txt), stopwords("english"))
#' 
#' itText <- tokenize("Ecco alcuni di testo contenente le parole che vogliamo rimuovere.", 
#'                    removePunct = TRUE)
#' removeFeatures(itText, stopwords("italian"), case_insensitive = TRUE)
#' 
#' ## example for dfm objects
#' mydfm <- dfm(ukimmigTexts, verbose=FALSE)
#' removeFeatures(mydfm, stopwords("english"))
#' 
#' ## example for collocations
#' (myCollocs <- collocations(inaugTexts[1:3], top=20))
#' removeFeatures(myCollocs, stopwords("english", verbose=FALSE))
removeFeatures <- function(x, stopwords=NULL, verbose=TRUE, ...) {
    UseMethod("removeFeatures")
}


# # @rdname removeFeatures
# # @export
# removeFeatures.character <- function(x, stopwords=NULL, verbose=TRUE, ...) {
#     if (is.null(stopwords))
#         stop("Must supply a character vector of stopwords, e.g. stopwords(\"english\")")
#     # tokenize while keeping spaces, and send to removeFeatures.tokenizedTexts
#     ret <- removeFeatures(tokenize(x, removePunct = FALSE, removeNumbers = FALSE, removeSeparators = FALSE, removeTwitter = FALSE),
#                           stopwords)
#     # remove first of two whitespaces
#     ret <- lapply(ret, function(x) x[!(stringi::stri_detect_charclass(x, "[\\p{Zs}]") & stringi::stri_detect_charclass(c(x[-1], " "), "[\\p{Zs}]"))])
#     print(length(ret))
#     tmpFun <- function(y) {
#         if (is.na(y[1])) return("")
#         if (y[1] == " ") 
#             return(y[-1])
#         else 
#             return(y)
#     }
#     ret <- lapply(ret, tmpFun)
#     # paste back into a string and return
#     sapply(ret, paste, collapse = "")
# }

#' @rdname removeFeatures
#' @export
removeFeatures.tokenizedTexts <- function(x, stopwords=NULL, verbose=TRUE, ...) {
    if (is.null(stopwords))
        stop("Must supply a character vector of stopwords, e.g. stopwords(\"english\")")
    # much faster than any regex method
    result <- lapply(x, function(x) x[which(!(toLower(x) %in% stopwords))])
    class(result) <- c("tokenizedTexts", class(result))
    result
}
    
    
#' @rdname removeFeatures
#' @export
removeFeatures.dfm <- function(x, stopwords = NULL, verbose = TRUE, ...) {
    selectFeatures(x, features = stopwords, selection = "remove", verbose = verbose, ...)
}


### now optimized for speed using data.table
#' @rdname removeFeatures
#' @export
removeFeatures.collocations <- function(x, stopwords=NULL, verbose=TRUE, pos=c(1,2,3), ...) {
    word <- word1 <- word2 <- word3 <- NULL
    if (is.null(stopwords))
        stop("Must supply a character vector of stopwords, e.g. stopwords(\"english\")")
    if (!all(pos %in% 1:3))
        stop("pos for collocation position can only be 1, 2, and/or 3")
    nstart <- nrow(x)
    stopwordTable <- data.table(word=stopwords, remove=1)
    setkey(stopwordTable, word)
    x$order <- 1:nrow(x)
    
    if (3 %in% pos) {
        setnames(stopwordTable, 1, "word3")
        setkey(x, word3)
        x <- stopwordTable[x]
        x <- x[is.na(remove)]
        x[, remove:=NULL]
    }
    if (2 %in% pos) {
        setnames(stopwordTable, 1, "word2")
        setkey(x, word2)
        x <- stopwordTable[x]
        x <- x[is.na(remove)]
        x[, remove:=NULL]
    }
    if (1 %in% pos) {
        setnames(stopwordTable, 1, "word1")
        setkey(x, word1)
        x <- stopwordTable[x]
        x <- x[is.na(remove)]
        x[, remove:=NULL]
    }
    setorder(x, order)
    setcolorder(x, c("word1", "word2", "word3", names(x)[4:ncol(x)]))
    x[, order:=NULL]
    nend <- nrow(x)
    if (verbose) cat("Removed ", format(nstart - nend, big.mark=","),  
                     " (", format((nstart - nend)/nstart*100, digits=3),
                     "%) of ", format(nstart, big.mark=","), 
                     " collocations containing one of ", 
                     length(stopwords), " stopwords.\n", sep="")
    x
}



#' @rdname removeFeatures
#' @export
stopwordsRemove <- function(x, stopwords=NULL, verbose=TRUE) {
    cat("stopwordsRemove is deprecated, use removeFeatures instead.")
    UseMethod("removeFeatures")
}



#' access built-in stopwords
#' 
#' This function retrieves stopwords from the type specified in the \code{kind} 
#' argument and returns the stopword list as a character vector The default is 
#' English.
#' 
#' The stopword list are SMART English stopwords from the SMART information
#' retrieval system (obtained from 
#' http://jmlr.csail.mit.edu/papers/volume5/lewis04a/a11-smart-stop-list/english.stop)
#' and a set of stopword lists from the Snowball stemmer project in different
#' languages (obtained from 
#' http://svn.tartarus.org/snowball/trunk/website/algorithms/*/stop.txt).
#' Supported languages are arabic, danish, dutch, english, finnish, french,
#' german, hungarian, italian, norwegian, portuguese, russian, spanish, and 
#' swedish. Language names are case sensitive.
#' @rdname stopwords
#' @section A note of caution:
#'  Stop words are an arbitrary choice imposed by the
#'   user, and accessing a pre-defined list of words to ignore does not mean
#'   that it will perfectly fit your needs. You are strongly encourged to
#'   inspect the list and to make sure it fits your particular requirements.
#' @param kind The pre-set kind of stopwords (as a character string).  Allowed
#'   values are \code{english}, \code{SMART}, \code{danish}, \code{french},
#'   \code{hungarian}, \code{norwegian}, \code{russian}, \code{swedish},
#'   \code{catalan}, \code{dutch}, \code{finnish}, \code{german},
#'   \code{italian}, \code{portuguese}, \code{spanish}, \code{arabic}
#' @param verbose if \code{FALSE}, suppress the annoying warning note
#' @return a character vector of stopwords
#' @name stopwords
#' @export
#' @examples
#' stopwords("english")[1:5]
#' stopwords("italian")[1:5]
#' stopwords("arabic")[1:5]
stopwords <- function(kind="english", verbose=FALSE) {
    if (!(kind %in% c("english", "SMART", "danish", "french", "hungarian", "norwegian", "russian", "swedish", "catalan", "dutch", "finnish",   
                      "german", "italian", "portuguese", "spanish", "arabic"))) {
        stop(paste(kind, "is not a recognized stopword list type."))
    }
    if (verbose) cat("note: using", kind, "builtin stopwords, but beware that one size may not fit all.\n")
    # data(stopwords, envir = environment())
    quanteda::.stopwords[[kind]]
}

#' @name .stopwords
#' @rdname stopwords
#' @docType data
NULL

#' @rdname stopwords
#' @export
stopwordsGet <- function(kind="english") {
    cat("stopwordsGet() is deprecated, use stopwords() instead.\n")
    stopwords(kind)
}


#' select features from an object
#' 
#' This function selects or discards features from a dfm.variety of objects, 
#' such as tokenized texts, a dfm, or a list of collocations.  The most common usage for 
#' \code{removeFeatures} will be to eliminate stop words from a text or 
#' text-based object, or to select only features from a list of regular 
#' expression.
#' @param x object whose features will be selected
#' @param features character vector of \link{regex}{regular expressions} 
#'   definding the features to be selected, or a dictionary class object whose 
#'   values will provide the features to be selected.  If a dictionary class 
#'   object, the values will be interpreted as regular expressions.  (We may add
#'   the option for other formats in the next revision.)
#' @param selection whether to keep or remove the features
#' @param valuetype how to interpret feature vector: \code{fixed} for words as 
#'   is; \code{"regex"} for regular expressions; or \code{"glob"} for 
#'   "glob"-style wildcard
#' @param case_insensitive ignore the case of dictionary values if \code{TRUE}
#' @param verbose if \code{TRUE} print message about how many features were 
#'   removed
#' @param ... supplementary arguments passed to the underlying functions in 
#'   \code{\link[stringi]{stri_detect_regex}}.  (This is how 
#'   \code{case_insensitive} is passed, but you may wish to pass others.)
#' @note This function selects features based on their labels.  To select
#'   features based on the values of a the document-feature matrix, use
#'   \code{\link{trim}}.
#' @export
#' @seealso \code{\link{removeFeatures}}, \code{\link{trim}}
#' @examples 
#' myDfm <- dfm(c("My Christmas was ruined by your opposition tax plan.", 
#'                "Does the United_States or Sweden have more progressive taxation?"),
#'              toLower = FALSE, verbose = FALSE)
#' mydict <- dictionary(list(countries = c("United_States", "Sweden", "France"),
#'                           wordsEndingInY = c("by", "my"),
#'                           notintext = "blahblah"))
#' selectFeatures(myDfm, mydict)
#' selectFeatures(myDfm, mydict, case_insensitive = FALSE)
#' selectFeatures(myDfm, c("s$", ".y"), "keep", valuetype = "regex")
#' selectFeatures(myDfm, c("s$", ".y"), "remove", valuetype = "regex")
#' selectFeatures(myDfm, stopwords("english"), "keep", valuetype = "fixed")
#' selectFeatures(myDfm, stopwords("english"), "remove", valuetype = "fixed")
selectFeatures <- function(x, features, ...) {
    UseMethod("selectFeatures")
}

#' @rdname selectFeatures
#' @export
selectFeatures.dfm <- function(x, features = NULL, selection = c("keep", "remove"), 
                               valuetype = c("glob", "regex", "fixed"),
                               case_insensitive = TRUE,
                               verbose = TRUE, ...) {
    selection <- match.arg(selection)
    valuetype <- match.arg(valuetype)
    if (is.null(features))
        stop("Must supply a character vector of words to keep or remove")
    features <- unique(unlist(features))  # to convert any dictionaries
    if (valuetype == "glob") 
        features <- lapply(features, utils::glob2rx)
    if (valuetype == "regex" | valuetype == "glob") {
        featIndex <- which(stringi::stri_detect_regex(features(x), paste0(features, collapse = "|"), 
                                                      case_insensitive = case_insensitive, ...))
    } else {
        if (case_insensitive)
            featIndex <- which(toLower(features(x)) %in% toLower(features))
        else featIndex <- which(features(x) %in% features)
    }

    if (verbose) cat(ifelse(selection=="keep", "kept", "removed"), " ", 
                     format(length(featIndex), big.mark=","),
                     " feature", ifelse(length(featIndex) > 1 | length(featIndex)==0, "s", ""), 
                     ", from ", length(features), " supplied feature type",
                     ifelse(length(features) > 0 | length(featIndex)==0, "s", ""),
                     "\n", sep = "")
    # if no features were removed, return original dfm
    if (length(featIndex) == 0)
        return(x)
    if (selection == "keep")
        return(x[, featIndex])
    else
        return(x[, -featIndex])
}

