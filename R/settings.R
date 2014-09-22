
SETTINGS_OPTIONS <- c("stopwords",
                      "collocations",
                      "dictionary",
                      "dictionary_regex",
                      "stem",
                      "delimiter_word",
                      "delimiter_sentence",
                      "delimiter_paragraph",
                      "clean_tolower",
                      "clean_removeDigits",
                      "clean_removePunct",
                      "unitsoriginal",
                      "units") 
DEFAULT_DELIM_SENTENCE <- ".!?"
DEFAULT_DELIM_WORD <- " "
DEFAULT_DELIM_PARAGRAPH <- "\n\n"


#' Get or set the corpus settings
#' 
#' @export 
settings <- function(x, ...) {
    UseMethod("settings")
}


#' Get or set various settings in the corpus for the treatment of texts, such as rules for 
#' stemming, stopwords, collocations, etc.
#' \code{settings(corp)}  query the corps settings
#' \code{settings(corp, settingname) <-}  update the corpus settings
#' @param corp Corpus from/to which settings are queried or applied
#' @rdname settings
#' @examples
#' settings(tempcorpus, "stopwords")
#' tempdfm <- dfm(inaugCorpus)
#' tempdfmSW <- dfm(inaugCorpus, stopwords=TRUE)
#' settings(inaugCorpus, "stopwords") <- TRUE
#' tempdfmSW <- dfm(inaugCorpus)
#' @export 
settings.corpus <- function(x, fields=NULL, ...) {
    if (is.null(fields)) {
        x$settings
    } else {
        if (!(fields %in% SETTINGS_OPTIONS)) stop(paste(fields, "not valid setting."))
        x$settings[fields]
    }
}

# replacement function for corpus settings
#' @export
#' @rdname settings
#' @param fields a valid corpus setting field name
#' @export
"settings<-" <- function(corp, fields, value) {
    if (is.dfm(corp)) stop("Cannot assign settings to a dfm object.")
    if (!(fields %in% SETTINGS_OPTIONS)) stop(paste(fields, "not valid setting."))
    corp$settings[fields] <- value
    corp
}

#' Get the settings from a which a \link{dfm} was created
#' @param x dfm from which settings are queried
#' @rdname settings
#' @examples
#' tempdfm <- dfm(inaugCorpus, stem=TRUE)
#' settings(tempdfm)
#' @export 
settings.dfm <- function(x, ...) {
    attributes(x)$settings
}



#' @export
settingsInitialize <- function() {
    list(stopwords=NULL,
         collocations=NULL,
         dictionary=NULL,
         dictionary_regex=FALSE,
         stem=FALSE,
         delimiter_word=DEFAULT_DELIM_WORD,
         delimiter_sentence=DEFAULT_DELIM_SENTENCE,
         delimiter_paragraph=DEFAULT_DELIM_PARAGRAPH,
         clean_tolower=TRUE,
         clean_removeDigits=TRUE,
         clean_removePunct=TRUE,
         units="documents",
         unitsoriginal="documents")
}


#' @export
settingsReset <- function(corp) {
    corp$settings <- settingsInitialize()
}

#' @export
settingsGet <- function(corp, match.call.list) {
    callingenv <- parent.frame()
    if (is.null(match.call.list$dictionary))
        assign("dictionary", settings(corp, "dictionary"), callingenv)
    if (is.null(match.call.list$dictionary_regex))
        assign("dictionary_regex", settings(corp, "dictionary_regex"), callingenv)
    if (is.null(match.call.list$stem))
        assign("stem", settings(corp, "stem"), callingenv)   
    if (is.null(match.call.list$stopwords))
        assign("stopwords", settings(corp, "stopwords"), callingenv)
    if (is.null(match.call.list$removeDigits))
        assign("removeDigits", settings(corp, "clean_removeDigits"), callingenv)
    if (is.null(match.call.list$removePunct))
        assign("removePunct", settings(corp, "clean_removePunct"), callingenv)
    if (is.null(match.call.list$lower))
        assign("lower", settings(corp, "clean_tolower"), callingenv)
    if (is.null(match.call.list$collocations))
        assign("collocations", settings(corp, "collocations"), callingenv)
}
    
# clean=TRUE,
# removeDigits=TRUE, removePunct=TRUE, lower=TRUE,                          
# addto=NULL

