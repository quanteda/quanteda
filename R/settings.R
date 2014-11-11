
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
#' @param  x object from/to which settings are queried or applied
#' @param  ... additional arguments
#' @export 
settings <- function(x, ...) {
    UseMethod("settings")
}


#' Get or set various settings in the corpus for the treatment of texts, such as rules for 
#' stemming, stopwords, collocations, etc.
#' @param field string containing the name of the setting to be set or queried
#' \code{settings(x)}  query the corps settings 
#' 
#' \code{settings(x, field) <-}  update the corpus settings for \code{field}
#' @rdname settings
#' @examples
#' settings(inaugCorpus, "stopwords")
#' tempdfm <- dfm(inaugCorpus)
#' tempdfmSW <- dfm(inaugCorpus, stopwords=TRUE)
#' settings(inaugCorpus, "stopwords") <- TRUE
#' tempdfmSW <- dfm(inaugCorpus)
#' @export 
settings.corpus <- function(x, field=NULL, ...) {
    if (is.null(field)) {
        x$settings
    } else {
        if (!(field %in% SETTINGS_OPTIONS)) stop(paste(field, "not valid setting."))
        x$settings[field]
    }
}

# replacement function for corpus settings
#' @export
#' @rdname settings
#' @param value new setting value
#' @export
"settings<-" <- function(x, field, value) {
    if (is.dfm(x)) stop("Cannot assign settings to a dfm object.")
    if (!(field %in% SETTINGS_OPTIONS)) stop(paste(field, "not valid setting."))
    x$settings[field] <- value
    x
}

#' Get the settings from a which a \link{dfm} was created
#' @rdname settings
#' @examples
#' tempdfm <- dfm(inaugCorpus, stem=TRUE)
#' settings(tempdfm)
#' @export 
settings.dfm <- function(x, ...) {
    attributes(x)$settings
}


#' \code{settingsInitialize} returns a list of legal settings, set to their default values
#' @rdname setttings
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


##
## DOESN'T MODIFY IN PLACE -- NEEDS REWRITING
##
# \code{settingsReset} restores settings for a corpus to the default values
# @rdname setttings
# @export
settingsReset <- function(corp) {
    corp$settings <- settingsInitialize()
}

# \code{settingsReset} restores settings for a corpus to the default values
# @rdname setttings
# @export
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

