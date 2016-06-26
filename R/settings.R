
SETTINGS_OPTIONS <- c("stopwords",
                      "collocations",
                      "dictionary",
                      "valuetype",
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

#' @rdname settings
#' @export
#' @details Calling \code{settings()} with no arguments returns a list of system default settings.
settings.default <- function(x=NULL, ...) {
    if (!is.null(x)) 
        stop("settings default should be used without arguments")
    settingsInitialize()
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
#' (tempdfm <- dfm(subset(inaugCorpus, Year>1980), verbose=FALSE))
#' (tempdfmSW <- dfm(subset(inaugCorpus, Year>1980),
#'                  ignoredFeatures=stopwords("english"), verbose=FALSE))
#' settings(inaugCorpus, "stopwords") <- TRUE
#' @export 
settings.corpus <- function(x, field=NULL, ...) {
    if (is.null(field)) {
        x$settings
    } else {
        if (!(field %in% SETTINGS_OPTIONS)) stop(paste(field, "not valid setting."))
        x$settings[[field]]
    }
}

# replacement function for corpus settings
#' @export
#' @rdname settings
#' @param value new setting value
#' @export
"settings<-" <- function(x, field, value) {
    if (is(x, "dfm")) stop("Cannot assign settings to a dfm object.")
    if (!(field %in% SETTINGS_OPTIONS)) stop(paste(field, "not valid setting."))
    x$settings[field] <- value
    # catm("note: corpus settings are not yet used in dfm construction.\n")
    x
}

#' Get the settings from a which a \link{dfm} was created
#' @rdname settings
#' @examples
#' tempdfm <- dfm(inaugCorpus, stem=TRUE, verbose=FALSE)
#' settings(tempdfm)
#' @export 
settings.dfm <- function(x, ...) {
    # attributes(x)$settings
    catm("note: dfm settings are not yet implemented - coming soon.\n")
}


# @rdname settings
# @export
settingsInitialize <- function() {
    tempsettings <- list(stopwords=NULL,
                         collocations=NULL,
                         dictionary=NULL,
                         valuetype = "glob",
                         stem=FALSE,
                         delimiter_word=DEFAULT_DELIM_WORD,
                         delimiter_sentence=DEFAULT_DELIM_SENTENCE,
                         delimiter_paragraph=DEFAULT_DELIM_PARAGRAPH,
                         clean_tolower=TRUE,
                         clean_removeDigits=TRUE,
                         clean_removePunct=TRUE,
                         units="documents",
                         unitsoriginal="documents")
    class(tempsettings) <- c("settings", class(tempsettings))
    tempsettings
}


#' @rdname settings
#' @method print settings
#' @export
print.settings <- function(x, ...) {
    cat("Settings:\n")
    for (s in names(x)) {
        cat("  ", s, ": ", sep="")
        print(x[[s]])
    }
}



##
## DOESN'T MODIFY IN PLACE -- NEEDS REWRITING
##
# \code{settingsReset} restores settings for a corpus to the default values
# @rdname settings
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
    if (is.null(match.call.list$valuetype))
        assign("valuetype", settings(corp, "valuetype"), callingenv)
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

