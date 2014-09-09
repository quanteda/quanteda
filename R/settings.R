
####
#### PACKAGE GLOBALS
####
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
                      "clean_removePunct") 
DEFAULT_DELIM_SENTENCE <- ".!?"
DEFAULT_DELIM_WORD <- " "
DEFAULT_DELIM_PARAGRAPH <- "\n\n"

#' Get or set the corpus settings
#' 
#' Get or set various settings in the corpus for the treatment of texts, such as rules for 
#' stemming, stopwords, collocations, etc.
#' \code{settings(corp)}  query the corps settings
#' \code{settings(corp, settingname) <-}  update the corpus settings
#' @param corp Corpus from/to which settings are queried or applied
#' @export 
settings <- function(corp, fields=NULL) {
    if (!(fields %in% SETTINGS_OPTIONS)) stop(paste(fields, "not valid setting."))
    if (is.null(fields))
        corp$settings
    else
        unlist(corp$settings[fields])
}

# replacement function for corpus settings
#' @export
#' @rdname settings
#' @param fields a valid corpus setting field name
#' @export
"settings<-" <- function(corp, value, fields) {
    if (!(fields %in% SETTINGS_OPTIONS)) stop(paste(fields, "not valid setting."))
    corp$settings[fields] <- value
    corp
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
         clean_removePunct=TRUE)
}


    
