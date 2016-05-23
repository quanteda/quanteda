#' old version of selectFeatures.tokenizedTexts
#' 
#' Calls C++ for super-fast selection or removal of features from a 
#' set of tokens.
#' @inheritParams selectFeatures
#' @export
selectFeaturesOLD <- function(x, ...) UseMethod("selectFeaturesOLD")

#' @rdname selectFeaturesOLD
#' @export
#' @examples
#' toks <- tokenize(c("This is some example text from me.", "More of the example text."), 
#'                  removePunct = TRUE)
#' selectFeaturesOLD(toks, stopwords("english"), "remove")
#' selectFeaturesOLD(toks, "ex", "keep", valuetype = "regex")
selectFeaturesOLD.tokenizedTexts <- function(x, features, selection = c("keep", "remove"), 
                                          valuetype = c("glob", "regex", "fixed"),
                                          case_insensitive = TRUE,
                                          verbose = TRUE, ...) {
    selection <- match.arg(selection)
    valuetype <- match.arg(valuetype)
    features <- unique(unlist(features))  # to convert any dictionaries
    
    originalvaluetype <- valuetype
    # convert glob to fixed if no actual glob characters (since fixed is much faster)
    if (valuetype == "glob") {
        # treat as fixed if no glob characters detected
        if (!sum(stringi::stri_detect_charclass(features, c("[*?]"))))
            valuetype <- "fixed"
        else {
            features <- sapply(features, utils::glob2rx, USE.NAMES = FALSE)
            valuetype <- "regex"
        }
    }
    
    if (valuetype == "fixed") {

        if (case_insensitive) {
            if (selection == "remove") {
                result <- lapply(x, function(x) x[which(!(toLower(x) %in% toLower(features)))])
            } else {
                result <- lapply(x, function(x) x[which((toLower(x) %in% toLower(features)))])
            }
        } else {
            if (selection == "remove") {
                result <- lapply(x, function(x) x[which(!(x %in% features))])
            } else {
                result <- lapply(x, function(x) x[which(x %in% features)])
            }
        }

    } else if (valuetype == "regex") {
        
        if (selection == "remove") {
            result <- lapply(x, function(y) {
                removeIndex <- which(stringi::stri_detect_regex(y, rep(paste0(features, collapse = "|"), length(y)), 
                                                                case_insensitive = case_insensitive, ...))
                if (length(removeIndex)) y[-removeIndex] else y
            })
        } else {
            result <- lapply(x, function(y) y[which(stringi::stri_detect_regex(y, rep(paste0(features, collapse = "|"), length(y)), 
                                                                               case_insensitive = case_insensitive, ...))])
        }
    }
    
    class(result) <- c("tokenizedTexts", class(result))
    attributes(result) <- attributes(x)
    result
}
