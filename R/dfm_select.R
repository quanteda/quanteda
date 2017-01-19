#' select features from a dfm or fcm
#' 
#' This function selects or discards features from a dfm or fcm, based on a 
#' pattern match with the feature names.   The most common usages are to 
#' eliminate features from a dfm already constructed, such as stopwords, or to 
#' select only terms of interest from a dictionary.
#' @param x the \link{dfm} or \link{fcm} object whose features will be selected
#' @param features one of: a character vector of features to be selected, a 
#'   \link{dfm} whose features will be used for selection, or a dictionary class
#'   object whose values (not keys) will provide the features to be selected. 
#'   For \link{dfm} objects, see details in the Value section below.
#' @param selection whether to \code{keep} or \code{remove} the features
#' @inheritParams valuetype
#' @param case_insensitive ignore the case of dictionary values if \code{TRUE}
#' @param min_nchar,max_nchar numerics specifying the minimum and maximum length
#'   in characters for features to be removed or kept; defaults are 1 and 
#'   \href{https://en.wikipedia.org/wiki/Donaudampfschiffahrtselektrizitätenhauptbetriebswerkbauunterbeamtengesellschaft}{79}.
#'    (Set \code{max_nchar} to \code{NULL} for no upper limit.) These are
#'   applied after (and hence, in addition to) any selection based on pattern
#'   matches.
#' @param verbose if \code{TRUE} print message about how many features were 
#'   removed
#' @param ... supplementary arguments passed to the underlying functions in 
#'   \code{\link[stringi]{stri_detect_regex}}
#' @details \code{dfm_remove} and \code{fcm_remove} are simply a convenience
#'   wrappers to calling \code{dfm_select} and \code{fcm_select} with
#'   \code{selection = "remove"}.
#' @note This function selects features based on their labels.  To select 
#'   features based on the values of a the document-feature matrix, use 
#'   \code{\link{dfm_trim}}.
#' @return A dfm or fcm after the feature selection has been applied.
#'   
#'   When \code{features} is a \link{dfm} object, then the returned object will 
#'   be identical in its feature set to the dfm supplied as the \code{features} 
#'   argument.  This means that any features in \code{x} not in \code{features} 
#'   will be discarded, and that any features in found in the dfm supplied as 
#'   \code{features} but not found in \code{x} will be added with all zero 
#'   counts.  This is useful when you have trained a model on one dfm, and need 
#'   to project this onto a test set whose features must be identical.
#' @export
#' @keywords dfm
#' @examples 
#' myDfm <- dfm(c("My Christmas was ruined by your opposition tax plan.", 
#'                "Does the United_States or Sweden have more progressive taxation?"),
#'              tolower = FALSE, verbose = FALSE)
#' mydict <- dictionary(list(countries = c("United_States", "Sweden", "France"),
#'                           wordsEndingInY = c("by", "my"),
#'                           notintext = "blahblah"))
#' dfm_select(myDfm, mydict)
#' dfm_select(myDfm, mydict, case_insensitive = FALSE)
#' dfm_select(myDfm, c("s$", ".y"), "keep")
#' dfm_select(myDfm, c("s$", ".y"), "keep", valuetype = "regex")
#' dfm_select(myDfm, c("s$", ".y"), "remove", valuetype = "regex")
#' dfm_select(myDfm, stopwords("english"), "keep", valuetype = "fixed")
#' dfm_select(myDfm, stopwords("english"), "remove", valuetype = "fixed")
#' 
#' # select based on character length
#' dfm_select(myDfm, min_nchar = 5)
#' 
#' # selecting on a dfm
#' textVec1 <- c("This is text one.", "This, the second text.", "Here: the third text.")
#' textVec2 <- c("Here are new words.", "New words in this text.")
#' (dfm1 <- dfm(textVec1, verbose = FALSE))
#' (dfm2a <- dfm(textVec2, verbose = FALSE))
#' (dfm2b <- dfm_select(dfm2a, dfm1))
#' setequal(featnames(dfm1), featnames(dfm2b))
#' 
#' # more selection on a dfm
#' dfm_select(dfm1, dfm2a)
#' dfm_select(dfm1, dfm2a, selection = "remove")
#' 
dfm_select <- function(x, features = NULL, selection = c("keep", "remove"), 
                       valuetype = c("glob", "regex", "fixed"),
                       case_insensitive = TRUE,
                       min_nchar = 1, max_nchar = 63,
                       verbose = FALSE, ...) {
    UseMethod("dfm_select")
}

#' @rdname dfm_select
#' @noRd
#' @export
dfm_select.dfm <-  function(x, features = NULL, selection = c("keep", "remove"), 
                            valuetype = c("glob", "regex", "fixed"),
                            case_insensitive = TRUE,
                            min_nchar = 1, max_nchar = 63,
                            verbose = FALSE, ...) {
    selection <- match.arg(selection)
    valuetype <- match.arg(valuetype)
    features_from_dfm <- FALSE
    
    # select features based on character length
    featIndex <- 1:nfeature(x)
    featIndex <- intersect(featIndex, 
                           which(stringi::stri_length(featnames(x)) >= min_nchar & stringi::stri_length(featnames(x)) <= max_nchar))
    
    # select features based on "features" pattern
    if (!is.null(features)) {
        
        if (!(is.character(features) | is.dfm(features) | is(features, "dictionary")))
            stop("features must be of type character, dictionary, or dfm")
        
        # special handling if features is a dfm
        if (is.dfm(features)) {
            if (selection == "keep") {
                features_dfm <- features <- featnames(features)
                features_from_dfm <- TRUE
            } else {
                features <- featnames(features)
            }
        }
        
        # to convert any dictionaries
        features <- unique(unlist(features))  
        
        # convert glob to fixed if no actual glob characters (since fixed is much faster)
        originalvaluetype <- valuetype
        if (valuetype == "glob") {
            # treat as fixed if no glob characters detected
            if (!sum(stringi::stri_detect_charclass(features, c("[*?]"))))
                valuetype <- "fixed"
            else {
                features <- sapply(features, utils::glob2rx, USE.NAMES = FALSE)
                valuetype <- "regex"
            }
        }
        
        features_x <- featnames(x)
        if (case_insensitive & valuetype == "fixed") {
            features_x <- char_tolower(features_x)
            features <- char_tolower(features)
        }
        
        # split features on concatenator if exists
        if (x@concatenator != "")
            features_x <- strsplit(features_x, x@concatenator)
        
        if (valuetype == "regex") {
            if (all.equal(x@ngrams, 1L)==TRUE) {
                featIndex <- intersect(featIndex, 
                                       which(stringi::stri_detect_regex(features_x, paste0(features, collapse = "|"), 
                                                                        case_insensitive = case_insensitive, ...)))
            } else {
                ####
                ####
                matchPattern <- paste0(features, collapse = "|")
                featIndex <- intersect(featIndex,
                                       which(sapply(features_x, 
                                                    function(x) any(stringi::stri_detect_regex(x, matchPattern, 
                                                                                               case_insensitive = case_insensitive, ...)))))
            }
        } else {
            if (all.equal(x@ngrams, 1L)==TRUE)
                featIndex <- intersect(featIndex, 
                                       which(features_x %in% features))  # unigrams
            else
                featIndex <- intersect(featIndex, 
                                       which(sapply(features_x, function(f) any(f %in% features), USE.NAMES = FALSE))) # ngrams
        }
        
        if (verbose & !features_from_dfm) 
            catm(ifelse(selection=="keep", "kept", "removed"), " ", 
                 format(length(featIndex), big.mark=","),
                 " feature", ifelse(length(featIndex) > 1 | length(featIndex)==0, "s", ""), 
                 ", from ", length(features), " supplied (", originalvaluetype, ") feature type",
                 ifelse(length(features) > 0 | length(featIndex)==0, "s", ""),
                 "\n", sep = "")
        
        # pad the zeros if features was a dfm, return in same feature order as original dfm
        # for selection = "keep" only
        if (features_from_dfm) {
            if (verbose)
                catm(ifelse(selection=="keep", "found", "zeroed"), " ", 
                     format(length(featIndex), big.mark=","),
                     " feature", ifelse(length(featIndex) > 1 | length(featIndex)==0, "s", ""), 
                     " from ", length(features), " supplied type",
                     ifelse(length(features) > 0 | length(featIndex)==0, "s", ""),
                     " in a dfm,", sep = "")
            # remove features in x that are not in features (from supplied dfm)
            x2 <- x[, featIndex]
            # now add zero-valued features to x that are not in x but are in features
            origDfmFeatureIndex <- which(!(char_tolower(features) %in% char_tolower(featnames(x2))))
            xOriginalFeatureLength <- nfeature(x2)
            xOriginalFeatures <- featnames(x2)
            if (verbose) catm(" padding 0s for another", length(origDfmFeatureIndex), "\n")
            x <- new("dfmSparse", Matrix::cbind2(x2,
                                                 sparseMatrix(i = NULL, j = NULL, dims = c(ndoc(x2), length(origDfmFeatureIndex)), 
                                                              dimnames = list(docnames(x2), features[origDfmFeatureIndex]))))
            featIndex <- match(features_dfm, featnames(x))
            # x <- x2 #[, features_dfm]
        }
    }        
    
    ##
    ## MIGHT NEED TO ADD BACK ORIGINAL ATTRIBUTES HERE
    ##
    
    if (!length(featIndex)) {
        if (selection == "keep")
            return(NULL)
        else 
            return(x)
    } else {
        if (selection == "keep")
            return(x[, featIndex])
        else 
            return(x[, -featIndex])
    }
}


#' @rdname dfm_select
#' @export
#' @examples 
#' tmpdfm <- dfm(c("This is a document with lots of stopwords.",
#'                 "No if, and, or but about it: lots of stopwords."),
#'               verbose = FALSE)
#' tmpdfm
#' dfm_remove(tmpdfm, stopwords("english"))
dfm_remove <- function(x, features = NULL, ...) {
    dfm_select(x, features, selection = "remove", ...)
}




                       