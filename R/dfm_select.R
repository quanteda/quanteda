#' select features from a dfm or fcm
#' 
#' This function selects or discards features from a \link{dfm} or \link{fcm}, based on a 
#' pattern match with the feature names.   The most common usages are to 
#' eliminate features from a dfm already constructed, such as stopwords, or to 
#' select only terms of interest from a dictionary.
#' @param x the \link{dfm} or \link{fcm} object whose features will be selected
#' @param features one of: a character vector of features to be selected, a 
#'   \link{dfm} whose features will be used for selection, or a dictionary class
#'   object whose values (not keys) will provide the features to be selected. 
#'   For \link{dfm} objects, see details in the Value section below.
#' @param documents select documents based on their document names. Works exactly 
#'   the same as features. 
#' @param selection whether to \code{keep} or \code{remove} the features
#' @inheritParams valuetype
#' @param case_insensitive ignore the case of dictionary values if \code{TRUE}
#' @param min_nchar,max_nchar numerics specifying the minimum and maximum length
#'   in characters for features to be removed or kept; defaults are 1 and 
#'   \href{https://en.wikipedia.org/wiki/Donaudampfschiffahrtselektrizitätenhauptbetriebswerkbauunterbeamtengesellschaft}{79}.
#'    (Set \code{max_nchar} to \code{NULL} for no upper limit.) These are
#'   applied after (and hence, in addition to) any selection based on pattern
#'   matches. These arguments are Ignored when padding is \code{TRUE}.
#' @param padding if \code{TRUE} features or documents not existing in x is 
#'   added to \link{dfm}. This option is available only when selection is 
#'   \code{keep} and valuetype is \code{fixed}. 
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
#' @return A \link{dfm} or \link{fcm} object, after the feature selection has been applied.
#'   
#'   When \code{features} is a \link{dfm} object and \code{padding} is \code{TRUE} , 
#'   then the returned object will be identical in its feature set to the dfm 
#'   supplied as the \code{features} argument. This means that any features in 
#'   \code{x} not in \code{features} will be discarded, and that any features in 
#'   found in the dfm supplied as \code{features} but not found in \code{x} will 
#'   be added with all zero counts.  This is useful when you have trained a model 
#'   on one dfm, and need to project this onto a test set whose features must be 
#'   identical.  See examples.
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
#' dfm_select(myDfm, c("s$", ".y"), selection = "keep", valuetype = "regex")
#' dfm_select(myDfm, c("s$", ".y"), selection = "remove", valuetype = "regex")
#' dfm_select(myDfm, stopwords("english"), selection = "keep", valuetype = "fixed")
#' dfm_select(myDfm, stopwords("english"), selection = "remove", valuetype = "fixed")
#' 
#' # select based on character length
#' dfm_select(myDfm, min_nchar = 5)
#' 
#' # selecting on a dfm
#' txts <- c("This is text one", "This is text two", "This is text three")
#' (dfm1 <- dfm(txts[1:2], verbose = FALSE))
#' (dfm2 <- dfm(txts[2:3], verbose = FALSE))
#' (dfm3 <- dfm_select(dfm1, dfm2, valuetype = "fixed", padding = TRUE, verbose = TRUE))
#' setequal(featnames(dfm2), featnames(dfm3))
#' 
dfm_select <- function(x, features = NULL, documents = NULL, 
                       selection = c("keep", "remove"), 
                       valuetype = c("glob", "regex", "fixed"),
                       case_insensitive = TRUE,
                       min_nchar = 1, max_nchar = 63,
                       padding = FALSE,
                       verbose = FALSE, ...) {
    UseMethod("dfm_select")
}

#' @rdname dfm_select
#' @noRd
#' @export
dfm_select.dfm <-  function(x, features = NULL, documents = NULL, 
                            selection = c("keep", "remove"), 
                            valuetype = c("glob", "regex", "fixed"),
                            case_insensitive = TRUE,
                            min_nchar = 1, max_nchar = 63,
                            padding = FALSE,
                            verbose = FALSE, ...) {
    
    selection <- match.arg(selection)
    valuetype <- match.arg(valuetype)
    attrs_org <- attributes(x)
    types <- featnames(x)
    labels <- docnames(x)
    
    # select features based on "features" pattern
    if (!is.null(features)) {
        # special handling if features is a dfm
        if (is.dfm(features)) {
            features <- featnames(features)
            valuetype <- 'fixed'
            case_insensitive <- FALSE
        }
        features <- unlist(features, use.names = FALSE) # this funciton does not accpet list
        features_id <- unlist(regex2id(features, types, valuetype, case_insensitive, FALSE), use.names = FALSE)
        features_id <- sort(features_id) # keep the original column order
    } else {
        if (selection == "keep")
            features_id <- 1:nfeature(x)
        else
            features_id <- NULL
    }
    
    if (!padding) {
        # select features based on character length
        features_id <- intersect(features_id, which(stringi::stri_length(types) >= min_nchar & 
                                                    stringi::stri_length(types) <= max_nchar))
    }
    
    # select documents based on "documents" pattern
    if (!is.null(documents)){
        documents <- unlist(documents, use.names = FALSE) # this funciton does not accpet list
        documents_id <- unlist(regex2id(documents, labels, valuetype, case_insensitive, FALSE), use.names = FALSE)
        documents_id <- sort(documents_id) # keep the original row order
    } else {
        if (selection == "keep")
            documents_id <- 1:ndoc(x)
        else
            documents_id <- NULL
    }
    
    types_add <- labels_add <- character() # avoid error in verbose message
    
    if (selection == "keep") {
        if (!length(features_id) || !length(documents_id)) {
            x <- NULL
        } else {
            x <- x[documents_id, features_id]
            if (valuetype == 'fixed' && padding) {
                
                # padding for features
                types_add <- setdiff(features, types)
                if (length(types_add)) {
                    x <- new("dfmSparse", Matrix::cbind2(x, sparseMatrix(i = NULL, j = NULL, 
                                                                         dims = c(ndoc(x), length(types_add)), 
                                                                         dimnames = list(docnames(x), types_add))))
                }
                if (case_insensitive)
                    x <- dfm_tolower(x)
                
                # padding for documents
                labels_add <- setdiff(documents, labels)
                if (length(labels_add)) {
                    x <- new("dfmSparse", Matrix::rbind2(x, sparseMatrix(i = NULL, j = NULL, 
                                                                         dims = c(length(labels_add), nfeature(x)), 
                                                                         dimnames = list(labels_add, featnames(x)))))
                }
            }
        }
    } else {
        if (length(features_id) == nfeature(x) || length(documents_id) == ndoc(x)) {
            x <- NULL    
        } else if(!length(features_id)) {
            x <- x[documents_id * -1,]
        } else if(!length(documents_id)) {
            x <- x[, features_id * -1]
        } else {
            x <- x[documents_id * -1, features_id * -1]
        }
    }
   
    if (verbose) {
        catm("dfm_select ", ifelse(selection=="keep", "kept", "removed"), " ", 
             format(length(features_id), big.mark=","),
             " feature", ifelse(length(features_id) > 1, "s", ""), " in ",
             format(length(documents_id), big.mark=","),
             " documents", ifelse(length(documents_id) > 1, "s", ""),
             " padding 0s for ",
             format(length(types_add), big.mark=","), 
             " feature", ifelse(length(types_add) > 1, "s", ""), " and ",
             format(length(labels_add), big.mark=","),
             " document", ifelse(length(labels_add) > 1, "s", ".\n"),
             sep = "")
    } 
    
    return(x)
}


#' @rdname dfm_select
#' @export
#' @examples 
#' tmpdfm <- dfm(c("This is a document with lots of stopwords.",
#'                 "No if, and, or but about it: lots of stopwords."),
#'               verbose = FALSE)
#' tmpdfm
#' dfm_remove(tmpdfm, stopwords("english"))
dfm_remove <- function(x, features = NULL, documents = NULL, ...) {
    dfm_select(x, features, selection = "remove", ...)
}




                       
