#' construct a compressed corpus object
#' @rdname corpuszip
#' @inheritParams corpus
#' @export
#' @keywords corpuszip
#' @examples
#' # create a compressed corpus from texts
#' corpuszip(data_char_inaugural)
#' 
#' # create a compressed corpus from texts and assign meta-data and document variables
#' cop <- corpus(data_char_ukimmig2010, 
#'               docvars = data.frame(party = names(data_char_ukimmig2010)))
#' cop_zip <- corpuszip(data_char_ukimmig2010, 
#'                      docvars = data.frame(party = names(data_char_ukimmig2010)))
#' object.size(cop)
#' object.size(cop_zip)
#' 
#
corpuszip <- function(x, docnames = NULL, docvars = NULL, text_field = "text", metacorpus = NULL, ...) {
    UseMethod("corpuszip")
}

#' @noRd
#' @export
corpuszip.character <- function(x, docnames = NULL, docvars = NULL, text_field = "text", metacorpus = NULL, ...) {
    if (!missing(text_field))
        stop("text_field is not applicable for this class of input")
    
    if (length(addedArgs <- list(...)))
        warning("Argument", ifelse(length(addedArgs)>1, "s ", " "), names(addedArgs), " not used.", sep = "")
    
    names_org <- names(x)
    
    # convert the dreaded "curly quotes" to ASCII equivalents
    x <- stringi::stri_replace_all_fixed(x, 
                                         c("\u201C", "\u201D", "\u201F",
                                           "\u2018", "\u201B", "\u2019"),                                     
                                         c("\"", "\"", "\"", 
                                           "\'", "\'", "\'"), vectorize_all = FALSE)
    
    # replace all hyphens with simple hyphen
    x <- stringi::stri_replace_all_regex(x, "\\p{Pd}", "-")

    # name the texts vector
    if (!is.null(docnames)) {
        stopifnot(length(docnames)==length(x))
        names <- docnames
    } else if (is.null(names_org)) {
        names <- paste("text", 1:length(x), sep="")
    } else if (is.null(names(x))) {
        # if they previously existed, but got obliterated by a stringi function
        names <- names_org
    }

    # create document-meta-data
    if (is.null(metacorpus$source)) {
        metacorpus$source <- paste(getwd(), "/* ", "on ",  Sys.info()["machine"], " by ", Sys.info()["user"], sep="")
    }
    metacorpus$created <- date()

    # user-supplied document-level variables (one kind of meta-data)
    if (!is.null(docvars)) {
        if (nrow(docvars) > 0) {
            stopifnot(nrow(docvars)==length(x))
            docvars <- as.data.frame(docvars)
        } 
    }
    
    # build and return the corpus object
    tempCorpus <- list(documents = memCompress(x, 'gzip'),
                       docvars = docvars,
                       names = names,
                       metadata = metacorpus, 
                       settings = settings(),
                       tokens = NULL)
    class(tempCorpus) <- list("corpuszip", "corpus", class(tempCorpus))
    return(tempCorpus)
}
#' @noRd
#' @export
texts.corpuszip <- function(x){
    unlist(strsplit(memDecompress(x$documents, 'gzip', asChar = TRUE), "\n"), use.names = FALSE)
}
#' @noRd
#' @export
"texts<-.corpuszip" <- function(x, value) { 
    x$documents <- memCompress(txt, 'gzip')
}

#' @noRd
#' @export
docvars.corpuszip <- function(x, field = NULL) {
    if (is.null(field))
        return(x$docvars)
    if (!(field %in% names(x$docvars)))
        return(NULL)
    return(x$docvars[, field, drop=TRUE])
}

#' @noRd
#' @export
"docvars<-.corpuszip" <- function(x, field = NULL, value) {
    if ("texts" %in% field) stop("You should use texts() instead to replace the corpus texts.")
    if (is.null(field)) {
        field <- names(value)
        if (is.null(field))
            field <- paste("docvar", 1:ncol(as.data.frame(value)), sep="")
    }
    x$docvars[field] <- value
    return(x)
}

setOldClass("corpuszip")
