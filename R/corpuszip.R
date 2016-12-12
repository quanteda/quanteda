quanteda_document_delimiter <- "###END_DOCUMENT###"

#' construct a compressed corpus object
#' 
#' Construct a compressed version of a \link{corpus}.
#' @inheritParams corpus
#' @importFrom utils object.size
#' @export
#' @keywords corpuszip internal
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
corpuszip <- function(x, docnames = NULL, docvars = NULL, text_field = "text", metacorpus = NULL, ...) {
    UseMethod("corpuszip")
}

#' @noRd
#' @export
corpuszip.data.frame <- function(x, docnames = NULL, docvars = NULL, text_field = "text", metacorpus = NULL, ...) {
    
    args <- list(...)
    if (!missing(docvars))
        stop("docvars are assigned automatically for data.frames")
    
    if (is.character(text_field)) {
        text_fieldi <- which(names(x)==text_field)
        if (length(text_fieldi)==0)
            stop("column name ", text_field, " not found.")
        text_field <- text_fieldi
    }
    if (!is.character(x[, text_fieldi]))
        stop("text_field must refer to a character mode column")
    
    corpuszip(x[, text_fieldi], 
           docvars = x[, -text_fieldi, drop = FALSE],
           docnames = if (!identical(row.names(x), as.character(1:nrow(x)))) row.names(x) else NULL, #paste0("text", 1:nrow(x)),
           metacorpus = metacorpus, ...)
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
        stopifnot(length(docnames) == length(x))
        # names(x) <- docnames
    } else if (is.null(names_org)) {
        docnames <- paste("text", 1:length(x), sep="")
    } else if (is.null(names(x))) {
        # if they previously existed, but got obliterated by a stringi function
        docnames <- names_org
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
            rownames(docvars) <- docnames
        } 
    }
    
    # paste delimiters into object to be compressed
    x[1 : (length(x)-1)] <- paste0(x[1 : (length(x)-1)], quanteda_document_delimiter)
    
    # build and return the corpus object
    tempCorpus <- list(texts = memCompress(x, 'gzip'),
                       documents = docvars,
                       metadata = metacorpus, 
                       settings = settings(),
                       tokens = NULL)
    # add docnames
    tempCorpus$docnames <- docnames
    # compute the compression %
    tempCorpus$compression_rate <- utils::object.size(tempCorpus$texts) / utils::object.size(x) * 100
    class(tempCorpus) <- list("corpuszip", "corpus", class(tempCorpus))
    return(tempCorpus)
}


#' @noRd
#' @export
texts.corpuszip <- function(x, groups = NULL, ...) {
    result <- memDecompress(x$texts, 'gzip', asChar = TRUE)
    result <- strsplit(result, paste0(quanteda_document_delimiter, "\n"))
    result <- unlist(result, use.names = FALSE)
    names(result) <- docnames(x)
    result
}

#' @noRd
#' @method as.character corpus
#' @export
as.character.corpuszip <- function(x, ...) {
    texts(x)
}


#' @noRd
#' @export
"texts<-.corpuszip" <- function(x, value) { 
    temp_texts <- texts(x)
    temp_texts <- value
    x$texts <- memCompress(temp_texts, 'gzip')
    # NOTE: this will not replace named elements in docnames
    x
}

# #' @noRd
# #' @export
# docvars.corpuszip <- function(x, field = NULL) {
#     if (is.null(field))
#         return(x$docvars)
#     if (!(field %in% names(x$docvars)))
#         return(NULL)
#     return(x$docvars[, field, drop=TRUE])
# }
# 
# #' @noRd
# #' @export
# "docvars<-.corpuszip" <- function(x, field = NULL, value) {
#     if ("texts" %in% field) stop("You should use texts() instead to replace the corpus texts.")
#     if (is.null(field)) {
#         field <- names(value)
#         if (is.null(field))
#             field <- paste("docvar", 1:ncol(as.data.frame(value)), sep="")
#     }
#     x$docvars[field] <- value
#     return(x)
# }

#' @export
#' @noRd
docnames.corpuszip <- function(x) {
    x$docnames
}


setOldClass("corpuszip")
