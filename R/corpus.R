#' construct a corpus object
#' 
#' Creates a corpus object from available sources.  The currently available  
#' sources are: 
#' \itemize{ 
#' \item a \link{character} vector, consisting of one document per element; if 
#'   the elements are named, these names will be used as document names.
#' \item a readtext object, from the \pkg{readtext} package 
#'   (which is a specially constructed data.frame)
#' \item a \link{data.frame}, whose default variable containing the document is 
#'   character vector named \code{text}, although this can be set to any other
#'   variable name using the \code{text_field} argument.  Other variables are 
#'   imported as document-level meta-data.
#' \item a \link{kwic} object constructed by \code{\link{kwic}}.
#' \item a \pkg{tm} \link[tm]{VCorpus} class  object, with the fixed metadata 
#'   fields imported as document-level metadata. Corpus-level metadata is not 
#'   currently imported.
#' } 
#' @param x a valid corpus source object
#' @param docnames Names to be assigned to the texts, defaults to the names of 
#'   the character vector (if any), otherwise assigns "text1", "text2", etc.
#' @param docvars A data frame of attributes that is associated with each text.
#' @param text_field the character name or numeric index of the source \code{data.frame}
#'   indicating the variable to be read in as text, which must be a character vector.
#'   All other variables in the data.frame will be imported as docvars.  This argument 
#'   is only used for \code{data.frame} objects (including those created by \pkg{readtext}).
#' @param metacorpus a named list containing additional (character) information to be added to the
#' corpus as corpus-level metadata.  Special fields recognized in the \code{\link{summary.corpus}}
#' are: 
#' \itemize{
#' \item{\code{source }}{a description of the source of the texts, used for 
#'   referencing;}
#' \item{\code{citation }}{information on how to cite the corpus; and}
#' \item{\code{notes }}{any additional information about who created the text, warnings, 
#'   to do lists, etc.}
#' }
#' @param compress logical; if \code{TRUE}, compress the texts in memory using gzip compression.
#' This significantly reduces the size of the corpus in memory, but will slow down operations that
#' require the texts to be extracted.
#' @param ... not used directly
#' @return A \link{corpus-class} class object containing the original texts, document-level 
#'   variables, document-level metadata, corpus-level metadata, and default 
#'   settings for subsequent processing of the corpus.  
#' @section A warning on accessing corpus elements:
#'   A corpus currently 
#'   consists of an S3 specially classed list of elements, but \strong{you should not 
#'   access these elements directly}. Use the extractor and replacement 
#'   functions instead, or else your code is not only going to be uglier, but
#'   also likely to break should the internal structure of a corpus object
#'   change (as it inevitably will as we continue to develop the package,
#'   including moving corpus objects to the S4 class system).
#' @seealso \link{corpus-class}, \code{\link{docvars}}, \code{\link{metadoc}}, 
#'   \code{\link{metacorpus}}, 
#'   \code{\link{settings}}, \code{\link{texts}}, \code{\link{ndoc}}, 
#'   \code{\link{docnames}}
#' @details The texts and document variables of corpus objects can also be 
#'   accessed using index notation. Indexing a corpus object as a vector will 
#'   return its text, equivalent to \code{texts(x)}.  Note that this is not the 
#'   same as subsetting the entire corpus -- this should be done using the 
#'   \code{\link{subset}} method for a corpus.
#'   
#'   Indexing a corpus using two indexes (integers or column names) will return 
#'   the document variables, equivalent to \code{docvars(x)}.  Because a corpus 
#'   is also a list, it is also possible to access, create, or replace docvars 
#'   using list notation, e.g. 
#'   
#'   \code{myCorpus[["newSerialDocvar"]] <- 
#'   paste0("tag", 1:ndoc(myCorpus))}.
#'   
#'   For details, see \link{corpus-class}.
#' @author Kenneth Benoit and Paul Nulty
#' @export
#' @keywords corpus
#' @examples
#' # create a corpus from texts
#' corpus(data_char_inaugural)
#' 
#' # create a corpus from texts and assign meta-data and document variables
#' summary(corpus(data_char_ukimmig2010, 
#'                docvars = data.frame(party = names(data_char_ukimmig2010))), 5) 
#'
#' corpus(texts(data_corpus_irishbudget2010))
#' 
#' # import a tm VCorpus
#' if ("tm" %in% rownames(installed.packages())) {
#'     data(crude, package = "tm")    # load in a tm example VCorpus
#'     mytmCorpus <- corpus(crude)
#'     summary(mytmCorpus, showmeta=TRUE)
#'     
#'     data(acq, package = "tm")
#'     summary(corpus(acq), 5, showmeta=TRUE)
#'     
#'     tmCorp <- tm::VCorpus(tm::VectorSource(data_char_inaugural[49:57]))
#'     quantCorp <- corpus(tmCorp)
#'     summary(quantCorp)
#' }
#' 
#' # construct a corpus from a data.frame
#' mydf <- data.frame(letter_factor = factor(rep(letters[1:3], each = 2)),
#'                   some_ints = 1L:6L,
#'                   some_text = paste0("This is text number ", 1:6, "."),
#'                   stringsAsFactors = FALSE,
#'                   row.names = paste0("fromDf_", 1:6))
#' mydf
#' summary(corpus(mydf, text_field = "some_text", 
#'                metacorpus = list(source = "From a data.frame called mydf.")))
#' 
#' # construct a corpus from a kwic object
#' mykwic <- kwic(data_corpus_inaugural, "southern")
#' summary(corpus(mykwic))
corpus <- function(x, docnames = NULL, docvars = NULL, text_field = "text", metacorpus = NULL, compress = FALSE, ...) {
    UseMethod("corpus")
}

#' @rdname corpus
#' @noRd
#' @export
corpus.character <- function(x, docnames = NULL, docvars = NULL, text_field = "text", metacorpus = NULL, compress = FALSE, ...) {
    if (!missing(text_field))
        stop("text_field is not applicable for this class of input")
    
    if (length(addedArgs <- list(...)))
        warning("Argument", ifelse(length(addedArgs)>1, "s ", " "), names(addedArgs), " not used.", sep = "")
    
    x_names <- names(x)
    
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
        names(x) <- docnames
    } else if (is.null(x_names)) {
        names(x) <- paste("text", 1:length(x), sep="")
    } else if (is.null(names(x))) {
        # if they previously existed, but got obliterated by a stringi function
        names(x) <- x_names
    }

    # create document-meta-data
    if (is.null(metacorpus$source)) {
        metacorpus$source <- paste(getwd(), "/* ", "on ",  Sys.info()["machine"], " by ", Sys.info()["user"], sep="")
    }
    metacorpus$created <- date()

    # create the documents data frame starting with the texts, using an empty field
    # this saves space if it needs to be separated later
    documents <- data.frame(texts = rep(NA, length(x)), 
                            row.names = names(x),
                            check.rows = TRUE, stringsAsFactors = FALSE)

    # user-supplied document-level variables (one kind of meta-data)
    if (!is.null(docvars)) {
        if (nrow(docvars) > 0) {
            stopifnot(nrow(docvars)==length(x))
            documents <- cbind(documents, docvars)
        } 
    } else {
    }
    
    # initialize results corpus
    tempCorpus <- list()
    
    ## compress and separate texts if compress == TRUE
    # paste delimiters into object to be compressed
    if (compress) {
        x[1 : (length(x)-1)] <- paste0(x[1 : (length(x)-1)], quanteda_document_delimiter)
        # compress texts
        texts <- memCompress(x, 'gzip')
        # remove texts from documents
        documents$texts <- NULL
        tempCorpus <- c(tempCorpus, list(texts = memCompress(x, "gzip")))
    } else {
        # otherwise replace NA placeholder with the actual text
        documents$texts <- x
    }

    # build and return the corpus object
    tempCorpus <- c(tempCorpus, list(documents = documents, 
                                     metadata = metacorpus, 
                                     settings = settings(),
                                     tokens = NULL))
                    
    ## add some elements if compress
    if (compress) {
        tempCorpus$docnames <- names(x)
        # compute the compression %
        tempCorpus$compression_rate <- utils::object.size(tempCorpus$texts) / utils::object.size(unname(x)) * 100
    }
    
    class(tempCorpus) <- c("corpus", class(tempCorpus))
    if (compress) {
        class(tempCorpus) <- c("corpuszip", class(tempCorpus))
    }
    return(tempCorpus)
}

#' @rdname corpus
#' @noRd
#' @keywords corpus
#' @export
corpus.data.frame <- function(x, docnames = NULL, docvars = NULL, text_field = "text", metacorpus = NULL, compress = FALSE, ...) {

    x <- as.data.frame(x)
        
    args <- list(...)
    if (!missing(docvars))
        stop("docvars are assigned automatically for data.frames")
    
    if (is.character(text_field)) {
        text_fieldi <- which(names(x) %in% text_field)
        if (length(text_fieldi)==0)
            stop("column name ", text_field, " not found")
        text_field <- text_fieldi
    } else if (is.numeric(text_field)) {
        text_fieldi <- text_field
    } else {
        stop("text_field must be a character (variable name) or numeric index")
    }

    if (length(text_fieldi) != 1)
        stop("only one text_field may be specified")

    if (text_fieldi > ncol(x) | text_fieldi <= 0 | (text_fieldi - as.integer(text_fieldi)))
        stop("text_field index refers to an invalid column")
    
    if (!is.character(x[, text_fieldi]))
        stop("text_field must refer to a character mode column")
    
    corpus(x[, text_fieldi], 
           docvars = x[, -text_fieldi, drop = FALSE],
           docnames = if (!identical(row.names(x), as.character(1:nrow(x)))) row.names(x) else NULL, #paste0("text", 1:nrow(x)),
           metacorpus = metacorpus, compress = compress, ...)
}


#' @rdname corpus
#' @noRd
#' @keywords corpus
#' @export
corpus.kwic <- function(x, docnames = NULL, docvars = NULL, text_field = "text", metacorpus = NULL, compress = FALSE, ...) {
    
    if (compress)
        warning("compress not yet implemented for corpus.kwic")
    if (!missing(docvars))
        stop("docvars are assigned automatically for kwic objects")
    if (!missing(text_field))
        stop("text_field is not applicable for this class of input")
    
    class(x) <- "data.frame"
    
    # convert docnames to a factor, as in original kwic
    x$docname <- factor(x$docname)
    
    result <- corpus(x, text_field = "contextPre", ...)
    result[["contextPost"]] <- NULL
    result[["context"]] <- "pre"
    docnames(result) <- paste0(docnames(result), ".pre")

    tempCorp <- corpus(x, text_field = "contextPost", ...)
    tempCorp[["contextPre"]] <- NULL
    tempCorp[["context"]] <- "post"
    docnames(tempCorp) <- paste0(docnames(tempCorp), ".post")
    
    result <- result + tempCorp
    metacorpus(result, "source") <- paste0("Corpus created from kwic(x, keywords = \"", 
                                           paste(attr(x, "keywords"), collapse = ", "),
                                           "\")")

    result
}

#' @rdname corpus
#' @noRd
#' @keywords corpus
#' @export
corpus.VCorpus <- function(x, docnames = NULL, docvars = NULL, text_field = "text", metacorpus = NULL, compress = FALSE, ...) {
    
    if (!missing(docvars))
        stop("docvars are assigned automatically for tm::VCorpus objects")
    if (!missing(text_field))
        stop("text_field is not applicable for this class of input")

    # extract the content (texts)
    texts <- sapply(x$content, "[[", "content")
    # paste together texts if they appear to be vectors
    if (any(lengths(texts) > 1))
        texts <- sapply(texts, paste, collapse = " ")
    
    # special handling for VCorpus meta-data
    metad <- as.data.frame(do.call(rbind, (lapply(x$content, "[[", "meta"))),
                           stringsAsFactors = FALSE, row.names = NULL)
    makechar <- function(x) gsub("character\\(0\\)", NA, as.character(x))
    datetimestampIndex <- which(names(metad) == "datetimestamp")
    metad[, -datetimestampIndex] <- apply(metad[, -datetimestampIndex], 2, makechar)
    if (length(datetimestampIndex))
        metad$datetimestamp <- t(as.data.frame((lapply(metad$datetimestamp, as.POSIXlt))))[,1]
    # give them the underscore character required
    # names(metad) <- paste("_", names(metad), sep="")
    
    metacorpus <- c(metacorpus, list(source = paste("Converted from tm VCorpus \'", deparse(substitute(x)), "\'", sep="")))
    
    # using docvars inappropriately here but they show up as docmeta given 
    # the _ in the variable names
    corpus(texts, docvars = metad, metacorpus = metacorpus, compress = compress, ...)
}


setOldClass("corpus")
