#' construct a corpus object
#' 
#' Creates a corpus object from available sources.  The currently available  
#' sources are: 
#' \itemize{ 
#' \item a \code{character} vector, consisting of one document per element.
#' \item a \code{data.frame}, whose default variable containing the document is 
#'   character vector named \code{text}, although this can be set to any other
#'   variable name using the \code{text_field} argument.  Other variables are 
#'   imported as document-level meta-data.
#' \item a \code{kwic} object constructed by \code{\link{kwic}}.
#' \item a \pkg{tm} \link[tm]{VCorpus} class  object, with the fixed metadata 
#'   fields imported as document-level metadata. Corpus-level metadata is not 
#'   currently imported.
#' } 
#' @param x a valid corpus source object
#' @param docnames Names to be assigned to the texts, defaults to the names of 
#'   the character vector (if any), otherwise assigns "text1", "text2", etc.
#' @param docvars A data frame of attributes that is associated with each text.
#' @param source A string specifying the source of the texts, used for 
#'   referencing.
#' @param citation Information on how to cite the corpus.
#' @param notes A string containing notes about who created the text, warnings, 
#'   To Dos, etc.
#' @param text_field the character name or integer index of the source \code{data.frame}
#'   indicating the variable to be read in as text, which must be a character vector.
#'   All other variables in the data.frame will be imported as docvars.  This argument 
#'   is only used for \code{data.frame} objects.
#' @param ... not used directly
#' @return A corpus class object containing the original texts, document-level 
#'   variables, document-level metadata, corpus-level metadata, and default 
#'   settings for subsequent processing of the corpus.  
#' @section A warning on accessing corpus elements:
#'   A corpus currently 
#'   consists of an S3 specially classed list of elements, but **you should not 
#'   access these elements directly**. Use the extractor and replacement 
#'   functions instead, or else your code is not only going to be uglier, but
#'   also likely to break should the internal structure of a corpus object
#'   change (as it inevitably will as we continue to develop the package,
#'   including moving corpus objects to the S4 class system).
#' @seealso \link{corpus-class}, \link{docvars}, \link{metadoc}, \link{metacorpus}, 
#'   \link{settings}, \link{texts}, \link{ndoc}, \link{docnames}
#' @details The texts and document variables of corpus objects can also be 
#'   accessed using index notation. Indexing a corpus object as a vector will 
#'   return its text, equivalent to \code{texts(x)}.  Note that this is not the 
#'   same as subsetting the entire corpus -- this should be done using the 
#'   \code{\link{subset}} method for a corpus.
#'   
#'   Indexing a corpus using two indexes (integers or column names) will return 
#'   the document variables, equivalent to \code{docvars(x)}.  Because a corpus 
#'   is also a list, it is also possible to access, create, or replace docvars 
#'   using list notation, e.g. \code{myCorpus[["newSerialDocvar"]] <- 
#'   paste0("tag", 1:ndoc(myCorpus))}.
#'   
#'   For details, see \link{corpus-class}.
#' @author Kenneth Benoit and Paul Nulty
#' @export
#' @keywords corpus
#' @examples
#' # create a corpus from texts
#' corpus(inaugTexts)
#' 
#' # create a corpus from texts and assign meta-data and document variables
#' ukimmigCorpus <- corpus(ukimmigTexts, 
#'                         docvars = data.frame(party = names(ukimmigTexts))) 
#'
#' corpus(texts(ie2010Corpus))
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
#'     tmCorp <- tm::VCorpus(tm::VectorSource(inaugTexts[49:57]))
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
#' summary(corpus(mydf, text_field = "some_text", source = "From a data.frame called mydf."))
#' 
#' # construct a corpus from a kwic object
#' mykwic <- kwic(inaugCorpus, "southern")
#' summary(corpus(mykwic))
corpus <- function(x, ...) {
    UseMethod("corpus")
}


#' @rdname corpus
#' @export
corpus.character <- function(x, docnames = NULL, docvars = NULL,
                             source = NULL, notes = NULL, citation = NULL, ...) {
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
    if (is.null(source)) {
        source <- paste(getwd(), "/* ", "on ",  Sys.info()["machine"], " by ", Sys.info()["user"], sep="")
    }
    created <- date()
    metadata <- list(source=source, created=created, notes=notes, citation=citation)
    
    # create the documents data frame starting with the texts
    documents <- data.frame(texts=x, row.names=names(x),
                            check.rows=TRUE, stringsAsFactors=FALSE)

    # user-supplied document-level variables (one kind of meta-data)
    if (!is.null(docvars)) {
        if (nrow(docvars) > 0) {
            stopifnot(nrow(docvars)==length(x))
            documents <- cbind(documents, docvars)
        } 
    }
    
    # build and return the corpus object
    tempCorpus <- list(documents = documents, 
                       metadata = metadata, 
                       settings = settings(),
                       tokens = NULL)
    class(tempCorpus) <- list("corpus", class(tempCorpus))
    return(tempCorpus)
}

#' @rdname corpus
#' @keywords corpus
#' @export
corpus.data.frame <- function(x, text_field = "text", 
                              source = NULL, notes = NULL, citation = NULL, ...) {
    
    args <- list(...)
    if ("docvars" %in% names(args))
        stop("docvars are assigned automatically for data.frames", )
    
    if (is.character(text_field)) {
        text_fieldi <- which(names(x)==text_field)
        if (length(text_fieldi)==0)
            stop("column name ", text_field, " not found.")
        text_field <- text_fieldi
    }
    if (!is.character(x[, text_fieldi]))
        stop("text_field must refer to a character mode column")
    
    corpus(x[, text_fieldi], 
           docvars = x[, -text_fieldi, drop = FALSE],
           docnames = if (!identical(row.names(x), as.character(1:nrow(x)))) row.names(x) else NULL, #paste0("text", 1:nrow(x)),
           source = source, notes = notes,
           citation = citation, ...)
}



#' @noRd
#' @export
#' @keywords corpus
#' @examples
#' \dontrun{# the fifth column of this csv file is the text field
#' mytexts <- textfile("http://www.kenbenoit.net/files/text_example.csv", text_field = 5)
#' mycorp <- corpus(mytexts)
#' mycorp2 <- corpus(textfile("http://www.kenbenoit.net/files/text_example.csv", text_field = "Title"))
#' identical(texts(mycorp), texts(mycorp2))
#' identical(docvars(mycorp), docvars(mycorp2))
#' }
corpus.corpusSource <- function(x, ...) {
    sources <- NULL
    if (x@cachedfile == "") {
        if (prod(dim(docvars(x))) == 0)
            return(corpus(texts(x), ...))
        else
            return(corpus(texts(x), docvars = quanteda::docvars(x), ...))
    } else {
        # load from tempfile only into function environment
        load(x@cachedfile, envir = environment())
        if (prod(dim(sources$docv)) == 0)
            return(corpus(sources$txts, ...))
        else
            return(corpus(sources$txts, docvars = sources$docv, ...))
    }
}


#' @rdname corpus
#' @keywords corpus
#' @export
corpus.kwic <- function(x, ...) {
    
    args <- list(...)
    if ("docvars" %in% names(args))
        stop("docvars are assigned automatically for kwic objects", )
    
    class(x) <- "data.frame"
    
    result <- corpus(x, text_field = "contextPre", ...)
    result[["contextPost"]] <- NULL
    result[["context"]] <- "pre"
    docnames(result) <- paste0(docnames(result), ".pre")

    tempCorp <- corpus(x, text_field = "contextPost", ...)
    tempCorp[["contextPre"]] <- NULL
    tempCorp[["context"]] <- "post"
    docnames(tempCorp) <- paste0(docnames(tempCorp), ".post")
    
    result <- result + tempCorp
    metacorpus(result, "source") <- paste0("Corpus created from kwic(x, keywords = \"", attr(x, "keywords"), "\")")

    result
}

#' @rdname corpus
#' @keywords corpus
#' @export
corpus.VCorpus <- function(x, ...) {
    # extract the content (texts)
    texts <- sapply(x$content, "[[", "content")
    # paste together texts if they appear to be vectors
    if (any(lengths(texts) > 1))
        texts <- sapply(texts, paste, collapse = " ")
    
    # special handling for VCorpus meta-data
    metad <- as.data.frame(do.call(rbind, (lapply(x$content, "[[", "meta"))),
                           stringsAsFactors = FALSE, row.names = FALSE)
    makechar <- function(x) gsub("character\\(0\\)", NA, as.character(x))
    datetimestampIndex <- which(names(metad) == "datetimestamp")
    metad[, -datetimestampIndex] <- apply(metad[, -datetimestampIndex], 2, makechar)
    if (length(datetimestampIndex))
        metad$datetimestamp <- t(as.data.frame((lapply(metad$datetimestamp, as.POSIXlt))))[,1]
    # give them the underscore character required
    # names(metad) <- paste("_", names(metad), sep="")
    
    # using docvars inappropriately here but they show up as docmeta given 
    # the _ in the variable names
    corpus(texts, docvars = metad,
           source = paste("Converted from tm VCorpus \'", deparse(substitute(x)), "\'", sep=""), ...)
}



# #' @param i index for documents or rows of document variables
# #' @param j index for column of document variables
# #' @param drop if \code{TRUE} the result is coerced to the lowest possible dimension
# #'   (see the examples). This only works for extracting elements, not for the
# #'   replacement. See \code{\link{drop}} for further details.
# #' @param ... vectors or empty (missing) or \code{NULL}, see \code{\link{`[`}}
# #' @rdname corpus
# setMethod("[", signature(x = "corpus", i = "index", j = "index", drop = "ANY"),
#           function(x, i, j, ..., drop = FALSE) docvars(x)[i, j, ..., drop])
# 
# #' @rdname corpus
# setMethod("[", signature(x = "corpus", i = "index", j = "MISSING", drop = "MISSING"),
#           function(x, i, j, ..., drop = FALSE) texts(x)[i])

setOldClass("corpus")
