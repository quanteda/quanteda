#' constructor for corpus objects
#' 
#' Creates a corpus from a document source.  The current available document 
#' sources are: \itemize{ \item a character vector (as in R class \code{char}) 
#' of texts; \item a \link{corpusSource-class} object, constructed using 
#' \code{\link{textfile}}; \item a \pkg{tm} \link[tm]{VCorpus} class corpus 
#' object, meaning that anything you can use to create a \pkg{tm} corpus, 
#' including all of the tm plugins plus the built-in functions of tm for 
#' importing pdf, Word, and XML documents, can be used to create a quanteda 
#' \link{corpus}. } Corpus-level meta-data can be specified at creation, 
#' containing (for example) citation information and notes, as can 
#' document-level variables and document-level meta-data.
#' @param x a source of texts to form the documents in the corpus, a character 
#'   vector or a \link{corpusSource-class} object created using 
#'   \code{\link{textfile}}.
#' @param ... additional arguments
#' @return A corpus class object containing the original texts, document-level 
#'   variables, document-level metadata, corpus-level metadata, and default 
#'   settings for subsequent processing of the corpus.  A corpus currently 
#'   consists of an S3 specially classed list of elements, but **you should not 
#'   access these elements directly**. Use the extractor and replacement 
#'   functions instead, or else your code is not only going to be uglier, but
#'   also likely to break should the internal structure of a corpus object
#'   change (as it inevitably will as we continue to develop the package,
#'   including moving corpus objects to the S4 class system).
#' @seealso \link{docvars}, \link{metadoc}, \link{metacorpus}, \link{settings}, 
#'   \link{texts}, \link{ndoc}, \link{docnames}
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
#' @author Kenneth Benoit and Paul Nulty
#' @export
corpus <- function(x, ...) {
    UseMethod("corpus")
}


# @param enc a string specifying the input encoding for texts in the corpus. 
#   Must be a valid entry in \code{\link[stringi]{stri_enc_list}()}, since 
#   the code in \code{corpus.character} will convert this to \code{encTo} using
#   \code{\link[stringi]{stri_encode}}.  We recommend that you do
#   \strong{not} use \code{enc}, since if left \code{NULL} (the default) then
#   \code{corpus()} will detect the input encoding(s) and convert
#   automatically.
#   
#   Currently only one input encoding can be specified for a collection of 
#   input texts, meaning that you should not mix input text encoding types in a
#   single \code{corpus} call.  However if you suspect multiple encodings, omit
#   the \code{enc} argument and \code{corpus()} will detect and convert each
#   file automatically.
# @param encTo target encoding, default is UTF-8.  Unless you have strong reasons
# to use an alternative encoding, we strongly recommend you leave this at its 
# default.  Must be a valid entry in \code{\link[stringi]{stri_enc_list}()}

#' @param docnames Names to be assigned to the texts, defaults to the names of 
#'   the character vector (if any), otherwise assigns "text1", "text2", etc.
#' @param docvars A data frame of attributes that is associated with each text.
#' @param source A string specifying the source of the texts, used for 
#'   referencing.
#' @param citation Information on how to cite the corpus.
#' @param notes A string containing notes about who created the text, warnings, 
#'   To Dos, etc.
#' @rdname corpus
#' @export
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
corpus.character <- function(x, docnames = NULL, docvars = NULL,
                             source = NULL, notes = NULL, citation = NULL, ...) {
    if (length(addedArgs <- list(...)))
        warning("Argument", ifelse(length(addedArgs)>1, "s ", " "), names(addedArgs), " not used.", sep = "")
    
#     # check validity of encoding label(s)
#     if (!is.null(enc)) {
#         if (!(enc %in% stringi::stri_enc_list(simplify = TRUE))) 
#             stop("enc = ", enc, " argument not found in stri_enc_list()")
#     }
#     if (!(encTo %in% stringi::stri_enc_list(simplify = TRUE))) 
#         stop("encTo = ", enc, " argument not found in stri_enc_list()")

    x_names <- names(x)
    
    # convert the dreaded "curly quotes" to ASCII equivalents
    x <- stringi::stri_replace_all_fixed(x, 
                                         c("\u201C", "\u201D", "\u201F",
                                           "\u2018", "\u201B", "\u2019"),                                     
                                         c("\"", "\"", "\"", 
                                           "\'", "\'", "\'"), vectorize_all = FALSE)
    
    # replace all hyphens with simple hyphen
    x <- stringi::stri_replace_all_regex(x, "\\p{Pd}", "-")

    # detect encoding based on 100 documents
#     detectSampleSize <- 100
#    detectedEncoding <- encoding(x[sample(length(x), min(detectSampleSize, length(x)))], verbose = FALSE)$probably
    # catm("Detected encoding:", detectedEncoding, "\n")
#     if (!is.null(enc))
#         if (enc != detectedEncoding)
#             catm("  NOTE:", enc, "specified as input encoding, but", detectedEncoding, "detected.  Are you SURE?\n\n")
    # use specified enc, not detected encoding
#     detected <- FALSE
#     if (is.null(enc)) {
#         enc <- detectedEncoding
#         detected <- TRUE
#     }
# 
#     # convert to "enc" if not already UTF-8 **unless** ISO-8859-1 detected, in which case do not do automatically
#     if (enc != encTo) {
#         if (enc != "ISO-8859-1" & encTo == "UTF-8") {
#             catm("Non-", encTo, " encoding ", ifelse(detected, "(possibly) detected", "specified"), ": ", 
#                 enc, ".\n", sep="")
#             # suppressWarnings(x <- stringi::stri_encode(x, from = enc, to = encTo))
#         }
#     }

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
    tempCorpus <- list(documents=documents, 
                       metadata=metadata, 
                       settings=settings(),
                       tokens=NULL)
    class(tempCorpus) <- list("corpus", class(tempCorpus))
    return(tempCorpus)
}


#' @rdname corpus
#' @export
#' @examples
#' \dontrun{# the fifth column of this csv file is the text field
#' mytexts <- textfile("http://www.kenbenoit.net/files/text_example.csv", textField = 5)
#' mycorp <- corpus(mytexts)
#' mycorp2 <- corpus(textfile("http://www.kenbenoit.net/files/text_example.csv", textField = "Title"))
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
#' @note When \code{x} is a \link[tm]{VCorpus} object, the fixed metadata 
#'   fields from that object are imported as document-level metadata. Currently
#'   no corpus-level metadata is imported, but we will add that soon.
#' @examples 
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
#' @export
corpus.VCorpus <- function(x, ...) {
    # extract the content (texts)
    #texts <- sapply(x, function(x) x$content)
    # texts <- sapply(x, as.character)
    texts <- sapply(x$content, "[[", "content")
    # paste together texts if they appear to be vectors
    if (any(lengths(texts) > 1))
        texts <- sapply(texts, paste, collapse = " ")
        
    # special handling for VCorpus meta-data
    # metad <- as.data.frame(t(as.data.frame(sapply(x$content, "[[", "meta"))))
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


#' @rdname corpus
#' @param textField the character name or integer index of the source data.frame
#'   indicating the column to be read in as text.  This must be of mode
#'   character.
#' @note When \code{x} is a \code{data.frame}, then there is no encoding
#'   conversion performed on the character input.  It is highly recommended that you 
#'   detect and convert this input to UTF-8 prior to using it as input for a corpus.
#' @examples 
#' 
#' # construct a corpus from a data.frame
#' mydf <- data.frame(letter_factor = factor(rep(letters[1:3], each = 2)),
#'                   some_ints = 1L:6L,
#'                   some_text = paste0("This is text number ", 1:6, "."),
#'                   stringsAsFactors = FALSE,
#'                   row.names = paste0("fromDf_", 1:6))
#' mydf
#' summary(corpus(mydf, textField = "some_text", source = "From a data.frame called mydf."))
#' @export
corpus.data.frame <- function(x, textField, ...) {

    args <- list(...)
    if ("docvars" %in% names(args))
        stop("docvars are assigned automatically for data.frames", )

    if (is.character(textField)) {
        textFieldi <- which(names(x)==textField)
        if (length(textFieldi)==0)
            stop("column name ", textField, " not found.")
        textField <- textFieldi
    }
    if (!is.character(x[, textFieldi]))
        stop("textField must refer to a character mode column")
    
    corpus(x[, textFieldi], 
           docvars = x[, -textFieldi, drop = FALSE],
           docnames = if (!identical(row.names(x), as.character(1:nrow(x)))) row.names(x) else NULL, #paste0("text", 1:nrow(x)),
           ...)
}


#' @rdname corpus
#' @examples 
#' 
#' # construct a corpus from a kwic object
#' mykwic <- kwic(inaugCorpus, "southern")
#' summary(corpus(mykwic))
#' @export
corpus.kwic <- function(x, ...) {
    
    args <- list(...)
    if ("docvars" %in% names(args))
        stop("docvars are assigned automatically for kwic objects", )
    
    class(x) <- "data.frame"
    
    result <- corpus(x, textField = "contextPre", ...)
    result[["contextPost"]] <- NULL
    result[["context"]] <- "pre"
    docnames(result) <- paste0(docnames(result), ".pre")

    tempCorp <- corpus(x, textField = "contextPost", ...)
    tempCorp[["contextPre"]] <- NULL
    tempCorp[["context"]] <- "post"
    docnames(tempCorp) <- paste0(docnames(tempCorp), ".post")
    
    result <- result + tempCorp
    metacorpus(result, "source") <- paste0("Corpus created from kwic(x, keywords = \"", attr(x, "keywords"), "\")")

    result
}




# print a corpus object
#
# print method for corpus objects
# @param x the corpus to be printed
# @param ... further arguments passed to or from other methods
#' @export
#  see http://stackoverflow.com/questions/6517222/how-to-properly-document-a-s3-method-of-a-generic-from-a-different-package-usin
#' @method print corpus
print.corpus <- function(x, ...) {
    cat("Corpus consisting of ", format(ndoc(x), big.mark=","), " document",
        ifelse(ndoc(x)>1, "s", ""), sep = "")
    if (!is.null(docvars(x))) 
        cat(" and ", format(ncol(docvars(x)), big.mark=","), " docvar", 
            ifelse(ncol(docvars(x)) == 1, "", "s"), "", sep="")
    cat(".\n")
    
    #         ", ",
    #         ifelse(is.null(corp$tokens), "un", ""),
    #         "indexed.\n", sep="")
    #     cat("Settings:")
    #      tempSettings <- unlist(settings(corp))
    #      for (i in 1:length(tempSettings)) {
    #          print(tempSettings[i])
    #      }
}

#' @return \code{is.corpus} returns \code{TRUE} if the object is a corpus
#' @rdname corpus
#' @export
is.corpus <- function(x) {
    "corpus" %in% class(x)
}

#' get or set corpus metadata
#' 
#' Get or set the corpus-level metadata in a quanteda corpus object.
#' 
#' @param x A quanteda corpus object
#' @param field Metadata field name(s).  If \code{NULL} (default), return all
#'   metadata names.
#' @return For \code{metacorpus}, a list of the metadata fields in the corpus. 
#'   If a list is not what you wanted, you can wrap the results in \link{unlist}, 
#'   but this will remove any metadata field that is set to \code{NULL}.
#'   
#'   For \code{metacorpus <-}, the corpus with the updated metadata.
#' @export
#' @examples
#' metacorpus(inaugCorpus)
#' metacorpus(inaugCorpus, "source")
#' metacorpus(inaugCorpus, "citation") <- "Presidential Speeches Online Project (2014)."
#' metacorpus(inaugCorpus, "citation")
metacorpus <- function(x, field = NULL)
    UseMethod("metacorpus")
 
#' @rdname metacorpus
#' @export
metacorpus.corpus <- function(x, field = NULL) {
    if (!is.null(field)) {
        stopifnot(TRUE)
        ## NEED TO CHECK HERE THAT FIELD LIST MATCHES METADATA FIELD NAMES
        return(x$metadata[field])
    } else {
        return(x$metadata)
    }
}

# replacement function for corpus-level data
#' @param value new value of the corpus metadata field
#' @export
#' @rdname metacorpus
"metacorpus<-" <- function(x, field, value) {
    if (!is.null(field)) {
        stopifnot(TRUE)
        ## NEED TO CHECK HERE THAT FIELD LIST MATCHES METADATA FIELD NAMES
    }
    x$metadata[field] <- value
    x
}


# internal accessor for documents object
# @export
documents <- function(corp) {
    corp$documents
}

# internal replacement function for documents
# @export
"documents<-" <- function(corp, value) {
    corp$documents <- value
    corp
}


#' get corpus texts
#' 
#' Get the texts in a quanteda corpus object, with grouping options.  Works for plain character
#' vectors too, if \code{groups} is a factor.
#' @param x A quanteda corpus object
#' @param groups character vector containing the names of document variables in
#'   a corpus, or a factor equal in length to the number of documents, used for 
#'   aggregating the texts through concatenation.  If \code{x} is of type character,
#'   then \code{groups} must be a factor.
#' @param ... unused
#' @return For \code{texts}, a character vector of the texts in the corpus.
#'   
#'   For \code{texts <-}, the corpus with the updated texts.
#' @export
#' @examples
#' nchar(texts(subset(inaugCorpus, Year < 1806)))
#' 
#' # grouping on a document variable
#' nchar(texts(subset(inaugCorpus, Year < 1806), groups = "President"))
#' 
#' # grouping a character vector using a factor
#' nchar(inaugTexts[1:5])
#' nchar(texts(inaugTexts[1:5], groups = as.factor(inaugCorpus[1:5, "President"])))
texts <- function(x, groups = NULL, ...) {
    if (length(addedArgs <- list(...)))
        warning("Argument", ifelse(length(addedArgs)>1, "s ", " "), names(addedArgs), " not used.", sep = "")
    UseMethod("texts")
}

#' @rdname texts
#' @export
texts.corpus <- function(x, groups = NULL, ...) {
    txts <- documents(x)$texts
    
    # without groups
    if (is.null(groups)) {
        names(txts) <- docnames(x)
        return(txts)
    }
        
    # with groups as a factor
    if (any(!groups %in% names(docvars(x)))) {
        stop("Check your docvar names.", 
             ifelse(length(groups) == ndoc(x), "  Try groups = as.factor()...", ""))
    }
    if (is.factor(groups)) {
        group.split <- groups
    } else {
#        if (length(groups) > 1) {
#            # if more than one grouping variable
            group.split <- as.factor(interaction(documents(x)[, groups], drop = TRUE))
#        } else {
#            # if only one grouping variable
#            group.split <- as.factor(documents(x)[, groups])
#        }
    }
    texts(txts, groups = group.split)
}

#' @rdname texts
#' @export
texts.character <- function(x, groups = NULL, ...) {
    if (is.null(groups)) return(x)
    if (!is.factor(groups)) stop("groups must be a factor")
    x <- split(x, groups)
    sapply(x, paste, collapse = " ")
}


#' @param value character vector of the new texts
#' @rdname texts
#' @export
"texts<-" <- function(x, value) {
    UseMethod("texts<-")
}

#' @rdname texts
#' @export
#' @note You are strongly encouraged as a good practice of text analysis 
#'   workflow \emph{not} to modify the substance of the texts in a corpus. 
#'   Rather, this sort of processing is better performed through downstream 
#'   operations.  For instance, do not lowercase the texts in a corpus, or you 
#'   will never be able to recover the original case.  Rather, apply 
#'   \code{\link{toLower}} to the corpus and use the result as an input, e.g. to
#'   \code{\link{tokenize}}.
#' @examples 
#' 
#' BritCorpus <- corpus(c("We must prioritise honour in our neighbourhood.", 
#'                        "Aluminium is a valourous metal."))
#' texts(BritCorpus) <- 
#'     stringi::stri_replace_all_regex(texts(BritCorpus),
#'                                    c("ise", "([nlb])our", "nium"),
#'                                    c("ize", "$1or", "num"),
#'                                    vectorize_all = FALSE)
#' texts(BritCorpus)
#' texts(BritCorpus)[2] <- "New text number 2."
#' texts(BritCorpus)
"texts<-.corpus" <- function(x, value) { 
    documents(x)$texts <- value
    x
}


#' get or set document-level meta-data
#' 
#' Get or set the document-level meta-data, including reserved fields for 
#' language and corpus.
#' @param x A quanteda corpus object
#' @param field character, the name of the metadata field(s) to be queried or set
#' @return For \code{texts}, a character vector of the texts in the corpus.
#'   
#'   For \code{texts <-}, the corpus with the updated texts.
#' @note Document-level meta-data names are preceded by an underscore character,
#'   such as \code{_language}, but when named in in the \code{field} argument,
#'   do \emph{not} need the underscore character.
#' @export
metadoc <- function(x, field = NULL) 
    UseMethod("metadoc")

#' @rdname metadoc 
#' @export
#' @examples
#' mycorp <- subset(inaugCorpus, Year>1990)
#' summary(mycorp, showmeta = TRUE)
#' metadoc(mycorp, "encoding") <- "UTF-8"
#' metadoc(mycorp)
#' metadoc(mycorp, "language") <- "english"
#' summary(mycorp, showmeta = TRUE)
metadoc.corpus <- function(x, field = NULL) {
    # CHECK TO SEE THAT VALUE LIST IS IN VALID DOCUMENT-LEVEL METADATA LIST
    # (this check not yet implemented)
    if (length(field) > 1)
        stop("cannot assign multiple fields.")
    if (is.null(field)) {
        documents(x)[, grep("^\\_", names(documents(x))), drop=FALSE]
    } else {
        ## error if field not defined in data
        fieldname <- ifelse(substr(field, 1, 1)=="_", 
                            field, 
                            paste("_", field, sep=""))
        documents(x)[, fieldname, drop=FALSE]
    }
}

#' @param value the new value of the new meta-data field
#' @rdname metadoc
#' @export
"metadoc<-" <- function(x, field = NULL, value) 
    UseMethod("metadoc")

#' @rdname metadoc
#' @export
"metadoc<-" <- function(x, field = NULL, value) {
    # CHECK TO SEE THAT VALUE LIST IS IN VALID DOCUMENT-LEVEL METADATA LIST
    # (this check not yet implemented)
    if (is.null(field)) {
        field <- paste("_", names(value), sep="")
        if (is.null(field))
            field <- paste("_metadoc", 1:ncol(as.data.frame(value)), sep="")
    } else {
        field <- paste("_", field, sep="")
    }
    documents(x)[field] <- value
    x
}

# replacement function for document-level metadata
#
# to get this to work with indexes, e.g. 
# metadoc(UDHRcorpus, "language")[1] <- "1st Row Only"
# or
# language(UDHRcorpus)[1] <- "1st row only"
# is trickier.  Solution lies in nesting a complex "[" function
# inside the calling function: see http://cran.r-project.org/doc/manuals/R-lang.html#Subset-assignment
#
# @export
# "metadoc<-[" <- function(corp, value, field) {
#     # CHECK TO SEE THAT VALUE LIST IS IN VALID DOCUMENT-LEVEL METADATA LIST
#     # (this check not yet implemented)
#     field <- paste("_", field, sep="")
#     documents(corp)[field] <- value
#     corp
# }



#' get or set for document-level variables
#' 
#' Get or set variables for the documents in a corpus
#' @param x corpus whose document-level variables will be read or set
#' @param field string containing the document-level variable name
#' @return \code{docvars} returns a data.frame of the document-level variables
#' @examples head(docvars(inaugCorpus))
#' @export
docvars <- function(x, field = NULL) {
    UseMethod("docvars")
}

#' @rdname docvars
#' @export
docvars.corpus <- function(x, field = NULL) {
    docvarsIndex <- intersect(which(substr(names(documents(x)), 1, 1) != "_"),
                              which(names(documents(x)) != "texts"))
    if (length(docvarsIndex)==0)
        return(NULL)
    if (is.null(field))
        return(documents(x)[, docvarsIndex, drop=FALSE])
    return(documents(x)[, field, drop=TRUE])
}

#' @rdname docvars
#' @param value the new values of the document-level variable
#' @note Another way to access and set docvars is through indexing of the corpus \code{j} element, 
#' such as \code{ie2010Corpus[, c("foren", "name"]} or for a single docvar, \code{ie2010Corpus[["name"]]}.  The latter
#' also permits assignment, including the easy creation of new document varibles, e.g. \code{ie2010Corpus[["newvar"]] <- 1:ndoc(ie2010Corpus)}.
#' See \code{\link{[.corpus}} for details.
#' @return \code{docvars<-} assigns \code{value} to the named \code{field}
#' @examples 
#' docvars(inaugCorpus, "President") <- paste("prez", 1:ndoc(inaugCorpus), sep="")
#' head(docvars(inaugCorpus))
#' 
#' # alternative using indexing
#' head(inaugCorpus[, "Year"])
#' inaugCorpus[["President2"]] <- paste("prezTwo", 1:ndoc(inaugCorpus), sep="")
#' head(docvars(inaugCorpus))
#' @export
"docvars<-" <- function(x, field = NULL, value) {
    UseMethod("docvars<-")
}


#' @rdname docvars
#' @export
"docvars<-.corpus" <- function(x, field = NULL, value) {
    if ("texts" %in% field) stop("You should use texts() instead to replace the corpus texts.")
    if (is.null(field)) {
        field <- names(value)
        if (is.null(field))
            field <- paste("docvar", 1:ncol(as.data.frame(value)), sep="")
    }
    documents(x)[field] <- value
    x
}


# accessor for tokens
# 
# Get the tokens object from a corpus
# @export
#  return(corp$docvars$tokens)
tokens <- function(x) {
    UseMethod("tokens")
}
tokens.corpus <- function(corp) {
    corp$tokens
}

# # replacement function for tokens
# # @export
#  corp$docvars$tokens <- value
#  return(corp)
# #"tokens<-" <- function(corp, value){
#}
# # @export
#types <- function(corp) {
#  return(unique(unlist(tokens(corp))))
#}

#' get or set document names
#' 
#' Get or set the document names from a corpus or a document-feature matrix.
#' of the \link{dfm} object.
#' @param x the object with docnames
#' @export
docnames <- function(x) {
    UseMethod("docnames")
}

#' @return \code{docnames} returns a character vector of the document names
#' @export
#' @rdname docnames
docnames.corpus <- function(x) {
    # didn't use accessor documents() because didn't want to pass
    # that large object
    rownames(x$documents)
}

#' @param value a character vector of the same length as \code{x}
#' @return \code{docnames <-} assigns new values to the document names of a corpus. (Does not work
#' for dfm objects, whose document names are fixed.)
#' @export
#' @examples 
#' # query the document names of the inaugural speech corpus
#' docnames(inaugCorpus) <- paste("Speech", 1:ndoc(inaugCorpus), sep="")
#' 
#' # reassign the document names of the inaugural speech corpus
#' docnames(inaugCorpus) <- paste("Speech", 1:ndoc(inaugCorpus), sep="")
#' 
#' @rdname docnames
"docnames<-" <- function(x, value) {
    if (!is.corpus(x))
        stop("docnames<-  only valid for corpus objects.")
    rownames(x$documents) <- value
    return(x)
}

#' get the number of documents or features
#' 
#' \code{ndoc} returns the number of documents or features in a quanteda object, which can be 
#' a corpus, dfm, or tokenized texts.
#' @param x a corpus or dfm object
#' @return an integer (count) of the number of documents or features in the corpus or dfm
#' @export
ndoc <- function(x) {
    UseMethod("ndoc")
}

#' @rdname ndoc
#' @examples 
#' ndoc(subset(inaugCorpus, Year>1980))
#' ndoc(dfm(subset(inaugCorpus, Year>1980), verbose=FALSE))
#' @export
ndoc.corpus <- function(x) {
    nrow(x$documents)
}

# # get or set the language of corpus documents
# # 
# # Get or set the \code{_language} document-level metadata field in a corpus.
# # @param corp a corpus object
# # @param drop return as a vector if \code{TRUE}, otherwise return a \code{data.frame}
# # @details This function modifies the \code{_language} value set by
# #   \code{\link{metadoc}}.  It is a wrapper for \code{metadoc(corp, "language")}.
# # @export
# language <- function(corp, drop=TRUE) {
#     if ("_language" %in% names(metadoc(corp))) {
#         result <- metadoc(corp, "language")
#         return(result[,1, drop=drop])
#     } else
#         return(rep(NULL, ndoc(corp)))
# }
# 
# # @rdname language
# # @param value the new value for the language meta-data field, a string or
# #   character vector equal in length to \code{ndoc(corp)}
# # @export
# "language<-" <- function(corp, value){
#     metadoc(corp, "language") <- value
#     # corp$documents$"_language" <- value
#     corp
# }



#' Randomly sample documents or features
#' 
#' Takes a random sample or documents or features of the specified size from a 
#' corpus or document-feature matrix, with or without replacement
#' 
#' @param x a corpus or dfm object whose documents or features will be sampled
#' @param size a positive number, the number of documents to select
#' @param replace Should sampling be with replacement?
#' @param prob A vector of probability weights for obtaining the elements of the
#'   vector being sampled.
#' @param ... unused
#'   \code{\link[base]{sample}}, which is not defined as a generic
#'   method in the \pkg{base} package.
#' @export
sample <- function(x, size, replace = FALSE, prob = NULL, ...) {
    UseMethod("sample")
}

#' @export
#' @rdname sample
sample.default <- function(x, size, replace = FALSE, prob = NULL, ...) {
    if (length(addedArgs <- list(...)))
        warning("Argument", ifelse(length(addedArgs)>1, "s ", " "), names(addedArgs), " not used.", sep = "")
    base::sample(x, size, replace, prob)
}


#' @export
#' @return A corpus object with number of documents equal to \code{size}, drawn 
#'   from the corpus \code{x}.  The returned corpus object will contain all of 
#'   the meta-data of the original corpus, and the same document variables for 
#'   the documents selected.
#' @seealso \code{\link{sample}}
#' @rdname sample
#' @examples
#' # sampling from a corpus
#' summary(sample(inaugCorpus, 5)) 
#' summary(sample(inaugCorpus, 10, replace=TRUE))
sample.corpus <- function(x, size = ndoc(x), replace = FALSE, prob = NULL, ...) {
    documents(x) <- documents(x)[sample(ndoc(x), size, replace, prob), , drop = FALSE]
    x
}

#' extract a subset of a corpus
#' 
#' Returns subsets of a corpus that meet certain conditions, including direct 
#' logical operations on docvars (document-level variables).  Works just like
#' the normal subset command but for corpus objects.
#' 
#' @param x corpus object to be subsetted.
#' @param subset logical expression indicating elements or rows to keep: missing
#'   values are taken as false.
#' @param select expression, indicating the attributes to select from the corpus
#' @param ... not used
#' @return corpus object
#' @export
#' @seealso \code{\link{select}}
#' @examples
#' summary(subset(inaugCorpus, Year>1980))
#' summary(subset(inaugCorpus, Year>1930 & President=="Roosevelt", select=Year))
subset.corpus <- function(x, subset, select, ...) {
    if (length(addedArgs <- list(...)))
        warning("Argument", ifelse(length(addedArgs)>1, "s ", " "), names(addedArgs), " not used.", sep = "")
    r <- if (missing(subset)) {
        rep_len(TRUE, nrow(documents(x)))
    } else {
        e <- substitute(subset)
        r <- eval(e, documents(x), parent.frame())
        r & !is.na(r)
    }
    vars <- if (missing(select)) 
        TRUE
    else {
        nl <- as.list(seq_along(documents(x)))
        names(nl) <- names(documents(x))
        c(1, eval(substitute(select), nl, parent.frame()))
    }
    documents(x) <- documents(x)[r, vars, drop = FALSE]
    x
}

#' summarize a corpus or a vector of texts
#' 
#' Displays information about a corpus or vector of texts.  For a corpus, this 
#' includes attributes and metadata such as date of number of texts, creation 
#' and source.  For texts, prints to the console a desription of the texts,
#' including number of types, tokens, and sentences.
#' 
#' @param object corpus or texts to be summarized
#' @param n maximum number of texts to describe, default=100
#' @param verbose set to \code{FALSE} to turn off printed output, for instance
#'   if you simply want to assign the output to a \code{data.frame}
#' @param showmeta for a corpus, set to \code{TRUE} to include document-level
#'   meta-data
#' @param toLower convert texts to lower case before counting types
#' @param ... additional arguments passed through to \code{\link{tokenize}}
#' @export
#' @method summary corpus
#' @examples
#' # summarize corpus information
#' summary(inaugCorpus)
#' summary(inaugCorpus, n=10)
#' mycorpus <- corpus(ukimmigTexts, docvars=data.frame(party=names(ukimmigTexts)), enc="UTF-8")
#' summary(mycorpus, showmeta=TRUE)  # show the meta-data
#' mysummary <- summary(mycorpus, verbose=FALSE)  # (quietly) assign the results
#' mysummary$Types / mysummary$Tokens             # crude type-token ratio
summary.corpus <- function(object, n = 100, verbose = TRUE, showmeta = FALSE, toLower = FALSE, ...) {
#     if (!(addedArgs <- names(list(...)) %in% )
#         warning("Argument", ifelse(length(addedArgs)>1, "s ", " "), names(addedArgs), " not used.", sep = "")
     
    if (verbose) {
        cat("Corpus consisting of ", ndoc(object), " document",
            ifelse(ndoc(object)>1, "s", ""), 
            ifelse(ndoc(object)<=n, "", 
                   paste(", showing ", n, " document", ifelse(n>1, "s", ""), sep="")),
            ".\n", sep="")
    }
    
    if (verbose) cat("\n")
    ### Turn off describeTexts until we can speed this up
    # dtexts <- describeTexts(texts(object), verbose=FALSE)
    outputdf <- data.frame(summary(texts(object), n, verbose = FALSE, toLower = toLower, ...))
    if (!is.null(docvars(object)))
        outputdf <- cbind(outputdf, docvars(object)[1:min(c(n, ndoc(object))),, drop=FALSE])
    # if (detail) outputdf <- cbind(outputdf, metadoc(object))
    if (showmeta)
        outputdf[names(metadoc(object))] <- metadoc(object)[1:min(c(n, ndoc(object))),,drop=FALSE]
    if (verbose) {
        print(head(outputdf, n), row.names=FALSE)
        cat("\nSource:  ", unlist(metacorpus(object, "source")), "\n", sep="")
        cat("Created: ",   unlist(metacorpus(object, "created")), "\n", sep="")
        cat("Notes:   ",   unlist(metacorpus(object, "notes")), "\n\n", sep="")
    }
    # invisibly pass the summary of the texts from describetexts()
    return(invisible(outputdf))
}



#' change the document units of a corpus
#' 
#' For a corpus, recast the documents down or up a level of aggregation.  "Down"
#' would mean going from documents to sentences, for instance.  "Up" means from 
#' sentences back to documents.  This makes it easy to reshape a corpus from a 
#' collection of documents into a collection of sentences, for instance.
#' (Because the corpus object records its current "units" status, there is no 
#' \code{from} option, only \code{to}.)
#' @param x corpus whose document units will be reshaped
#' @param to new documents units for the corpus to be recast in
#' @param ... not used
#' @export
changeunits <- function(x, ...) 
    UseMethod("changeunits")

#' @rdname changeunits
#' @return A corpus object with the documents defined as the new units,
#'   including document-level meta-data identifying the original documents.
#' @export
#' @examples
#' # simple example
#' mycorpus <- corpus(c(textone = "This is a sentence.  Another sentence.  Yet another.", 
#'                      textwo = "Premiere phrase.  Deuxieme phrase."), 
#'                    docvars = data.frame(country=c("UK", "USA"), year=c(1990, 2000)),
#'                    notes = "This is a simple example to show how changeunits() works.")
#' summary(mycorpus)
#' summary(changeunits(mycorpus, to = "sentences"), showmeta=TRUE)
#' 
#' # example with inaugural corpus speeches
#' (mycorpus2 <- subset(inaugCorpus, Year>2004))
#' paragCorpus <- changeunits(mycorpus2, to="paragraphs")
#' paragCorpus
#' summary(paragCorpus, 100, showmeta=TRUE)
#' ## Note that Bush 2005 is recorded as a single paragraph because that text used a single
#' ## \n to mark the end of a paragraph.
changeunits.corpus <- function(x, to = c("sentences", "paragraphs", "documents"), ...) {
    to <- match.arg(to)
    if (to == "documents") stop("documents not yet implemented.")
    
    if (length(addedArgs <- names(list(...))))
        warning("Argument", ifelse(length(addedArgs)>1, "s ", " "), names(addedArgs), " not used.", sep = "")
    
    # make the new corpus
    segmentedTexts <- segment(texts(x), to)
    lengthSegments <- sapply(segmentedTexts, length)
    newcorpus <- corpus(unlist(segmentedTexts))
    # repeat the docvars and existing document metadata
    docvars(newcorpus, names(docvars(x))) <- as.data.frame(lapply(docvars(x), rep, lengthSegments))
    docvars(newcorpus, names(metadoc(x))) <- as.data.frame(lapply(metadoc(x), rep, lengthSegments))
    # add original document name as metadata
    metadoc(newcorpus, "document") <- rep(names(segmentedTexts), lengthSegments)
    # give a serial number (within document) to each sentence
    sentenceid <- lapply(lengthSegments, function(n) seq(from=1, to=n))
    metadoc(newcorpus, "serialno") <- unlist(sentenceid, use.names=FALSE)
    
    # copy settings and corpus metadata
    newcorpus$settings <- x$settings
    newcorpus$metadata <- x$metadata
    
    # modify settings flag for changeunits info
    settings(newcorpus, "unitsoriginal") <- settings(newcorpus, "units")
    settings(newcorpus, "units") <- to
    
    newcorpus
}

rep.data.frame <- function(x, ...)
    as.data.frame(lapply(x, rep, ...))

#' @rdname corpus
#' @param c1 corpus one to be added
#' @param c2 corpus two to be added
#' @details The \code{+} operator for a corpus object will combine two corpus 
#'   objects, resolving any non-matching \code{\link{docvars}} or 
#'   \code{\link{metadoc}} fields by making them into \code{NA} values for the 
#'   corpus lacking that field.  Corpus-level meta data is concatenated, except 
#'   for \code{source} and \code{notes}, which are stamped with information 
#'   pertaining to the creation of the new joined corpus.
#'   
#'   The `c()` operator is also defined for corpus class objects, and provides an easy way to 
#'   combine multiple corpus objects.
#'   
#'   There are some issues that need to be addressed in future revisions of 
#'   quanteda concerning the use of factors to store document variables and 
#'   meta-data.  Currently most or all of these are not recorded as factors, 
#'   because we use \code{stringsAsFactors=FALSE} in the 
#'   \code{\link{data.frame}} calls that are used to create and store the 
#'   document-level information, because the texts should always be stored as
#'   character vectors and never as factors. 
#' @export
`+.corpus` <- function(c1, c2) {
    ## deal with metadata first
    # note the source and date/time-stamp the creation
    metacorpus(c1, "source") <- paste("Combination of corpuses", deparse(substitute(c1)),
                                      "and", deparse(substitute(c2)))
    metacorpus(c1, "created") <- date()
    # concatenate the other fields if not identical already
    for (field in names(metacorpus(c2))) {
        if (field %in% c("source", "created")) next
        if (!identical(metacorpus(c1, field), metacorpus(c2, field)))
            metacorpus(c1, field) <- paste(metacorpus(c1, field), metacorpus(c2, field))
    }
    
    row.names <- c(rownames(c1$documents), rownames(c2$documents))
    c1$documents <- data.frame(
       data.table::rbindlist(list(c1$documents, c2$documents), use.names=T, fill=T)
    )
    #  Put rownames back in because the hadleyverse discards them
    rownames(c1$documents) <- make.unique(row.names, sep='')

    # settings
    ### currently just use the c1 settings
    
    return(c1)
}


#' @rdname corpus
#' @param recursive logical used by `c()` method, always set to `FALSE`
#' @examples 
#' 
#' # concatenate corpus objects
#' corpus1 <- corpus(inaugTexts[1:2])
#' corpus2 <- corpus(inaugTexts[3:4])
#' corpus3 <- subset(inaugCorpus, President == "Obama")
#' summary(c(corpus1, corpus2, corpus3))
#' @export
c.corpus <- function(..., recursive = FALSE) {
    dots <- list(...)
    if (length(dots) == 1) return(dots[[1]])
    result <- dots[[1]] + dots[[2]]
    if (length(dots) == 2) return(result)
    for (i in 3:length(dots))
        result <- result + dots[[i]]
    metacorpus(result, "source") <- paste0("Concatenation by c.corpus(", names(dots), ")")
    return(result)
}


#' count the number of tokens or types
#' 
#' Return the count of tokens (total features) or types (unique features) in a
#' text, corpus, or dfm.  "tokens" here means all words, not unique words, and
#' these are not cleaned prior to counting.
#' @param x texts or corpus whose tokens or types will be counted
#' @param ... additional arguments passed to \code{\link{tokenize}}
#' @note Due to differences between raw text tokens and features that have been 
#'   defined for a \link{dfm}, the counts be different for dfm objects and the 
#'   texts from which the dfm was generated.  Because the method tokenizes the 
#'   text in order to count the tokens, your results will depend on the options 
#'   passed through to \code{\link{tokenize}}
#' @return scalar count of the total tokens or types
#' @examples
#' # simple example
#' txt <- c(text1 = "This is a sentence, this.", text2 = "A word. Repeated repeated.")
#' ntoken(txt)
#' ntype(txt)
#' ntoken(toLower(txt))  # same
#' ntype(toLower(txt))   # fewer types
#' ntoken(toLower(txt), removePunct = TRUE)
#' ntype(toLower(txt), removePunct = TRUE)
#' 
#' # with some real texts
#' ntoken(subset(inaugCorpus, Year<1806, removePunct = TRUE))
#' ntype(subset(inaugCorpus, Year<1806, removePunct = TRUE))
#' ntoken(dfm(subset(inaugCorpus, Year<1800)))
#' ntype(dfm(subset(inaugCorpus, Year<1800)))
#' @export
ntoken <- function(x, ...) {
    UseMethod("ntoken")
}

#' @rdname ntoken
#' @export
ntype <- function(x, ...) {
    UseMethod("ntype")
}

#' @rdname ntoken
#' @export
ntoken.corpus <- function(x, ...) {
    ntoken(texts(x), ...)
}

#' @rdname ntoken
#' @export
ntype.corpus <- function(x, ...) {
    ntype(texts(x), ...)
}


#' @rdname ntoken
#' @export
ntoken.character <- function(x, ...) {
    ntoken(tokenize(x, ...))
}

#' @rdname ntoken
#' @export
ntoken.tokenizedTexts <- function(x, ...) {
    sapply(x, length)
}

#' @rdname ntoken
#' @export
ntype.character <- function(x, ...) {
    ntype(tokenize(x, ...))
}


#' @rdname ntoken
#' @export
ntoken.dfm <- function(x, ...) {
    if (length(list(...)) > 0)
        warning("additional arguments not used for ntoken.dfm()")
    rowSums(x)
}

#' @rdname ntoken
#' @export
ntype.dfm <- function(x, ...) {
    if (length(list(...)) > 0)
        warning("additional arguments not used for ntoken.dfm()")
    # apply(x, 1, function(dfmrow) sum(dfmrow > 0))
    tmp <- sparseMatrix(i = x@i, p = x@p, x = x@x > 0, index1 = FALSE, dims = x@Dim)
    tmp <- rowSums(tmp)
    names(tmp) <- docnames(x)
    tmp
}

#' @rdname ntoken
#' @export
ntype.tokenizedTexts <- function(x, ...) {
    sapply(lapply(x, unique), length)
}
    

#' count the number of sentences
#' 
#' Return the count of sentences in a corpus or character.
#' @param x texts or corpus whose sentences will be counted
#' @param ... additional arguments passed to \code{\link{tokenize}}
#' @note `nsentence()` relies on the boundaries definitions in the \pkg{stringi}
#'   package (see \link[stringi]{stri_opts_brkiter}).  It does not count
#'   sentences correctly if the text has been transformed to lower case, and for
#'   this reason `nsentence()` will stop with an error if it detects all
#'   lower-cased text.
#' @return scalar count(s) of the total sentences per text
#' @examples
#' # simple example
#' txt <- c(text1 = "This is a sentence: second part of first sentence.",
#'          text2 = "A word. Repeated repeated.")
#' nsentence(txt)
#' @export
nsentence <- function(x, ...) {
    UseMethod("nsentence")
}

#' @rdname nsentence
#' @export
nsentence.character <- function(x, ...) {
    upcase <- try(any(stringi::stri_detect_charclass(x, "[A-Z]")), silent = TRUE)
    if (!is.logical(upcase)) {
        # warning("Input text contains non-UTF-8 characters.")
    }
    else if (!upcase)
        warning("nsentence() does not correctly count sentences in all lower-cased text")
    lengths(tokenize(x, what = "sentence", ...))
}

#' @rdname nsentence
#' @export
nsentence.corpus <- function(x, ...) {
    nsentence(texts(x), ...)
}

#' @export
#' @param i index for documents or rows of document variables
#' @param j index for column of document variables
#' @param drop if \code{TRUE}, return a vector if extracting a single document
#'   variable; if \code{FALSE}, return it as a single-column data.frame.  See
#'   \code{\link{drop}} for further details.
#' @method [ corpus
#' @rdname corpus
#' @examples 
#' 
#' # ways to index corpus elements
#' inaugCorpus["1793-Washington"]    # 2nd Washington inaugural speech
#' inaugCorpus[2]                    # same
#' ie2010Corpus[, "year"]            # access the docvars from ie2010Corpus
#' ie2010Corpus[["year"]]            # same
#' 
#' # create a new document variable
#' ie2010Corpus[["govtopp"]] <- ifelse(ie2010Corpus[["party"]] %in% c("FF", "Greens"), 
#'                                     "Government", "Opposition")
#' docvars(ie2010Corpus)
`[.corpus` <- function(x, i, j = NULL, ..., drop = TRUE) {
    if (is.null(j))
        return(texts(x)[i, ...])
    else {
        if (!is.null(docvars(x)))
        x$documents <- x$documents[-1]  # remove texts
        return(x$documents[i, j, ..., drop = drop])
    }
}

#' @export
#' @method [[ corpus
#' @rdname corpus
`[[.corpus` <- function(x, i, ...) {
    if (is.null(docvars(x)))
        stop("cannot index docvars this way because none exist")
    x$documents <- x$documents[-1]  # remove texts
    x$documents[[i, ...]]
}

#' @export
#' @param value a vector that will form a new docvar
#' @method [[<- corpus
#' @rdname corpus
`[[<-.corpus` <- function(x, i, value) {
    x$documents[[i]] <- value
    x
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
