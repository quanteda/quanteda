#' Construct a corpus object
#'
#' Creates a corpus object from available sources.  The currently available
#' sources are:
#' \itemize{
#' \item a \link{character} vector, consisting of one document per element; if
#'   the elements are named, these names will be used as document names.
#' \item a \link{data.frame} (or a \pkg{tibble} \code{tbl_df}), whose default
#' document id is a variable identified by \code{docid_field}; the text of the
#' document is a variable identified by \code{textid_field}; and other variables
#' are imported as document-level meta-data.  This matches the format of
#' data.frames constructed by the the \pkg{readtext} package.
#' \item a \link{kwic} object constructed by \code{\link{kwic}}.
#' \item a \pkg{tm} \link[tm]{VCorpus} or \link[tm]{SimpleCorpus} class  object,
#'   with the fixed metadata
#'   fields imported as \link{docvars} and corpus-level metadata imported
#'   as \link{metacorpus} information.
#' \item a \link{corpus} object.
#' }
#' @param x a valid corpus source object
#' @param docnames Names to be assigned to the texts.  Defaults to the names of
#'   the character vector (if any); \code{doc_id} for a data.frame; the document
#'   names in a \pkg{tm} corpus; or a vector of user-supplied labels equal in
#'   length to the number of documents.  If none of these are round, then
#'   "text1", "text2", etc. are assigned automatically.
#' @param docvars a data.frame of document-level variables associated with each text
#' @param text_field the character name or numeric index of the source
#'   \code{data.frame} indicating the variable to be read in as text, which must
#'   be a character vector. All other variables in the data.frame will be
#'   imported as docvars.  This argument is only used for \code{data.frame}
#'   objects (including those created by \pkg{readtext}).
#' @param metacorpus a named list containing additional (character) information
#'   to be added to the corpus as corpus-level metadata.  Special fields
#'   recognized in the \code{\link{summary.corpus}} are:
#' \itemize{
#' \item{\code{source }}{a description of the source of the texts, used for
#'   referencing;}
#' \item{\code{citation }}{information on how to cite the corpus; and}
#' \item{\code{notes }}{any additional information about who created the text, warnings,
#'   to do lists, etc.}
#' }
#' @param ... not used directly
#' @return A \link{corpus-class} class object containing the original texts,
#'   document-level variables, document-level metadata, corpus-level metadata,
#'   and default settings for subsequent processing of the corpus.
#' @section A warning on accessing corpus elements: A corpus currently consists
#'   of an S3 specially classed list of elements, but \strong{you should not
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
#'   the document variables, equivalent to \code{docvars(x)}.  It is also
#'   possible to access, create, or replace docvars using list notation, e.g.
#'
#'   \code{myCorpus[["newSerialDocvar"]] <-
#'   paste0("tag", 1:ndoc(myCorpus))}.
#'
#'   For details, see \link{corpus-class}.
#' @author Kenneth Benoit and Paul Nulty
#' @export
#' @keywords corpus2
#' @examples
#' # create a corpus from texts
#' corp2 <- corpus2(data_char_ukimmig2010)
#'
#' # create a corpus from texts and assign meta-data and document variables
#' summary(corpus2(data_char_ukimmig2010,
#'                docvars = data.frame(party = names(data_char_ukimmig2010))), 5)
#'
#' corpus2(texts(data_corpus_irishbudget2010))
#'
#'
#' # construct a corpus from a data.frame
#' mydf <- data.frame(letter_factor = factor(rep(letters[1:3], each = 2)),
#'                   some_ints = 1L:6L,
#'                   some_text = paste0("This is text number ", 1:6, "."),
#'                   stringsAsFactors = FALSE,
#'                   row.names = paste0("fromDf_", 1:6))
#' mydf
#' summary(corpus2(mydf, text_field = "some_text"))
#'
#' # construct a corpus from a kwic object
#' mykwic <- kwic(data_corpus_inaugural, "southern")
#' summary(corpus2(mykwic))
corpus2 <- function(x, ...) {
    UseMethod("corpus2")
}

#' @rdname corpus2
#' @noRd
#' @export
corpus2.default <- function(x, ...) {
    stop(friendly_class_undefined_message(class(x), "corpus2"))
}

#' @rdname corpus2
#' @export
corpus2.corpus2 <- function(x, docnames = quanteda::docnames(x), 
                            docvars = quanteda::docvars(x),  ...) {
    corpus2(texts(x), docnames, docvars)
}

#' @rdname corpus2
#' @export
corpus2.character <- function(x, docnames = NULL, 
                              docvars = NULL, ...) {
    
    unused_dots(...)
    x[is.na(x)] <- ""
    
    if (!is.null(docnames)) {
        if(length(docnames) != length(x))
            stop("docnames must the the same length as x")
        docname <- docnames
    } else if (!is.null(names(x))) {
        docname <- names(x)
    } else {
        docname <- paste0(quanteda_options("base_docname"), seq_along(x))
    }
    
    # ensure that docnames are unique
    #if (any(duplicated(docname)))
    #    docname <- make.unique(docname)
    
    # convert the dreaded "curly quotes" to ASCII equivalents
    x <- stri_replace_all_fixed(x,
                                c("\u201C", "\u201D", "\u201F", 
                                  "\u2018", "\u201B", "\u2019"),
                                c("\"", "\"", "\"",
                                  "\'", "\'", "\'"), vectorize_all = FALSE)
    
    # replace all hyphens with simple hyphen
    x <- stri_replace_all_regex(x, "\\p{Pd}", "-")
    
    # normalize EOL
    x <- stri_replace_all_fixed(x, "\r\n", "\n") # Windows
    x <- stri_replace_all_fixed(x, "\r", "\n") # Old Macintosh
    
    docvars_internal <- data.frame("_docname" = docname,
                                   "_docid" = seq_along(x), 
                                   "_segid" = rep(1, length(x)), 
                                   check.names = FALSE,
                                   stringsAsFactors = FALSE)
    
    if (!is.null(docvars) && nrow(docvars) > 0) {
        if (any(is_internal(names(docvars))))
            message_error("docvar_invalid")
        docvars <- cbind(docvars_internal, docvars)
    } else {
        docvars <- docvars_internal
    }
    
    result <- unname(x)
    class(result) <- "corpus2"
    attr(result, "docvars") <- docvars
    attr(result, "created") <- date()
    attr(result, "source") <- c("directory" = getwd(), 
                                Sys.info()["machine"], 
                                Sys.info()["user"])
    return(result)
}

#' @rdname corpus2
#' @param docid_field optional column index of a document identifier; defaults
#'   to "doc_id", but if this is not found, then will use the rownames of the
#'   data.frame; if the rownames are not set, it will use the default sequence
#'   based on \code{(\link{quanteda_options}("base_docname")}.
#' @keywords corpus2
#' @method corpus2 data.frame
#' @export
corpus2.data.frame <- function(x, docid_field = "doc_id", text_field = "text",
                               ...) {
    
    unused_dots(...)
    # coerce data.frame variants to data.frame - for #1232
    x <- as.data.frame(x)
    
    text_index <- 0
    if (length(text_field) != 1)
        stop("text_field must refer to a single column")
    if (is.character(text_field)) {
        text_index <- match(text_field, names(x))
    } else {
        text_index <- match(text_field, seq(length(x)))
    }
    if (is.na(text_index))
        stop("text_field index refers to an invalid column")
    
    docid_index <- 0
    if (length(docid_field) != 1)
        stop("docid_field must refer to a single column")
    if (identical(docid_field, "row.names")) {
        docnames <- row.names(x)
    } else {
        if (is.character(docid_field)) {
            docid_index <- match(docid_field, names(x))
        } else {
            docid_index <- match(docid_field, seq(length(x)))
        }
        if (is.na(docid_index)) {
            if (identical(docid_field, "doc_id")) {
                docid_index <- 0
                docname <- paste0(quanteda_options("base_docname"), seq_len(nrow(x)))
            } else {
                stop("docid_field index refers to an invalid column")
            }
        } else {
            docname <- as.character(x[[docid_index]])
        }
    }

    docvars <- x[c(docid_index, text_index) * -1]

    # fix the variable names for missing or NA - for #1388
    # will not affect tibbles since these conditions can never exist for tibbles
    is_empty <- (!nzchar(names(docvars)) | is.na(names(docvars)))
    if (any(is_empty))
        names(docvars)[is_empty] <- paste0("V", seq(length(docvars))[is_empty])
    
    corpus2(x[[text_index]], docvars = docvars, docnames = docname)
}


#' @rdname corpus2
#' @param split_context logical; if \code{TRUE}, split each kwic row into two
#'   "documents", one for "pre" and one for "post", with this designation saved
#'   in a new docvar \code{context} and with the new number of documents
#'   therefore being twice the number of rows in the kwic.
#' @param extract_keyword logical; if  \code{TRUE}, save the keyword matching
#'   \code{pattern} as a new docvar \code{keyword}
#' @examples 
#' # from a kwic
#' kw <- kwic(data_char_sampletext, "econom*")
#' summary(corpus2(kw))
#' summary(corpus2(kw, split_context = FALSE))
#' texts(corpus2(kw, split_context = FALSE))
#' 
#' @export
corpus2.kwic <- function(x, split_context = TRUE, extract_keyword = TRUE, ...) {
    
    unused_dots(...)
    class(x) <- "data.frame"

    # convert docnames to a factor, as in original kwic
    x$docname <- factor(x$docname)
    
    if (split_context) {
        pre <- corpus2(x[,c("docname", "from", "to", "pre", "keyword")], text_field = "pre")
        docvars(pre, "context") <- "pre"
        docnames(pre) <- paste0(docnames(pre), ".pre")
        
        post <- corpus2(x[,c("docname", "from", "to", "post", "keyword")], text_field = "post")
        docvars(post, "context") <- "post"
        docnames(post) <- paste0(docnames(post), ".post")
        
        result <- pre + post
        if (!extract_keyword) docvars(result, "keyword") <- NULL
        
    } else {
        result <- corpus2(
            apply(x[, c("pre", "keyword", "post")], 1, paste, collapse = " "),
            docnames = paste0(x[["docname"]], ".L", x[["from"]])
        )
        if (extract_keyword) docvars(result, "keyword") <- x[["keyword"]]
    }
    
    # handle disassociated brackets, quotes, parens, etc
    regex <- "([\\b\\s][\\p{Pi}\\p{Ps}\"\'])\\s(\\S+)\\s([\\p{Pf}\\p{Pe}\"\'][\\b\\s])"
    texts(result) <- stri_replace_all_regex(texts(result), regex, "$1$2$3")
    
    # remove spaces before punctuation that should not have it
    texts(result) <- stri_replace_all_regex(texts(result), "\\s([!%*?;:,.]{1})", "$1")
    
    attr(result, "created") <- date()
    attr(result, "source") <- c("object" = "kwic", 
                                Sys.info()["machine"], 
                                Sys.info()["user"])
    
    return(result)
}
