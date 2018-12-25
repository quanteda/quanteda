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
#' \item a \pkg{tm} \link[tm]{VCorpus} or \link[tm]{SimpleCorpus} class object.
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
#' \itemize{
#' \item{\code{source }}{a description of the source of the texts, used for
#'   referencing;}
#' \item{\code{citation }}{information on how to cite the corpus; and}
#' \item{\code{notes }}{any additional information about who created the text, warnings,
#'   to do lists, etc.}
#' }
#' @param compress logical; if \code{TRUE}, compress the texts in memory using
#'   gzip compression. This significantly reduces the size of the corpus in
#'   memory, but will slow down operations that require the texts to be
#'   extracted.
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
#' @seealso \link{corpus-class}, \code{\link{docvars}}, 
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
#' @keywords corpus
#' @examples
#' # create a corpus from texts
#' corpus(data_char_ukimmig2010)
#'
#' # create a corpus from texts and assign meta-data and document variables
#' summary(corpus(data_char_ukimmig2010,
#'                docvars = data.frame(party = names(data_char_ukimmig2010))), 5)
#'
#' corpus(texts(data_corpus_irishbudget2010))
#'
#' # import a tm VCorpus
#' if (requireNamespace("tm", quietly = TRUE)) {
#'     data(crude, package = "tm")    # load in a tm example VCorpus
#'     vcorp <- corpus(crude)
#'     summary(vcorp)
#'
#'     data(acq, package = "tm")
#'     summary(corpus(acq), 5)
#'
#'     vcorp2 <- tm::VCorpus(tm::VectorSource(data_char_ukimmig2010))
#'     corp <- corpus(vcorp2)
#'     summary(corp)
#' }
#'
#' # construct a corpus from a data.frame
#' df <- data.frame(letter_factor = factor(rep(letters[1:3], each = 2)),
#'                  some_ints = 1L:6L,
#'                  some_text = paste0("This is text number ", 1:6, "."),
#'                  stringsAsFactors = FALSE,
#'                  row.names = paste0("fromDf_", 1:6))
#' df
#' summary(corpus(df, text_field = "some_text"))
#'
#' # construct a corpus from a kwic object
#' kw <- kwic(data_corpus_inaugural, "southern")
#' summary(corpus(kw))
corpus <- function(x, ...) {
    UseMethod("corpus")
}

#' @rdname corpus
#' @noRd
#' @export
corpus.default <- function(x, ...) {
    stop(friendly_class_undefined_message(class(x), "corpus"))
}

#' @rdname corpus
#' @export
corpus.corpus <- function(x, docnames = NULL, docvars = NULL, ...) {
    x <- as.corpus(x)
    if (is.null(docnames) && is.null(docvars))
        return(x)
    corpus(texts(x), docnames, docvars)
}

#' @rdname corpus
#' @export
corpus.character <- function(x, docnames = NULL, 
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
    
    if (!is.null(docvars) && nrow(docvars) > 0) {
        if (any(is_system(names(docvars))))
            message_error("docvar_invalid")
        docvars <- cbind(make_docvars(length(x), docname), docvars)
    } else {
        docvars <- make_docvars(length(x), docname)
    }
    
    result <- unname(x)
    class(result) <- "corpus"
    attr(result, "docvars") <- docvars
    attr(result, "unit") <- "documents"
    attr(result, "meta") <- meta("character")
    
    return(result)
}

#' @rdname corpus
#' @param docid_field optional column index of a document identifier; defaults
#'   to "doc_id", but if this is not found, then will use the rownames of the
#'   data.frame; if the rownames are not set, it will use the default sequence
#'   based on \code{(\link{quanteda_options}("base_docname")}.
#' @keywords corpus
#' @method corpus data.frame
#' @export
corpus.data.frame <- function(x, docid_field = "doc_id", text_field = "text", ...) {
    
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
        stop("text_field column not found or invalid")
    if (!is.character(x[[text_index]]))
        stop("text_field must refer to a character mode column")
    
    docid_index <- 0
    if (length(docid_field) != 1)
        stop("docid_field must refer to a single column")
    if (identical(docid_field, "row.names")) {
        docname <- row.names(x)
    } else {
        if (is.character(docid_field)) {
            docid_index <- match(docid_field, names(x))
        } else {
            docid_index <- match(docid_field, seq(length(x)))
        }
        if (is.na(docid_index)) {
            if (identical(docid_field, "doc_id")) {
                if (is.character(attr(x, "row.names"))) { 
                    docname <- row.names(x)
                } else {
                    docname <- paste0(quanteda_options("base_docname"), seq_len(nrow(x)))
                }
                docid_index <- 0
            } else {
                stop("docid_field column not found or invalid")
            }
        } else {
            docname <- as.character(x[[docid_index]])
        }
    }
    
    # detect missing or NA in names - for #1388
    is_empty <- (!nzchar(names(x)) | is.na(names(x)))
    docvars <- x[c(docid_index, text_index) * -1]
    row.names(docvars) <- NULL
    
    is_empty <- is_empty[c(docid_index, text_index) * -1]
    if (any(is_empty))
        names(docvars)[is_empty] <- paste0("V", seq(length(docvars))[is_empty])
    
    corpus(x[[text_index]], docvars = docvars, docnames = docname)
}


#' @rdname corpus
#' @param split_context logical; if \code{TRUE}, split each kwic row into two
#'   "documents", one for "pre" and one for "post", with this designation saved
#'   in a new docvar \code{context} and with the new number of documents
#'   therefore being twice the number of rows in the kwic.
#' @param extract_keyword logical; if  \code{TRUE}, save the keyword matching
#'   \code{pattern} as a new docvar \code{keyword}
#' @examples 
#' # from a kwic
#' kw <- kwic(data_char_sampletext, "econom*", separator = "",
#'            remove_separators = FALSE) # keep original separators
#' summary(corpus(kw))
#' summary(corpus(kw, split_context = FALSE))
#' texts(corpus(kw, split_context = FALSE))
#' 
#' @export
corpus.kwic <- function(x, split_context = TRUE, extract_keyword = TRUE, ...) {
    
    unused_dots(...)
    class(x) <- "data.frame"
    
    if (split_context) {
        pre <- corpus(x[,c("docname", "from", "to", "pre", "keyword")], 
                      docid_field = "docname", text_field = "pre")
        docvars(pre, "context") <- "pre"
        docnames(pre) <- paste0(docnames(pre), ".pre")
        
        post <- corpus(x[,c("docname", "from", "to", "post", "keyword")], 
                       docid_field = "docname", text_field = "post")
        docvars(post, "context") <- "post"
        docnames(post) <- paste0(docnames(post), ".post")
        result <- pre + post
        if (!extract_keyword) docvars(result, "keyword") <- NULL
        
    } else {
        result <- corpus(paste0(x[["pre"]], x[["keyword"]], x[["post"]]),
                         docnames = x[["docname"]])
        docnames(result) <- paste0(x[["docname"]], ".L", x[["from"]])
        if (extract_keyword) docvars(result, "keyword") <- x[["keyword"]]
    }
    
    attr(result, "meta") <- meta("kwic")
    return(result)
}

#' @rdname corpus
#' @export
corpus.Corpus <- function(x, ...) {
    
    unused_dots(...)
    
    if (inherits(x, what = "VCorpus")) {
        x <- unclass(x)
        docs <- x$content
        
        txt <- character()
        docvars <- data.frame()
        for (i in seq_along(docs)) {
            doc <- docs[[i]]
            txt <- c(txt, doc$content)
            l <- lengths(doc$meta)
            meta <- rep(list(NA), length(l))
            names(meta) <- names(doc$meta)
            meta[l == 1] <- doc$meta[l == 1]
            meta[l > 1] <- lapply(doc$meta[l > 1], paste, collapse = " ")
            docvars <- rbind_fill(docvars, as.data.frame(meta, stringsAsFactors = FALSE))
        }
        names(txt) <- names(docs)
        
    } else if (inherits(x, what = "SimpleCorpus")) {
        x <- unclass(x)
        txt <- x$content
        docvars <- x$dmeta
    } else {
        stop("Cannot construct a corpus from this tm ", class(x)[1], " object")
    }
    corpus(txt, docvars = docvars)
}

# internal function to create corpus meta data
meta <- function(source = c("character", "kwic")) {
    source <- match.arg(source)
    list("source" = source,
         "package-version" = packageVersion("quanteda"),
         "r-version" = getRversion(),
         "system" = Sys.info()[c("sysname", "machine", "user")],
         "directory" = getwd(),
         "created" = Sys.time()
    )
}

# internal function to rbind data.frames that have different columns
rbind_fill <- function(x, y) {
    name1 <- names(x)
    name2 <- names(y)
    if (!identical(name1, name2)) {
        name <- union(name1, name2)
        name1_missing <- setdiff(name, name1)
        if (length(name1_missing)) {
            fill1 <- rep(list(rep(NA, nrow(x))), length(name1_missing))
            names(fill1) <- name1_missing
            x <- cbind(x, fill1)
        }
        
        name2_missing <- setdiff(name, name2)
        if (length(name2_missing)) {
            fill2 <- rep(list(rep(NA, nrow(y))), length(name2_missing))
            names(fill2) <- name2_missing
            y <- cbind(y, fill2)
        }
    }
    return(rbind(x, y))
}
