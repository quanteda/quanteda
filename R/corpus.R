#' Construct a corpus object
#'
#' Creates a corpus object from available sources.  The currently available
#' sources are:
#' \itemize{
#' \item a [character] vector, consisting of one document per
#' element; if the elements are named, these names will be used as document
#' names.
#' \item a [data.frame] (or a \pkg{tibble} `tbl_df`), whose default
#' document id is a variable identified by `docid_field`; the text of the
#' document is a variable identified by `text_field`; and other variables
#' are imported as document-level meta-data.  This matches the format of
#' data.frames constructed by the the \pkg{readtext} package.
#' \item a [kwic] object constructed by [kwic()].
#' \item a \pkg{tm} [VCorpus][tm::VCorpus] or [SimpleCorpus][tm::SimpleCorpus] class  object,
#'   with the fixed metadata
#'   fields imported as [docvars] and corpus-level metadata imported
#'   as [metacorpus] information.
#' \item a [corpus] object.
#' }
#' @param x a valid corpus source object
#' @param docnames Names to be assigned to the texts.  Defaults to the names of
#'   the character vector (if any); `doc_id` for a data.frame; the document
#'   names in a \pkg{tm} corpus; or a vector of user-supplied labels equal in
#'   length to the number of documents.  If none of these are round, then
#'   "text1", "text2", etc. are assigned automatically.
#' @param docvars a data.frame of document-level variables associated with each
#'   text
#' @param unique_docnames logical; if `TRUE`, enforce strict uniqueness in
#'   `docnames`; otherwise, rename duplicated docnames using an added serial
#'   number, and treat them as segments of the same document.
#' @param text_field the character name or numeric index of the source
#'   `data.frame` indicating the variable to be read in as text, which must
#'   be a character vector. All other variables in the data.frame will be
#'   imported as docvars.  This argument is only used for `data.frame`
#'   objects (including those created by \pkg{readtext}).
#' @param meta a named list that will be added to the corpus as corpus-level,
#'   user meta-data.  This can later be accessed or updated using
#'   [meta()].
#' @param ... not used directly
#' @return A [corpus-class] class object containing the original texts,
#'   document-level variables, document-level metadata, corpus-level metadata,
#'   and default settings for subsequent processing of the corpus.
#'
#'   For \pkg{quanteda} >= 2.0, this is a specially classed character vector. It
#'   has many additional attributes but **you should not access these
#'   attributes directly**, especially if you are another package author. Use the
#'   extractor and replacement functions instead, or else your code is not only
#'   going to be uglier, but also likely to break should the internal structure
#'   of a corpus object change.  Using the accessor and replacement functions
#'   ensures that future code to manipulate corpus objects will continue to work.
#' @seealso [corpus-class], [docvars()],
#'   [meta()], [texts()], [ndoc()],
#'   [docnames()]
#' @details The texts and document variables of corpus objects can also be
#'   accessed using index notation and the `$` operator for accessing or assigning
#'   docvars.  For details, see [`[.corpus()`][corpus-class].
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
#' dat <- data.frame(letter_factor = factor(rep(letters[1:3], each = 2)),
#'                   some_ints = 1L:6L,
#'                   some_text = paste0("This is text number ", 1:6, "."),
#'                   stringsAsFactors = FALSE,
#'                   row.names = paste0("fromDf_", 1:6))
#' dat
#' summary(corpus(dat, text_field = "some_text",
#'                meta = list(source = "From a data.frame called mydf.")))
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
corpus.corpus <- function(x, docnames = quanteda::docnames(x),
                          docvars = quanteda::docvars(x),
                          meta = quanteda::meta(x), ...) {
    x <- as.corpus(x)
    result <- corpus(texts(x), docnames = docnames, docvars = docvars, meta = meta, ...)
    meta_system(result, "source") <- "corpus"
    return(result)
}

#' @rdname corpus
#' @export
corpus.character <- function(x, docnames = NULL, docvars = NULL,
                             meta = list(), unique_docnames = TRUE, ...) {

    unused_dots(...)
    is_na <- is.na(x)
    if (any(is.na(x))) {
        warning("NA is replaced by empty string", call. = FALSE)
        x[is_na] <- ""
    }
    
    if (!is.null(docnames)) {
        if (length(docnames) != length(x))
            stop(message_error("docnames_mismatch"))
    } else if (!is.null(names(x))) {
        docnames <- names(x)
    }
    if (any(duplicated(docnames)) && unique_docnames)
        stop("docnames must be unique")
    if (!is.null(docvars) && nrow(docvars) > 0) {
        row.names(docvars) <- NULL
        if (any(is_system(names(docvars))))
            stop(message_error("docvars_invalid"))
        docvars <- cbind(make_docvars(length(x), docnames), docvars)
    } else {
        docvars <- make_docvars(length(x), docnames)
    }

    # normalize Unicode
    x <- stri_trans_nfc(x)

    # normalize EOL
    x <- stri_replace_all_fixed(x, "\r\n", "\n") # Windows
    x <- stri_replace_all_fixed(x, "\r", "\n") # Old Macintosh

    if (any(duplicated(docvars[["docid_"]]))) {
        unit <- "segments"
    } else {
        unit <- "documents"
    }
    build_corpus(
        x,
        unit = unit,
        docvars = docvars,
        meta = list("user" = meta)
    )
}

#' @rdname corpus
#' @param docid_field optional column index of a document identifier; defaults
#'   to "doc_id", but if this is not found, then will use the rownames of the
#'   data.frame; if the rownames are not set, it will use the default sequence
#'   based on `([quanteda_options]("base_docname")`.
#' @keywords corpus
#' @method corpus data.frame
#' @export
corpus.data.frame <- function(x, docid_field = "doc_id", text_field = "text",
                              meta = list(), unique_docnames = TRUE, ...) {

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
        docname <- rownames(x)
    } else {
        if (is.character(docid_field)) {
            docid_index <- match(docid_field, names(x))
        } else {
            docid_index <- match(docid_field, seq(length(x)))
        }
        if (is.na(docid_index)) {
            if (identical(docid_field, "doc_id")) {
                if (is.character(attr(x, "row.names"))) {
                    docname <- rownames(x)
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
    rownames(docvars) <- NULL

    is_empty <- is_empty[c(docid_index, text_index) * -1]
    if (any(is_empty))
        names(docvars)[is_empty] <- paste0("V", seq(length(docvars))[is_empty])
    result <- corpus(x[[text_index]], docvars = docvars, docnames = docname,
                     meta = meta, unique_docnames = unique_docnames)
    meta_system(result, "source") <- "data.frame"
    return(result)
}


#' @rdname corpus
#' @param split_context logical; if `TRUE`, split each kwic row into two
#'   "documents", one for "pre" and one for "post", with this designation saved
#'   in a new docvar `context` and with the new number of documents
#'   therefore being twice the number of rows in the kwic.
#' @param extract_keyword logical; if  `TRUE`, save the keyword matching
#'   `pattern` as a new docvar `keyword`
#' @examples
#' # from a kwic
#' kw <- kwic(data_char_sampletext, "econom*", separator = "",
#'            remove_separators = FALSE) # keep original separators
#' summary(corpus(kw))
#' summary(corpus(kw, split_context = FALSE))
#' texts(corpus(kw, split_context = FALSE))
#'
#' @export
corpus.kwic <- function(x, split_context = TRUE, extract_keyword = TRUE, meta = list(), ...) {

    unused_dots(...)
    class(x) <- "data.frame"

    if (split_context) {
        pre <- corpus(x[, c("docname", "from", "to", "pre", "keyword")],
                      docid_field = "docname", text_field = "pre", meta = meta, unique_docnames = FALSE)
        docvars(pre, "context") <- "pre"
        docnames(pre) <- paste0(docnames(pre), ".pre")

        post <- corpus(x[, c("docname", "from", "to", "post", "keyword")],
                       docid_field = "docname", text_field = "post",
                       meta = meta, unique_docnames = FALSE)
        docvars(post, "context") <- "post"
        docnames(post) <- paste0(docnames(post), ".post")
        result <- pre + post
        if (!extract_keyword) docvars(result, "keyword") <- NULL
    } else {
        result <- corpus(paste0(x[["pre"]], x[["keyword"]], x[["post"]]),
                         docnames = x[["docname"]], meta = meta, unique_docnames = FALSE)
        docnames(result) <- paste0(x[["docname"]], ".L", x[["from"]])
        if (extract_keyword) docvars(result, "keyword") <- x[["keyword"]]
    }

    meta_system(result, "source") <- "kwic"
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
            txt <- c(txt, paste(doc$content, collapse = "\n\n"))
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
    result <- corpus(txt, docvars = docvars, meta = unclass(unclass(x)$meta))
    meta_system(result, "source") <- "tm"
    return(result)
}
