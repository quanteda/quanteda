# internal-only functions for handling tm corpora -------

names_tmCorpus <- function(x) 
    unlist(meta(x, "id", "local"), use.names = FALSE)

meta <- function(x, tag = NULL, type = c("indexed", "corpus", "local"), ...) {
    if (!is.null(tag) && missing(type)) {
        type <- if (tag %in% colnames(x$dmeta)) "indexed"
        else if (tag %in% names(x$meta)) "corpus"
        else "local"
    }
    type <- match.arg(type)
    if (identical(type, "indexed"))
        if (is.null(tag)) x$dmeta else x$dmeta[tag]
    else if (identical(type, "corpus"))
        if (is.null(tag)) x$meta else x$meta[[tag]]
    else if (identical(type, "local"))
        lapply(x, meta, tag)
    else
        stop("invalid type")
}

make_unique_tm_names <- function(char_names, int_lengths, start_at = 1, sep = ".") {
    # expand the char_names
    char_names <- rep(char_names, int_lengths)
    int_seqs <- unlist(lapply(int_lengths, seq_len), use.names = FALSE)
    int_seqs <- int_seqs - (1 - start_at)
    paste(char_names, int_seqs, sep = sep)
}

flatten_lists <- function(x) {
    x <- lapply(x, unclass)
    for (i in seq_along(x)) {
        x[[i]] <- lapply(x[[i]], function(y) {
            if (lubridate::is.POSIXlt(y)) y <- as.character(y)
            if (length(y) > 1) y <- paste(y, collapse = " ")
            if (length(y) == 0) y <- NULL
            y
        })
    }
    x
}
