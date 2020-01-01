printcorp <- function(x, ndoc = 6, nchar = 60) {
    if (ndoc < 0) stop("ndoc must be >= 0")
    if (nchar < 0) stop("nchar must be >= 0")
    ndoc <- round(ndoc)
    nchar <- round(nchar)
    
    print(x)
    
    if (ndoc > 0) {
        docnames_to_print <- docnames(x)[seq_len(ndoc)]
        max_docname_width <- max(nchar(docnames_to_print))
        for (docname in docnames_to_print) {
            cat(paste0(stringi::stri_pad_left(paste0("[", docname, "] "), 
                                               width = max_docname_width + 4),
                       stringi::stri_replace_all_regex(stringi::stri_sub(texts(x[docname]), from = 1, to = nchar),
                                                       "\\n", " "),
                       " ...\n"))
        }
    }
    
    remainder_docs <- ndoc(x) - ndoc
    if (ndoc > 0 && ndoc < ndoc(x))
        cat("and ", remainder_docs, " more document", 
              if (remainder_docs != 1) "s", ".", sep = "")
}
