tokens_internal_old <- function(x, what = c("word", "sentence", "character", "fastestword", "fasterword"),
                            remove_numbers = FALSE,
                            remove_punct = FALSE,
                            remove_symbols = FALSE,
                            remove_separators = TRUE,
                            remove_twitter = FALSE,
                            remove_hyphens = FALSE,
                            remove_url = FALSE,
                            ngrams = 1L,
                            skip = 0L,
                            concatenator = "_",
                            hash = TRUE,
                            verbose = getOption("verbose"),  
                            include_docvars = TRUE, 
                            ...) {
    
    # trap older arguments, issue a warning, and call with correct arguments
    thecall <- as.list(match.call())[-1]
    oldargindex <- 
        stri_detect_regex(names(thecall), 
                          "remove(Numbers|Punct|Symbols|Separators|Twitter|Hyphens|URL)$")
    if (any(oldargindex)) {
        warning(names(thecall)[oldargindex], " is deprecated; use ",
                tolower(gsub("([A-Z]+)", "_\\1", names(thecall)[oldargindex])), " instead", call. = FALSE)
        names(thecall)[oldargindex] <- tolower(gsub("([A-Z]+)", "_\\1", names(thecall)[oldargindex]))
        return(do.call(tokens, thecall))
    }
    
    what <- match.arg(what)
    names_org <- names(x)
    attrs_org <- attributes(x)
    
    # disable remove_twitter if remove_punct = FALSE
    if (!remove_punct & remove_twitter) {
        remove_twitter <- FALSE
        warning("remove_twitter reset to FALSE when remove_punct = FALSE")
    }
    
    # warn about unused arguments
    if (length(added_args <- list(...)) & 
        !all(names(added_args) %in% paste0("remove", c("Numbers", "Punct", "Symbols", "Separators", "Twitter", "Hyphens", "URL", "simplify")))) {
        warning("Argument", if (length(added_args) > 1L) "s " else " ", names(added_args), " not used.", sep = "")
    }
    
    # deprecate "simplify"
    if ("simplify" %in% names(added_args)) warning("simplify no longer available")
    
    if (!is.integer(ngrams)) ngrams <- as.integer(ngrams)
    
    if (verbose) catm("Starting tokenization...\n")
    
    time_start <- proc.time()
    
    # Split x into smaller blocks to reducre peak memory consumption
    x <- split(x, ceiling(seq_along(x) / 10000))
    for (i in seq_along(x)) {
        
        if (verbose) catm("...tokenizing", i, "of" , length(x), "blocks\n")
        if (what %in% c("word", "fastestword", "fasterword")) {
            temp <- tokens_word_old(x[[i]], what, remove_numbers, remove_punct, remove_symbols, 
                                remove_separators, remove_twitter, remove_hyphens, remove_url, verbose)
        } else if (what == "character") {
            temp <- tokens_character(x[[i]],remove_punct, remove_symbols, remove_separators, verbose)
        } else if (what == "sentence") {
            temp <- tokens_sentence(x[[i]], verbose)
        } else {
            stop(what, " not implemented in tokens().")
        }
        
        # Hash the tokens
        if (hash == TRUE) {
            if (verbose) catm("...serializing tokens ")
            if (i == 1) {
                x[[i]] <- tokens_hash(temp)
            } else {
                x[[i]] <- tokens_hash(temp, attr(x[[i - 1]], 'types'))
            }
            if (verbose) catm(length(attr(x[[i]], 'types')), 'unique types\n')
        } else {
            x[[i]] <- temp
        }
    }
    
    # Put all the blocked results togather
    result <- unlist(x, recursive = FALSE)
    
    if (hash == TRUE){
        class(result) <- c("tokens", "tokenizedTexts")
        types(result) <- attr(x[[length(x)]], 'types') # last block has all the types
    } else {
        class(result) <- c("tokenizedTexts", "list")
    }  
    if (!identical(ngrams, 1L)) {
        if (verbose) catm("...creating ngrams\n")
        result <- tokens_ngrams(result, n = ngrams, skip = skip, concatenator = concatenator)
    }
    if (verbose){
        catm("...total elapsed: ", (proc.time() - time_start)[3], "seconds.\n")
        catm("Finished tokenizing and cleaning", format(length(result), big.mark=","), "texts.\n")
    }
    
    names(result) <- names_org
    attr(result, "what") <- what
    attr(result, "ngrams") <- ngrams
    attr(result, "skip") <- skip
    attr(result, "concatenator") <- concatenator
    attr(result, 'padding') <- FALSE
    
    # issue #607: remove @ # only if not part of Twitter names
    if (remove_punct & !remove_twitter) {
        result <- tokens_remove(result, "^#+$|^@+$", valuetype = "regex")
    }
    
    return(result)
}

tokens_word_old <- function(txt, 
                        what = 'word', 
                        remove_numbers = FALSE, 
                        remove_punct = FALSE, 
                        remove_symbols = FALSE, 
                        remove_separators = TRUE, 
                        remove_twitter = FALSE, 
                        remove_hyphens = FALSE, 
                        remove_url = FALSE, 
                        verbose = FALSE){
    
    # to preserve intra-word hyphens, replace with _hy_
    if (!remove_hyphens & remove_punct)
        txt <- stri_replace_all_regex(txt, "(\\b)[\\p{Pd}](\\b)", "$1_hy_$2")
    else if (remove_hyphens)
        txt <- stri_replace_all_regex(txt, "(\\b)[\\p{Pd}](\\b)", "$1 $2")
    
    if (remove_url) {
        if (verbose & remove_url) catm(", removing URLs")
        URLREGEX <- "https?:\\/\\/(www\\.)?[-a-zA-Z0-9@:%._\\+~#=]{1,256}\\.[a-z]{2,4}\\b([-a-zA-Z0-9@:%_\\+.~#?&//=]*)"
        txt <- stri_replace_all_regex(txt, URLREGEX, "")
    }
    
    if (what %in% c("fasterword", "fastestword")) {
        
        regexToEliminate <- paste(if (remove_numbers) "\\b\\d+\\b" else "",
                                  if (remove_punct) paste0("(?![", if (remove_twitter) "_" else "@#_",  "])[\\p{P}]") else "",
                                  if (remove_symbols) "[\\p{S}]" else "",
                                  sep = "|")
        
        # catm("\n..", regexToEliminate, "..\n", sep = "")
        regexToEliminate <- stri_replace_all_regex(regexToEliminate, "^\\|+", "")
        regexToEliminate <- stri_replace_all_regex(regexToEliminate, "\\|+$", "")
        # catm("\n..", regexToEliminate, "..\n", sep = "")
        if (stri_replace_all_fixed(regexToEliminate, "|", "") != "")
            txt <- stri_replace_all_regex(txt, regexToEliminate, "")
        
        if (verbose & remove_punct==TRUE) catm(", ", what, " tokenizing", sep="")
        if (what=="fastestword")
            tok <- stri_split_fixed(txt, " ")
        else if (what=="fasterword")
            tok <- stri_split_charclass(txt, "\\p{WHITE_SPACE}")
        
    } else {
        
        if (remove_twitter == FALSE) {
            if (verbose) catm("...preserving Twitter characters (#, @)\n")
            txt <- stri_replace_all_fixed(txt, c("#", "@"), c("_ht_", "_as_"), vectorize_all = FALSE)
        }
        
        tok <- stri_split_boundaries(txt, 
                                     type = "word", 
                                     # this is what obliterates currency symbols, Twitter tags, and URLs
                                     skip_word_none = (remove_punct | remove_symbols) & remove_separators, 
                                     # but does not remove 4u, 2day, etc.
                                     skip_word_number = remove_numbers) 
        
        # Remove separators (including control characters) if option is TRUE
        if (remove_separators & !remove_punct) {
            tok <- lapply(tok, function(x) x[!stri_detect_charclass(x, "[\\p{Z}\\p{C}]")])
        }
        
        if (remove_punct & !remove_separators) {
            tok <- lapply(tok, function(x) x[!stri_detect_charclass(x, "[\\p{P}]")])
        }
        
        if (remove_twitter == FALSE) {
            if (verbose) catm("...replacing Twitter characters (#, @)\n")
            tok <- lapply(tok, stri_replace_all_fixed, c("_ht_", "_as_"), c("#", "@"), vectorize_all = FALSE)
        }
        
    }
    
    # Put hyphens back the fast way
    if (!remove_hyphens & remove_punct)
        tok <- lapply(tok, stri_replace_all_fixed, "_hy_", "-")
    
    tok <- qatd_cpp_chars_remove(tok, "")
    return(tok)
}