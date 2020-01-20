# main tokens functions ----------

#' Construct a tokens object
#'
#' Construct a tokens object, either by importing a named list of characters
#' from an external tokenizer, or by calling the internal \pkg{quanteda}
#' tokenizer.
#' 
#' `tokens()` works on tokens class objects, which means that the removal rules
#' can be applied post-tokenization, although it should be noted that it will
#' not be possible to remove things that are not present.  For instance, if the
#' `tokens` object has already had punctuation removed, then `tokens(x,
#' remove_punct = TRUE)` will have no additional effect.
#' @rdname tokens
#' @param x a (uniquely) named list of characters or a [tokens] object; or a
#'   [corpus], or a character object that will be tokenized
#' @keywords tokens
#' @export
#' @param remove_punct logical; if `TRUE` remove all characters in the Unicode
#'   "Punctuation" `[P]` class, with exceptions for those used as prefixes for
#'   valid social media tags if `preserve_tags = TRUE`
#' @param remove_numbers logical; if `TRUE` remove tokens that consist only of
#'   numbers, but not words that start with digits, e.g. `2day`
#' @param remove_symbols logical; if `TRUE` remove all characters in the Unicode
#'   "Symbol" `[S]` class
#' @param remove_url logical; if `TRUE` find and eliminate URLs beginning with
#'   http(s) -- see section "Dealing with URLs".
#' @param preserve_tags keep (social media) tags intact, such as "#hashtags" and
#'   "#usernames" even when punctuation will be removed.  The rules defining a
#'   valid "tag" can be found
#'   [here](https://www.hashtags.org/featured/what-characters-can-a-hashtag-include/)
#'   for hashtags and
#'   [here](https://help.twitter.com/en/managing-your-account/twitter-username-rules)
#'   for usernames.
#' @param split_infix_hyphens logical; if `TRUE` split words that are connected by
#'   hyphenation and hyphenation-like characters in between words, e.g.
#'   `"self-aware"` becomes `c("self", "-", "aware")`.  Default is `FALSE` to
#'   preserve such words as is, with the hyphens.
#' @param verbose if `TRUE`, print timing messages to the console; off by
#'   default
#' @param include_docvars if `TRUE`, pass docvars through to the tokens object.
#'   Only applies when tokenizing [corpus] objects.
#' @param ... additional arguments not used
#' @details While \pkg{quanteda} has a its own tokenizer, users may prefer to
#'   use other tokenizers that return a named list as input.  This can be done
#'   by piping the output from these other tokenizers into the `tokens()`
#'   constructor, with additional removal options applied at the construction
#'   stage.  This will only have an effect, however, if the tokens exist for
#'   which removal is specified at in the `tokens()` call.  For instance, it is
#'   impossible to remove punctuation if the input list to `tokens()` already
#'   had its punctuation tokens removed at the external tokenization stage.
#'   
#'   Recommended tokenizers are those from the \pkg{tokenizers} package, which
#'   are generally faster than the default (built-in) tokenizer but always
#'   splits infix hyphens, or \pkg{spacyr}.
#' @return \pkg{quanteda} `tokens` class object, by default a serialized list of
#'   integers corresponding to a vector of types.
#' @seealso [tokens_ngrams()], [tokens_skipgrams()], [as.list.tokens()], [as.tokens()]
#' @keywords tokens
#' @examples
#' txt <- c(doc1 = "A sentence, showing how tokens() works.",
#'          doc2 = "@quantedainit and #textanalysis on Twitter.",
#'          doc3 = "Self-documenting code??",
#'          doc4 = "£1,000,000 for 50¢ is gr8 4ever \U0001f600")
#' tokens(txt)
#' tmp <- tokens(txt)
#' 
#' # removing punctuation marks and tags
#' tokens(txt[1:2], remove_punct = TRUE)
#' tokens(txt[1:2], remove_punct = TRUE, preserve_tags = TRUE)
#' 
#' # split hyphenated words
#' tokens(txt[3], split_infix_hyphens = TRUE)
#' 
#' # symbols and numbers
#' tokens(txt[4])
#' tokens(txt[4], remove_numbers = TRUE)
#' tokens(txt[4], remove_symbols = TRUE)
#' # note the difference
#' tokens(tokenize(txt[4]), remove_symbols = TRUE)
#' 
#' # using other tokenizers
#' tokenizers::tokenize_words(txt, lowercase = FALSE, strip_punct = FALSE) %>%
#'     tokens(remove_symbols = TRUE)
#' tokenizers::tokenize_characters(txt[3], strip_non_alphanum = FALSE) %>%
#'     tokens(remove_punct = TRUE)
#' tokenizers::tokenize_sentences(
#'     "The quick brown fox.  It jumped over the lazy dog.") %>%
#'     tokens()
#'
tokens <-  function(x,
                    remove_punct = FALSE,
                    remove_symbols = FALSE,
                    remove_numbers = FALSE,
                    remove_url = FALSE,
                    preserve_tags = !remove_punct,
                    split_infix_hyphens = FALSE,
                    include_docvars = TRUE,
                    verbose = quanteda_options("verbose"),
                    ...) {
    UseMethod("tokens")
}

#' @export
tokens.default <- function(x, ...) {
    stop(friendly_class_undefined_message(class(x), "tokens"))
}

#' @rdname tokens
#' @noRd
#' @export
tokens.list <- function(x, ...) {
    tokens(as.tokens(x), ...)
}

#' @rdname tokens
#' @noRd
#' @export
tokens.character <- function(x, ...) {
    tokens(corpus(x), ...)
}

#' @rdname tokens
#' @importFrom stringi stri_startswith_fixed
#' @export
#' @noRd
tokens.corpus <- function(x, ..., include_docvars = TRUE) {
    x <- as.corpus(x)
    attrs <- attributes(x)
    
    dots <- list(...)
    
    # look for deprecated remove arguments
    if ("remove_twitter" %in% names(dots)) {
        dots$preserve_tags <- !dots$remove_twitter
        .Deprecated(msg = paste0("'remove_twitter =", dots$remove_twitter, 
                                 "' is deprecated, use 'preserve_tags = ",
                                 dots$preserve_tags, "' instead."))
        dots$remove_twitter <- NULL
    }
    if ("remove_separators" %in% names(dots)) {
        warning(msg = "remove_separators is not used for character or corpus inputs")
        dots$remove_separators <- NULL
    }
    if ("remove_hyphens" %in% names(dots)) {
        dots$split_infix_hyphens <- dots$remove_hyphens
        .Deprecated(msg = "'remove_hyphens' argument is deprecated, use 'split_infix_hyphens' instead.")
        dots$remove_hyphens <- NULL
    }
    
    # look for "what" argument and handle appropriately
    what <- "word"
    if ("what" %in% names(dots)) {
        .Deprecated(msg = "'what' is deprecated, consider the tokenizers package instead")
        what <- match.arg(dots$what, 
                          c("word", "sentence", "character", "fastestword", "fasterword"))
        dots$what <- NULL
    }
    # call the appropriate tokenizer wrapper
    tokenizer_fn <- switch(what,
                           word = tokenize,
                           sentence = tokenize_sentence,
                           character = tokenize_character,
                           fasterword = tokenize_fasterword,
                           fastestword = tokenize_fastestword)
    result <- as.tokens(do.call(tokenizer_fn, c(list(x = x), dots)))
    
    # post process if legacy fasterword or fastestword
    if (stri_startswith_fixed(what, "fast") && stri_startswith_fixed(what, "sentence")) {
        result <- do.call(tokens, c(list(x = result), dots))
    }

    attributes(result, FALSE) <- attrs
    if (include_docvars) {
        attr(result, "docvars") <- get_docvars(x, user = TRUE, system = TRUE)
    } else {
        attr(result, "docvars") <- get_docvars(x, user = FALSE, system = TRUE)
    }
    attr(result, "unit") <- attr(x, "unit")
    attr(result, "meta") <- attr(x, "meta")
    return(result)
}

#' @rdname tokens
#' @export
#' @noRd
tokens.tokens <-  function(x,
                           remove_punct = FALSE,
                           remove_symbols = FALSE,
                           remove_numbers = FALSE,
                           remove_url = FALSE,
                           preserve_tags = !remove_punct,
                           split_infix_hyphens = FALSE,
                           include_docvars = TRUE,
                           verbose = quanteda_options("verbose"),
                           ...) {
    x <- as.tokens(x)
    
    check_dots(dots <- list(...), c(names(formals("tokens")),
                                    "remove_hyphens", "remove_twitter", "remove_separators"))
    if (any(c("remove_hyphens", "remove_twitter", "remove_separators") %in% names(dots))) {
        if ("remove_twitter" %in% names(dots)) {
            dots$preserve_tags <- !dots$remove_twitter
            .Deprecated(msg = paste0("'remove_twitter =", dots$remove_twitter, 
                                     "' is deprecated, use 'preserve_tags = ",
                                     dots$preserve_tags, "' instead."))
            dots$remove_twitter <- NULL
        }
        if ("remove_separators" %in% names(dots)) {
            warning(msg = "'remove_separators' argument is defunct")
            dots$remove_separators <- NULL
        }
        if ("remove_hyphens" %in% names(dots)) {
            dots$split_infix_hyphens <- dots$remove_hyphens
            .Deprecated(msg = "'remove_hyphens' argument is deprecated, use 'split_infix_hyphens' instead.")
            dots$remove_hyphens <- NULL
        }
        
        do.call(tokens, c(list(x = x), dots))
    }

    if (verbose) catm("Starting tokenization...\n")
    time_start <- proc.time()

    if (split_infix_hyphens) {
        if (verbose) catm("...separating hyphenated tokens")
        x <- tokens_split(x, separator = "\\p{Pd}", valuetype = "regex", 
                          remove_separator = FALSE)
        if (verbose) catm("\n")
    }

    if (!preserve_tags) {
        if (verbose) catm("...separating tag characters (#, @)")
        x <- tokens_split(x, separator = "^[#@]", valuetype = "regex", 
                          remove_separator = FALSE)
        if (verbose) catm("\n")
    }
    
    # removals
    regex_to_remove <- character()
    removing_msg <- "...removing "
    if (remove_punct) {
        regex_to_remove <- c(regex_to_remove, "^\\p{P}$")
        removing_msg <- c(removing_msg, "punctuation")
    }
    if (remove_symbols) {
        regex_to_remove <- c(regex_to_remove, "^\\p{S}$")
        removing_msg <- c(removing_msg, "symbols")
    }
    if (remove_numbers) {
        # includes currency amounts and those containing , or . digit separators
        regex_to_remove <- c(regex_to_remove, "^\\p{Sc}{0,1}(\\p{N}+[,.]{0,1}\\p{N}+)+\\p{Sc}{0,1}$")
        removing_msg <- c(removing_msg, "numbers")
    }
    if (remove_url) {
        regex_to_remove <- c(regex_to_remove, "^(https{0,1}|s{0,1}ftp)://")
        removing_msg <- c(removing_msg, "URLs")
    }
    if (length(regex_to_remove)) {
        if (verbose) catm(paste(removing_msg, collapse = ", "), "\n")
        x <- tokens_remove(x, paste(regex_to_remove, collapse = "|"),
                           valuetype = "regex",  padding = FALSE,
                           startpos = 1, endpos = -1)
    }

    if (!include_docvars)
        docvars(x) <- NULL

    if (verbose) {
        catm("...total elapsed: ",
             format((proc.time() - time_start)[3], digits = 3), "seconds.\n")
        catm("Finished (re-)constructing tokens from ", format(length(x), big.mark = ","), " text",
             if (length(x) > 1) "s", ".\n", sep = "")
    }
    return(x)
}

# coercion and checking functions -----------

#' Coercion, checking, and combining functions for tokens objects
#'
#' Coercion functions to and from [tokens] objects, checks for whether an
#' object is a [tokens] object, and functions to combine [tokens]
#' objects.
#' @param x object to be coerced or checked
#' @param concatenator character between multi-word expressions, default is the
#'   underscore character.  See Details.
#' @param ... additional arguments used by specific methods.  For
#'   [c.tokens], these are the [tokens] objects to be concatenated.
#' @return `as.tokens` returns a quanteda [tokens] object.
#' @details The `concatenator` is used to automatically generate dictionary
#'   values for multi-word expressions in [tokens_lookup()] and
#'   [dfm_lookup()]. The underscore character is commonly used to join
#'   elements of multi-word expressions (e.g. "piece_of_cake", "New_York"), but
#'   other characters (e.g. whitespace " " or a hyphen "-") can also be used.
#'   In those cases, users have to tell the system what is the concatenator in
#'   your tokens so that the conversion knows to treat this character as the
#'   inter-word delimiter, when reading in the elements that will become the
#'   tokens.
#' @export
#' @rdname as.tokens
#' @examples
#'
#' # create tokens object from list of characters with custom concatenator
#' dict <- dictionary(list(country = "United States",
#'                    sea = c("Atlantic Ocean", "Pacific Ocean")))
#' lis <- list(c("The", "United-States", "has", "the", "Atlantic-Ocean",
#'               "and", "the", "Pacific-Ocean", "."))
#' toks <- as.tokens(lis, concatenator = "-")
#' tokens_lookup(toks, dict)
#'
as.tokens <- function(x, concatenator = "_", ...) {
    UseMethod("as.tokens")
}

#' @export
as.tokens.default <- function(x, concatenator = "", ...) {
    stop(friendly_class_undefined_message(class(x), "as.tokens"))
}

#' @rdname as.tokens
#' @importFrom stringi stri_trans_nfc
#' @export
as.tokens.list <- function(x, concatenator = "_", ...) {
    x <- lapply(x, stri_trans_nfc)
    x <- serialize_tokens(x)
    docvar <- make_docvars(length(x), names(x))
    compile_tokens(x, docvar[["docname_"]],
                   concatenator = concatenator,
                   types = attr(x, "types"), source = "list",
                   docvars = docvar)
}

#' @rdname as.tokens
#' @export
as.tokens.tokens <- function(x, ...) {
    if (is_pre2(x)) {
        attr(x, "docvars") <- upgrade_docvars(attr(x, "docvars"), names(x))
        attr(x, "meta") <- meta_system_defaults("tokens")
    }
    return(x)
}

#' @rdname as.tokens
#' @param use_lemma logical; if `TRUE`, use the lemma rather than the raw
#'   token
#' @param include_pos character; whether and which part-of-speech tag to use:
#'   `"none"` do not use any part of speech indicator, `"pos"` use the
#'   `pos` variable, `"tag"` use the `tag` variable.  The POS
#'   will be added to the token after `"concatenator"`.
#' @export
as.tokens.spacyr_parsed <- function(x, concatenator = "/",
                                    include_pos = c("none", "pos", "tag"),
                                    use_lemma = FALSE, ...) {

    include_pos <- match.arg(include_pos)
    temp <- x[[if (use_lemma) "lemma" else "token"]]
    if (include_pos != "none")
        temp <- paste(temp, x[[include_pos]], sep = concatenator)
    as.tokens(base::split(temp, factor(x[["doc_id"]], levels = unique(x[["doc_id"]]))))
}

#' @rdname as.tokens
#' @export
#' @return `is.tokens` returns `TRUE` if the object is of class
#'   tokens, `FALSE` otherwise.
is.tokens <- function(x) {
    "tokens" %in% class(x)
}

# internal functions ------------

compile_tokens <- function(x, names, types, ngrams = 1, skip = 0,
                           what = "word", concatenator = "_", padding = FALSE,
                           unit = "documents", source = "corpus", 
                           docvars = data.frame(), meta = list()) {
    structure(x,
              names = names,
              class = "tokens",
              what = what,
              ngrams = ngrams,
              skip = skip,
              concatenator = concatenator,
              padding = padding,
              types = types,
              unit = unit,
              meta = list("system" = meta_system_defaults(source),
                          "user" = meta),
              docvars = docvars)
}

#' Function to serialized list-of-character tokens
#'
#' Creates a serialized object of tokens, called by [tokens()].
#' @param x a list of character vectors
#' @param types_reserved optional pre-existing types for mapping of tokens
#' @param ... additional arguments
#' @return a list the serialized tokens found in each text
#' @importFrom fastmatch fmatch
#' @importFrom stringi stri_detect_regex
#' @keywords internal tokens
serialize_tokens <- function(x, types_reserved = NULL, ...) {

    attrs <- attributes(x)
    types <- unique(unlist(x, use.names = FALSE))
    # remove empty types and control chracters
    types <- types[nzchar(types) & !stri_detect_regex(types, "^[\\p{Cf}]+$")]
    types <- union(types_reserved, types) # prepend new types

    x <- lapply(x, function(x) {
        id <- fastmatch::fmatch(x, types)
        is_na <- is.na(id)
        if (length(is_na) > 0) {
            id[!is_na]
        } else {
            integer()
        }
    })

    attributes(x) <- attrs
    attr(x, "types") <- types
    class(x) <- "tokens"
    return(x)
}

#' recompile a serialized tokens object
#'
#' This function recompiles a serialized tokens object when the vocabulary has
#' been changed in a way that makes some of its types identical, such as
#' lowercasing when a lowercased version of the type already exists in the type
#' table, or introduces gaps in the integer map of the types.  It also
#' re-indexes the types attribute to account for types that may have become
#' duplicates, through a procedure such as stemming or lowercasing; or the
#' addition of new tokens through compounding.
#' @param x the [tokens] object to be recompiled
#' @param gap if `TRUE`, remove gaps between token IDs
#' @param dup if `TRUE`, merge duplicated token types into the same ID
#' @param method `"C++"` for C++ implementation or `"R"` for an older
#'   R-based method
#' @examples
#' # lowercasing
#' toks1 <- tokens(c(one = "a b c d A B C D",
#'                  two = "A B C d"))
#' attr(toks1, "types") <- char_tolower(attr(toks1, "types"))
#' unclass(toks1)
#' unclass(quanteda:::tokens_recompile(toks1))
#'
#' # stemming
#' toks2 <- tokens("Stemming stemmed many word stems.")
#' unclass(toks2)
#' unclass(quanteda:::tokens_recompile(tokens_wordstem(toks2)))
#'
#' # compounding
#' toks3 <- tokens("One two three four.")
#' unclass(toks3)
#' unclass(tokens_compound(toks3, "two three"))
#'
#' # lookup
#' dict <- dictionary(list(test = c("one", "three")))
#' unclass(tokens_lookup(toks3, dict))
#'
#' # empty pads
#' unclass(tokens_select(toks3, dict))
#' unclass(tokens_select(toks3, dict, pad = TRUE))
#'
#' # ngrams
#' unclass(tokens_ngrams(toks3, n = 2:3))
#'
#' @keywords internal tokens
tokens_recompile <- function(x, method = c("C++", "R"), gap = TRUE, dup = TRUE) {

    method <- match.arg(method)
    attrs <- attributes(x)

    if (method == "C++") {
        x <- qatd_cpp_tokens_recompile(x, types(x), gap, dup)
        attributes(x, FALSE) <- attrs
        return(x)
    }

    # Check for padding
    index_unique <- unique(unlist(unclass(x), use.names = FALSE))
    padding <- (index_unique == 0)
    attrs$padding <- any(padding) # add padding flag
    index_unique <- index_unique[!padding] # exclude padding

    if (!gap && !dup) {
        attributes(x) <- attrs
        return(x)
    }

    # Remove gaps in the type index, if any, remap index
    if (gap) {
        if (any(is.na(match(seq_len(length(types(x))), index_unique)))) {
            types_new <- types(x)[index_unique]
            index_new <- c(0, seq_along(index_unique)) # padding index is zero but not in types
            index_unique <- c(0, index_unique) # padding index is zero but not in types
            x <- lapply(unclass(x), function(y) index_new[fastmatch::fmatch(y, index_unique)])
            attributes(x) <- attrs
            types(x) <- types_new
        }
    }

    # Reindex duplicates, if any
    if (dup) {
        if (any(duplicated(types(x)))) {
            types <- types(x)
            types_unique <- unique(types)
            index_mapping <- match(types, types_unique)
            index_mapping <- c(0, index_mapping) # padding index is zero but not in types
            x <- lapply(unclass(x), function(y) index_mapping[y + 1]) # shift index for padding
            attributes(x) <- attrs
            types(x) <- types_unique
        }
    }
    Encoding(types(x)) <- "UTF-8"
    return(x)
}

#' Get word types from a tokens object
#'
#' Get unique types of tokens from a [tokens] object.
#' @param x a tokens object
#' @export
#' @seealso [featnames]
#' @examples
#' toks <- tokens(data_corpus_inaugural)
#' types(toks)
types <- function(x) {
    UseMethod("types")
}

#' @export
types.default <- function(x) {
    stop(friendly_class_undefined_message(class(x), "types"))
}

#' @export
types.tokens <- function(x) {
    attr(x, "types")
}

"types<-" <- function(x, value) {
    UseMethod("types<-")
}

"types<-.tokens" <- function(x, value) {
    if (!is.character(value))
        stop("replacement value must be character")
    attr(x, "types") <- value
    return(x)
}
