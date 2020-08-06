# tokens() ----------

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
#' @param x the input object to the tokens constructor, one of:
#'   a (uniquely) named **list** of characters; a [tokens] object; or a
#'   [corpus] or [character] object that will be tokenized
#' @param what character; which tokenizer to use.  The default `what = "word"`
#'   is the version 2 \pkg{quanteda} tokenizer.  Legacy tokenizers (version < 2)
#'   are also supported, including the default `what = "word1"`.
#'   See the Details and quanteda Tokenizers below.
#' @param remove_punct logical; if `TRUE` remove all characters in the Unicode
#'   "Punctuation" `[P]` class, with exceptions for those used as prefixes for
#'   valid social media tags if `preserve_tags = TRUE`
#' @param remove_symbols logical; if `TRUE` remove all characters in the Unicode
#'   "Symbol" `[S]` class
#' @param remove_numbers logical; if `TRUE` remove tokens that consist only of
#'   numbers, but not words that start with digits, e.g. `2day`
#' @param remove_url logical; if `TRUE` find and eliminate URLs beginning with
#'   http(s)
#' @param remove_separators logical; if `TRUE` remove separators and separator
#'   characters (Unicode "Separator" `[Z]` and "Control" `[C]` categories)
#' @param split_hyphens logical; if `TRUE`, split words that are connected by
#'   hyphenation and hyphenation-like characters in between words, e.g.
#'   `"self-aware"` becomes `c("self", "-", "aware")`
#' @param include_docvars if `TRUE`, pass docvars through to the tokens object.
#'   Does not apply when the input is a character data or a list of characters.
#' @inheritParams tokens_select
#' @param verbose if `TRUE`, print timing messages to the console
#' @param ... used to pass arguments among the functions
#' @section Details:
#'   As of version 2, the choice of tokenizer is left more to the user,
#'   and `tokens()` is treated more as a constructor (from a named list) than a
#'   tokenizer. This allows users to use any other tokenizer that returns a
#'   named list, and to use this as an input to `tokens()`, with removal and
#'   splitting rules applied after this has been constructed (passed as
#'   arguments).  These removal and splitting rules are conservative and will
#'   not remove or split anything, however, unless the user requests it.
#'
#'   Using external tokenizers is best done by piping the output from these
#'   other tokenizers into the `tokens()` constructor, with additional removal
#'   and splitting options applied at the construction stage.  These will only
#'   have an effect, however, if the tokens exist for which removal is specified
#'   at in the `tokens()` call.  For instance, it is impossible to remove
#'   punctuation if the input list to `tokens()` already had its punctuation
#'   tokens removed at the external tokenization stage.
#'
#'   To construct a tokens object from a list with no additional processing, call
#'   [as.tokens()] instead of `tokens()`.
#'
#'   Recommended tokenizers are those from the \pkg{tokenizers} package, which
#'   are generally faster than the default (built-in) tokenizer but always
#'   splits infix hyphens, or \pkg{spacyr}.
#'
#' @section quanteda Tokenizers:
#'   The default word tokenizer `what = "word"` splits tokens using
#'   [stri_split_boundaries(x, type = "word")][stringi::stri_split_boundaries]
#'   but by default preserves infix hyphens (e.g. "self-funding"), URLs, and
#'   social media "tag" characters (#hashtags and @usernames), and email
#'   addresses.  The rules defining a valid "tag" can be found
#'   [here](https://www.hashtags.org/featured/what-characters-can-a-hashtag-include/)
#'   for hashtags and
#'   [here](https://help.twitter.com/en/managing-your-account/twitter-username-rules)
#'   for usernames.
#'
#'   In versions < 2, the argument `remove_twitter` controlled whether social
#'   media tags were preserved or removed, even when `remove_punct = TRUE`.
#'   This argument is not longer functional in versions >= 2.  If greater
#'   control over social media tags is desired, you should user an alternative
#'   tokenizer, including non-\pkg{quanteda} options.
#'
#'   For backward compatibility, the following older tokenizers are also
#'   supported through `what`:
#'   \describe{
#'   \item{`"word1"`}{(legacy) implements similar behaviour to the version of
#'   `what = "word"` found in pre-version 2.  (It preserves social media tags
#'   and infix hyphens, but splits URLs.)  "word1" is also slower than "word".}
#'   \item{`"fasterword"`}{(legacy) splits on whitespace and control characters,
#'   using `stringi::stri_split_charclass(x, "[\\p{Z}\\p{C}]+")`}
#'   \item{`"fastestword"`}{(legacy) splits on the space character, using
#'   `stringi::stri_split_fixed(x, " ")`}
#'   \item{`"character"`}{tokenization into individual characters}
#'   \item{`"sentence"`}{sentence segmenter based on
#'   [stri_split_boundaries][stringi::stri_split_boundaries], but with
#'   additional rules to avoid splits on words like "Mr." that would otherwise
#'   incorrectly be detected as sentence boundaries.  For better sentence
#'   tokenization, consider using \pkg{spacyr}.} }
#'
#' @return \pkg{quanteda} `tokens` class object, by default a serialized list of
#'   integers corresponding to a vector of types.
#' @seealso [tokens_ngrams()], [tokens_skipgrams()], [as.list.tokens()],
#'   [as.tokens()]
#' @keywords tokens
#' @export
#' @examples
#' txt <- c(doc1 = "A sentence, showing how tokens() works.",
#'          doc2 = "@quantedainit and #textanalysis https://example.com?p=123.",
#'          doc3 = "Self-documenting code??",
#'          doc4 = "£1,000,000 for 50¢ is gr8 4ever \U0001f600")
#' tokens(txt)
#' tokens(txt, what = "word1")
#'
#' # removing punctuation marks but keeping tags and URLs
#' tokens(txt[1:2], remove_punct = TRUE)
#'
#' # splitting hyphenated words
#' tokens(txt[3])
#' tokens(txt[3], split_hyphens = TRUE)
#'
#' # symbols and numbers
#' tokens(txt[4])
#' tokens(txt[4], remove_numbers = TRUE)
#' tokens(txt[4], remove_numbers = TRUE, remove_symbols = TRUE)
#'
#' \dontrun{# using other tokenizers
#' tokens(tokenizers::tokenize_words(txt[4]), remove_symbols = TRUE)
#' tokenizers::tokenize_words(txt, lowercase = FALSE, strip_punct = FALSE) %>%
#'     tokens(remove_symbols = TRUE)
#' tokenizers::tokenize_characters(txt[3], strip_non_alphanum = FALSE) %>%
#'     tokens(remove_punct = TRUE)
#' tokenizers::tokenize_sentences(
#'     "The quick brown fox.  It jumped over the lazy dog.") %>%
#'     tokens()
#' }
#'
tokens <-  function(x,
                    what = "word",
                    remove_punct = FALSE,
                    remove_symbols = FALSE,
                    remove_numbers = FALSE,
                    remove_url = FALSE,
                    remove_separators = TRUE,
                    split_hyphens = FALSE,
                    include_docvars = TRUE,
                    padding = FALSE,
                    verbose = quanteda_options("verbose"),
                    ...) {

    tokens_env$START_TIME <- proc.time()
    object_class <- class(x)[1]
    if (verbose) catm("Creating a tokens object from a", object_class, "input...\n")

    UseMethod("tokens")
}

# GLOBAL FOR dfm THAT FUNCTIONS CAN RESET AS NEEDED TO RECORD TIME ELAPSED
tokens_env <- new.env()
tokens_env$START_TIME <- NULL

#' @rdname tokens
#' @noRd
#' @export
tokens.default <- function(x, ...) {
    stop(friendly_class_undefined_message(class(x), "tokens"))
}

#' @rdname tokens
#' @noRd
#' @export
tokens.list <- function(x,
                        what = "word",
                        remove_punct = FALSE,
                        remove_symbols = FALSE,
                        remove_numbers = FALSE,
                        remove_url = FALSE,
                        remove_separators = TRUE,
                        split_hyphens = FALSE,
                        include_docvars = TRUE,
                        padding = FALSE,
                        verbose = quanteda_options("verbose"),
                        ...) {
    tokens(as.tokens(x),
           remove_punct = remove_punct,
           remove_symbols = remove_symbols,
           remove_numbers = remove_numbers,
           remove_url = remove_url,
           remove_separators = remove_separators,
           split_hyphens = split_hyphens,
           verbose = quanteda_options("verbose"),
           ...)
}

#' @rdname tokens
#' @noRd
#' @export
tokens.character <- function(x,
                             what = "word",
                             remove_punct = FALSE,
                             remove_symbols = FALSE,
                             remove_numbers = FALSE,
                             remove_url = FALSE,
                             remove_separators = TRUE,
                             split_hyphens = FALSE,
                             include_docvars = TRUE,
                             padding = FALSE,
                             verbose = quanteda_options("verbose"),
                             ...) {
    tokens.corpus(corpus(x),
           what = what,
           remove_punct = remove_punct,
           remove_symbols = remove_symbols,
           remove_numbers = remove_numbers,
           remove_url = remove_url,
           remove_separators = remove_separators,
           split_hyphens = split_hyphens,
           include_docvars = include_docvars,
           padding = padding,
           verbose = verbose,
           ...)
}

#' @rdname tokens
#' @noRd
#' @importFrom stringi stri_startswith_fixed
#' @export
tokens.corpus <- function(x,
                          what = "word",
                          remove_punct = FALSE,
                          remove_symbols = FALSE,
                          remove_numbers = FALSE,
                          remove_url = FALSE,
                          remove_separators = TRUE,
                          split_hyphens = FALSE,
                          include_docvars = TRUE,
                          padding = FALSE,
                          verbose = quanteda_options("verbose"),
                          ...)  {
    x <- as.corpus(x)
    attrs <- attributes(x)

    dots <- list(...)
    what <- match.arg(what, c("word", "word1", "sentence", "character",
                              "fasterword", "fastestword"))
    # deprecated arguments
    if ("remove_hyphens" %in% names(dots)) {
        split_hyphens <- dots[["remove_hyphens"]]
        .Deprecated(msg = "'remove_hyphens' is deprecated, use 'split_hyphens' instead.")
        dots$remove_hyphens <- NULL
    }
    if ("remove_twitter" %in% names(dots)) {
        warning("'remove_twitter' is defunct; see 'quanteda Tokenizers' in ?tokens",
                call. = FALSE)
        dots$remove_twitter <- NULL
    }
    check_dots(dots, c(names(formals(tokens))))

    # call the appropriate tokenizer function
    if (verbose) catm(" ...starting tokenization\n")
    tokenizer_fn <- switch(what,
                           word = tokenize_word,
                           word1 = tokenize_word1,
                           sentence = tokenize_sentence,
                           character = tokenize_character,
                           fasterword = tokenize_fasterword,
                           fastestword = tokenize_fastestword)


    if (!remove_separators && !what %in% c("word", "word1", "character"))
        warning("remove_separators is always TRUE for this type")

    # split x into smaller blocks to reduce peak memory consumption
    x <- texts(x)
    x <- split(x, factor(ceiling(seq_along(x) / quanteda_options("tokens_block_size"))))
    x <- lapply(x, function(y) {
        if (verbose)
            catm(" ...", head(names(y), 1), " to ", tail(names(y), 1), "\n", sep = "")
            #catm(" ...", head(names(y), 1), " to ", tail(names(y), 1),
            #     " by process ", Sys.getpid(), "\n", sep = "")
            
        y <- normalize_characters(y)
        if (what == "word") {
            y <- preserve_special(y, split_hyphens = split_hyphens,
                                  split_tags = FALSE, verbose = verbose)
            special <- attr(y, "special")
        }
        y <- serialize_tokens(tokenizer_fn(y, split_hyphens = split_hyphens, verbose = verbose))
        if (what == "word")
            y <- restore_special(y, special)
        if (what == "word1" && !split_hyphens)
            y <- restore_special1(y, split_hyphens = FALSE, split_tags = TRUE)
        return(y)
    })
    type <- unique(unlist(lapply(x, attr, "types"), use.names = FALSE))
    if (verbose)
        catm(" ...", format(length(type), big.mark = ",", trim = TRUE),
             " unique type", if (length(type) == 1) "" else "s",
             "\n", sep = "")
    x <- lapply(x, function(y) {
        map <- fastmatch::fmatch(attr(y, "types"), type)
        y <- lapply(y, function(z) map[z])
        return(y)
    })

    result <- build_tokens(
        unlist(x, recursive = FALSE),
        types = type, what = what,
        docvars = select_docvars(attrs[["docvars"]], user = include_docvars, system = TRUE),
        meta = attrs[["meta"]]
    )

    result <- tokens.tokens(result,
                            remove_punct = remove_punct,
                            remove_symbols = remove_symbols,
                            remove_numbers = remove_numbers,
                            remove_url = remove_url,
                            remove_separators = remove_separators,
                            split_hyphens = FALSE,
                            include_docvars = TRUE,
                            padding = padding,
                            verbose = verbose)
    return(result)
}

#' @rdname tokens
#' @noRd
#' @importFrom stringi stri_startswith_fixed
#' @export
tokens.tokens <-  function(x,
                           what = "word",
                           remove_punct = FALSE,
                           remove_symbols = FALSE,
                           remove_numbers = FALSE,
                           remove_url = FALSE,
                           remove_separators = FALSE,
                           split_hyphens = FALSE,
                           include_docvars = TRUE,
                           padding = FALSE,
                           verbose = quanteda_options("verbose"),
                           ...) {
    x <- as.tokens(x)
    dots <- list(...)

    # deprecated arguments
    if ("remove_hyphens" %in% names(dots)) {
        split_hyphens <- dots[["remove_hyphens"]]
        .Deprecated(msg = "'remove_hyphens' is deprecated, use 'split_hyphens' instead.")
        dots$remove_hyphens <- NULL

    }
    if ("remove_twitter" %in% names(dots)) {
        .Deprecated(msg = "'remove_twitter' is deprecated and inactive for tokens.tokens()")
        dots$remove_twitter <- NULL
    }
    check_dots(dots, c(names(formals(tokens))))

    # splits
    if (split_hyphens) {
        if (verbose) catm(" ...splitting hyphens\n")
        x <- tokens_split(x, "\\p{Pd}", valuetype = "regex", remove_separator = FALSE)
    }

    # removals
    removals <- removals_regex(separators = remove_separators,
                               punct = remove_punct,
                               symbols = remove_symbols,
                               numbers = remove_numbers,
                               url = remove_url)

    if (length(removals) && verbose) {
        msg <- stri_replace_all_fixed(names(removals),
                                      c("url", "punct"),
                                      c("URLs", "punctuation"),
                                      vectorize_all = FALSE)
        catm(" ...removing", paste(msg, collapse = ", "), "\n")
    }

    if (length(removals[["separators"]])) {
        x <- tokens_remove(x, removals[["separators"]], valuetype = "regex",
                           verbose = FALSE)
        removals["separators"] <- NULL
    }

    if (length(removals)) {
        x <- tokens_remove(x, paste(unlist(removals), collapse = "|"),
                           valuetype = "regex",  padding = padding,
                           verbose = FALSE)
    }

    if (!include_docvars)
        docvars(x) <- NULL

    if (verbose) {
        catm(" ...complete, elapsed time:",
             format((proc.time() - tokens_env$START_TIME)[3], digits = 3), "seconds.\n")
        catm("Finished constructing tokens from ", format(length(x), big.mark = ","), " document",
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
    build_tokens(
        x,
        types = attr(x, "types"),
        concatenator = concatenator,
        docvars = make_docvars(length(x), names(x))
    )
}

#' @rdname as.tokens
#' @export
as.tokens.tokens <- function(x, ...) {
    upgrade_tokens(x)
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
is.tokens <- function(x) "tokens" %in% class(x)


# utility functions ------------

removals_regex <- function(separators = FALSE,
                           punct = FALSE,
                           symbols = FALSE,
                           numbers = FALSE,
                           url = FALSE) {
    regex <- list()
    if (separators)
        regex[["separators"]] <- "^[\\p{Z}\\p{C}]+$"
    if (punct)
        regex[["punct"]] <- "^\\p{P}+$"
    if (symbols)
        regex[["symbols"]] <- "^\\p{S}$"
    if (numbers) # includes currency amounts and those containing , or . digit separators, and 100bn
        regex[["numbers"]] <- "^\\p{Sc}{0,1}\\p{N}+([.,]*\\p{N})*\\p{Sc}{0,1}$"
    if (url)
        regex[["url"]] <- "(^((https{0,1}|s{0,1}ftp)://)|(\\w+@\\w+))"
    return(regex)
}


#' Function to serialize list-of-character tokens
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
    type <- attr(x, "types")
    if (method == "C++") {
        x <- qatd_cpp_tokens_recompile(x, type, gap, dup)
        x <- rebuild_tokens(x, attrs)
    } else {

        # Check for padding
        index_unique <- unique(unlist(unclass(x), use.names = FALSE))
        padding <- index_unique == 0
        attrs[["padding"]] <- any(padding) # add padding flag
        index_unique <- index_unique[!padding] # exclude padding

        if (!gap && !dup) return(x)

        # Remove gaps in the type index, if any, remap index
        if (gap) {
            if (any(is.na(match(seq_len(length(type)), index_unique)))) {
                type_new <- type[index_unique]
                index_new <- c(0, seq_along(index_unique)) # padding index is zero but not in types
                index_unique <- c(0, index_unique) # padding index is zero but not in types
                x <- lapply(unclass(x), function(y) index_new[fastmatch::fmatch(y, index_unique)])
                attributes(x) <- attrs
                type <- type_new
            }
        }

        # Reindex duplicates, if any
        if (dup) {
            if (any(duplicated(type))) {
                type_unique <- unique(type)
                index_mapping <- match(type, type_unique)
                index_mapping <- c(0, index_mapping) # padding index is zero but not in types
                x <- lapply(unclass(x), function(y) index_mapping[y + 1]) # shift index for padding
                attributes(x) <- attrs
                type <- type_unique
            }
        }
        Encoding(type) <- "UTF-8"
        attr(x, "types") <- type
        x <- rebuild_tokens(x, attrs)
    }
    return(x)
}

# types() --------------
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
