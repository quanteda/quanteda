# tokens() ----------

#' Construct a tokens object
#'
#' @description Construct a tokens object, either by importing a named list of
#'   characters from an external tokenizer, or by calling the internal
#'   \pkg{quanteda} tokenizer.
#'
#' @description `tokens()` can also be applied to tokens class objects, which
#'   means that the removal rules can be applied post-tokenization, although it
#'   should be noted that it will not be possible to remove things that are not
#'   present.  For instance, if the `tokens` object has already had punctuation
#'   removed, then `tokens(x, remove_punct = TRUE)` will have no additional
#'   effect.
#' @param x the input object to the tokens constructor; a [tokens], [corpus] or
#'   [character] object to tokenize.
#' @param what character; which tokenizer to use.  The default `what = "word"`
#'   is the current version of the \pkg{quanteda} tokenizer, set by
#'   `quanteda_options(okens_tokenizer_word)`. Legacy tokenizers (version < 2)
#'   are also supported, including the default `what = "word1"`. See the Details
#'   and quanteda Tokenizers below.
#' @param remove_punct logical; if `TRUE` remove all characters in the Unicode
#'   "Punctuation" `[P]` class, with exceptions for those used as prefixes for
#'   valid social media tags if `preserve_tags = TRUE`
#' @param remove_symbols logical; if `TRUE` remove all characters in the Unicode
#'   "Symbol" `[S]` class
#' @param remove_numbers logical; if `TRUE` remove tokens that consist only of
#'   numbers, but not words that start with digits, e.g. `2day`
#' @param remove_url logical; if `TRUE` removes URLs (http, https, ftp, sftp)
#'   and email addresses.
#' @param remove_separators logical; if `TRUE` remove separators and separator
#'   characters (Unicode "Separator" `[Z]` and "Control" `[C]` categories)
#' @param split_hyphens logical; if `FALSE`, do not split words that are
#'   connected by hyphenation and hyphenation-like characters in between words,
#'   e.g. `"self-aware"` becomes `c("self", "-", "aware")`
#' @param split_tags logical; if `FALSE`, do not split social media tags defined
#'   in `quanteda_options()`. The default patterns are `pattern_hashtag =
#'   "#\\w+#?"` and `pattern_username = "@[a-zA-Z0-9_]+"`.
#' @param include_docvars if `TRUE`, pass docvars through to the tokens object.
#'   Does not apply when the input is a character data or a list of characters.
#' @param concatenator character; the concatenation character that will connect
#'   the tokens making up a multi-token sequence.
#' @inheritParams tokens_select
#' @param xptr if `TRUE`, returns a `tokens_xptr` class object
#' @param verbose if `TRUE`, print timing messages to the console
#' @param ... used to pass arguments among the functions
#' @section Details: As of version 2, the choice of tokenizer is left more to
#'   the user, and `tokens()` is treated more as a constructor (from a named
#'   list) than a tokenizer. This allows users to use any other tokenizer that
#'   returns a named list, and to use this as an input to `tokens()`, with
#'   removal and splitting rules applied after this has been constructed (passed
#'   as arguments).  These removal and splitting rules are conservative and will
#'   not remove or split anything, however, unless the user requests it.
#'
#'   You usually do not want to split hyphenated words or social media tags, but
#'   extra steps required to preserve such special tokens. If there are many
#'   random characters in your texts, you should `split_hyphens = TRUE` and
#'   `split_tags = TRUE` to avoid a slowdown in tokenization.
#'
#'   Using external tokenizers is best done by piping the output from these
#'   other tokenizers into the `tokens()` constructor, with additional removal
#'   and splitting options applied at the construction stage.  These will only
#'   have an effect, however, if the tokens exist for which removal is specified
#'   at in the `tokens()` call.  For instance, it is impossible to remove
#'   punctuation if the input list to `tokens()` already had its punctuation
#'   tokens removed at the external tokenization stage.
#'
#'   To construct a tokens object from a list with no additional processing,
#'   call [as.tokens()] instead of `tokens()`.
#'
#'   Recommended tokenizers are those from the \pkg{tokenizers} package, which
#'   are generally faster than the default (built-in) tokenizer but always
#'   splits infix hyphens, or \pkg{spacyr}.  The default tokenizer in
#'   **quanteda** is very smart, however, and if you do not have special
#'   requirements, it works extremely well for most languages as well as text
#'   from social media (including hashtags and usernames).
#'
#' @section quanteda Tokenizers: The default word tokenizer `what = "word"` is
#'   updated in major version 4.  It is even smarter than the v3 and v4
#'   versions, with additional options for customization.  See
#'   [tokenize_word4()] for full details.
#' 
#'   The default tokenizer splits tokens using [stri_split_boundaries(x, type =
#'   "word")][stringi::stri_split_boundaries] but by default preserves infix
#'   hyphens (e.g. "self-funding"), URLs, and social media "tag" characters
#'   (#hashtags and @usernames), and email addresses.  The rules defining a
#'   valid "tag" can be found at
#'   https://www.hashtags.org/featured/what-characters-can-a-hashtag-include/
#'   for hashtags and at
#'   https://help.twitter.com/en/managing-your-account/twitter-username-rules
#'   for usernames.
#'
#'   For backward compatibility, the following older tokenizers are also
#'   supported through `what`: 
#'   \describe{ 
#'   \item{`"word1"`}{(legacy) implements
#'   similar behaviour to the version of `what = "word"` found in pre-version 2.
#'   (It preserves social media tags and infix hyphens, but splits URLs.)
#'   "word1" is also slower than "word2" and "word4".  In "word1",
#'   the argument `remove_twitter` controlled whether social
#'   media tags were preserved or removed, even when `remove_punct = TRUE`. This
#'   argument is not longer functional in versions >= 2, but equivalent control
#'   can be had using the `split_tags` argument and selective tokens removals.}
#'   \item{`"word2", "word3"`}{(legacy) implements
#'   similar behaviour to the versions of "word" found in \pkg{quanteda} versions
#'   3 and 4.} 
#'   \item{`"fasterword"`}{(legacy) splits
#'   on whitespace and control characters, using
#'   `stringi::stri_split_charclass(x, "[\\p{Z}\\p{C}]+")`}
#'   \item{`"fastestword"`}{(legacy) splits on the space character, using
#'   `stringi::stri_split_fixed(x, " ")`} \item{`"character"`}{tokenization into
#'   individual characters} \item{`"sentence"`}{sentence segmenter based on
#'   [stri_split_boundaries][stringi::stri_split_boundaries], but with
#'   additional rules to avoid splits on words like "Mr." that would otherwise
#'   incorrectly be detected as sentence boundaries.  For better sentence
#'   tokenization, consider using \pkg{spacyr}.} }
#' @return \pkg{quanteda} `tokens` class object, by default a serialized list of
#'   integers corresponding to a vector of types.
#' @seealso [tokens_ngrams()], [tokens_skipgrams()], [tokens_compound()],
#'   [tokens_lookup()], [concat()], [as.list.tokens()], [as.tokens()]
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
#' tokenizers::tokenize_words(txt, lowercase = FALSE, strip_punct = FALSE) |>
#'     tokens(remove_symbols = TRUE)
#' tokenizers::tokenize_characters(txt[3], strip_non_alphanum = FALSE) |>
#'     tokens(remove_punct = TRUE)
#' tokenizers::tokenize_sentences(
#'     "The quick brown fox.  It jumped over the lazy dog.") |>
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
                    split_tags = FALSE,
                    include_docvars = TRUE,
                    padding = FALSE,
                    concatenator = "_",
                    verbose = quanteda_options("verbose"),
                    ...,
                    xptr = FALSE) {
    UseMethod("tokens")
}

#' @rdname tokens
#' @noRd
#' @export
tokens.default <- function(x, ...) {
    check_class(class(x), "tokens")
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
                        split_tags = FALSE,
                        include_docvars = TRUE,
                        padding = FALSE,
                        concatenator = "_",
                        verbose = quanteda_options("verbose"),
                        ...) {
    
    if (is.null(global$object_class)) {
        global$object_class <- class(x)[1]
        global$proc_time <- proc.time()   
    }
    
    tokens(as.tokens(x),
           remove_punct = remove_punct,
           remove_symbols = remove_symbols,
           remove_numbers = remove_numbers,
           remove_url = remove_url,
           remove_separators = remove_separators,
           split_hyphens = split_hyphens,
           split_tags = split_tags,
           concatenator = concatenator,
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
                             split_tags = FALSE,
                             include_docvars = TRUE,
                             padding = FALSE,
                             concatenator = "_",
                             verbose = quanteda_options("verbose"),
                             ...,
                             xptr = FALSE) {
    
    if (is.null(global$object_class)) {
        global$object_class <- class(x)[1]
        global$proc_time <- proc.time()   
    }
    
    tokens.corpus(corpus(x),
           what = what,
           remove_punct = remove_punct,
           remove_symbols = remove_symbols,
           remove_numbers = remove_numbers,
           remove_url = remove_url,
           remove_separators = remove_separators,
           split_hyphens = split_hyphens,
           split_tags = split_tags,
           include_docvars = include_docvars,
           padding = padding,
           concatenator = concatenator,
           verbose = verbose,
           ...,
           xptr = xptr)
}

#' @rdname tokens
#' @noRd
#' @importFrom utils getFromNamespace
#' @export
tokens.corpus <- function(x,
                          what = "word",
                          remove_punct = FALSE,
                          remove_symbols = FALSE,
                          remove_numbers = FALSE,
                          remove_url = FALSE,
                          remove_separators = TRUE,
                          split_hyphens = FALSE,
                          split_tags = FALSE,
                          include_docvars = TRUE,
                          padding = FALSE,
                          concatenator = "_",
                          verbose = quanteda_options("verbose"),
                          ...,
                          xptr = FALSE)  {
    
    if (is.null(global$object_class)) {
        global$object_class <- class(x)[1]
        global$proc_time <- proc.time()   
    }
    
    x <- as.corpus(x)
    
    if (verbose) {
        if (xptr) {
            catm("Creating a tokens_xptr from a", global$object_class, "object...\n")
        } else {
            catm("Creating a tokens from a", global$object_class, "object...\n")
        }
    }
        
    
    what <- match.arg(what, c("word", paste0("word", 1:4), 
                              "sentence", "character",
                              "fasterword", "fastestword"))
    remove_punct <- check_logical(remove_punct)
    remove_symbols <- check_logical(remove_symbols)
    remove_numbers <- check_logical(remove_numbers)
    remove_url <- check_logical(remove_url)
    remove_separators <- check_logical(remove_separators)
    split_hyphens <- check_logical(split_hyphens)
    split_tags <- check_logical(split_tags)
    include_docvars <- check_logical(include_docvars)
    padding <- check_logical(padding)
    concatenator <- check_character(concatenator)
    verbose <- check_logical(verbose)
    check_dots(..., method = c("tokens", "tokenize_word4"))
    
    attrs <- attributes(x)
    
    # call the appropriate tokenizer function
    if (verbose) catm(" ...starting tokenization\n")
    
    tokenizer <- switch(what,
                        word = paste0("tokenize_", quanteda_options("tokens_tokenizer_word")),
                        sentence = "tokenize_sentence",
                        character = "tokenize_character",
                        # only for backward compatibility
                        word4 = "tokenize_word4",
                        word3 = "tokenize_word2",
                        word2 = "tokenize_word2",
                        word1 = "tokenize_word1",
                        fasterword = "tokenize_fasterword", 
                        fastestword = "tokenize_fastestword")
    
    tokenizer_fn <- tryCatch(
        getFromNamespace(tokenizer, ns = "quanteda"),
        error = function(e) {
        stop("Invalid value in tokens_tokenizer_word", call. = FALSE)
    })
    
    if (!remove_separators && tokenizer %in% 
        paste0("tokenize_", c("fasterword", "fastestword", "sentence")))
        warning("remove_separators is always TRUE for this type")
    
    if (tokenizer == "tokenize_word1") {
        x <- preserve_special1(x, split_hyphens = split_hyphens,
                               split_tags = split_tags, verbose = verbose)
    } else if (tokenizer %in% c("tokenize_word2", "tokenize_word3")) {
        x <- preserve_special(x, split_hyphens = split_hyphens,
                              split_tags = split_tags, verbose = verbose)
        special <- attr(x, "special")
    }
    
    # split x into smaller blocks to reduce peak memory consumption
    x <- as.character(x)
    x <- split(x, factor(ceiling(seq_along(x) / quanteda_options("tokens_block_size"))))
    
    result <- cpp_serialize(list())
    for (i in seq_along(x)) {
        if (verbose) catm(" ...tokenizing", i, "of", length(x), "blocks\n")
        temp <- tokenizer_fn(x[[i]], split_hyphens = split_hyphens, split_tags = split_tags, 
                             verbose = verbose, ...)
        result <- cpp_serialize_add(temp, result, get_threads())
    }
    result <- build_tokens(
        result, 
        types = NULL, 
        what = what,
        tokenizer = tokenizer,
        concatenator = concatenator,
        docvars = select_docvars(attrs[["docvars"]], user = include_docvars, system = TRUE),
        meta = attrs[["meta"]]
    )
    
    if (tokenizer == "tokenize_word1") {
        result <- restore_special1(result, split_hyphens = split_hyphens,
                                   split_tags = split_tags)
    } else if (tokenizer %in% c("tokenize_word2", "tokenize_word3")) {
        result <- restore_special(result, special)
    } else if (tokenizer == "tokenize_word4") {
        result <- tokens_restore(result)
    }
    
    result <- tokens(result,
                     remove_punct = remove_punct,
                     remove_symbols = remove_symbols,
                     remove_numbers = remove_numbers,
                     remove_url = remove_url,
                     remove_separators = remove_separators,
                     split_hyphens = FALSE,
                     split_tags = FALSE,
                     include_docvars = TRUE,
                     padding = padding,
                     concatenator = concatenator,
                     verbose = verbose)
    
    if (!xptr)
        result <- as.tokens(result)
    
    if (verbose) {
        n <- length(types(result))
        catm(" ...", format(n, big.mark = ",", trim = TRUE),
             " unique type", if (n == 1) "" else "s", "\n", sep = "")
        catm(" ...complete, elapsed time:",
             format((proc.time() - global$proc_time)[3], digits = 3), "seconds.\n")
        
        if (xptr) {
            catm("Finished constructing tokens_xptr from ", format(ndoc(result), big.mark = ","), " document",
                 if (ndoc(result) > 1) "s", ".\n", sep = "")
        } else {
            catm("Finished constructing tokens from ", format(ndoc(result), big.mark = ","), " document",
                 if (ndoc(result) > 1) "s", ".\n", sep = "")
        }
    }
    global$object_class <- NULL
    return(result)
}

#' @rdname tokens
#' @noRd
#' @importFrom stringi stri_replace_all_fixed
#' @export
tokens.tokens_xptr <-  function(x,
                           what = "word",
                           remove_punct = FALSE,
                           remove_symbols = FALSE,
                           remove_numbers = FALSE,
                           remove_url = FALSE,
                           remove_separators = TRUE,
                           split_hyphens = FALSE,
                           split_tags = FALSE,
                           include_docvars = TRUE,
                           padding = FALSE,
                           concatenator = "_",
                           verbose = quanteda_options("verbose"),
                           ...) {

    if (is.null(global$object_class)) {
        global$object_class <- class(x)[1]
        global$proc_time <- proc.time()   
    }
    
    remove_punct <- check_logical(remove_punct)
    remove_symbols <- check_logical(remove_symbols)
    remove_numbers <- check_logical(remove_numbers)
    remove_url <- check_logical(remove_url)
    remove_separators <- check_logical(remove_separators)
    split_hyphens <- check_logical(split_hyphens)
    split_tags <- check_logical(split_tags)
    include_docvars <- check_logical(include_docvars)
    padding <- check_logical(padding)
    concatenator <- check_character(concatenator)
    verbose <- check_logical(verbose)
    check_dots(..., method = c("tokens", "tokenize_word4"))
    
    # splits
    if (split_hyphens) {
        if (verbose) catm(" ...splitting hyphens\n")
        x <- tokens_split(x, "\\p{Pd}", valuetype = "regex", remove_separator = FALSE)
    }
    if (split_tags) {
        warning("split_tags argument is not used", call. = FALSE)
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
    
    if (!identical(get_concatenator(x), concatenator)) {
        #warning('concatenator changed from "', 
        #        get_concatenator(x), '" to "', concatenator, '"', call. = FALSE)
        set_types(x) <- stri_replace_all_fixed(get_types(x), get_concatenator(x), 
                                               concatenator)
        set_concatenator(x) <- concatenator
    }
    
    global$object_class <- NULL
    return(x)
}

#' @export
tokens.tokens <- function(x, ...) {
    if (is.null(global$object_class)) {
        global$object_class <- class(x)[1]
        global$proc_time <- proc.time()   
    }
    as.tokens(tokens(as.tokens_xptr(x), ...))
}

# coercion and checking functions -----------

#' Coercion, checking, and combining functions for tokens objects
#'
#' Coercion functions to and from [tokens] objects, checks for whether an
#' object is a [tokens] object, and functions to combine [tokens]
#' objects.
#' @param x object to be coerced or checked
#' @inheritParams tokens
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
as.tokens.default <- function(x, concatenator = "_", ...) {
    check_class(class(x), "as.tokens")
}

#' @importFrom stringi stri_trans_nfc
#' @export
as.tokens.list <- function(x, concatenator = "_", ...) {
    result <- build_tokens(
        cpp_serialize(lapply(x, as.character), get_threads()),
        types = NULL,
        concatenator = concatenator,
        docvars = make_docvars(length(x), names(x))
    )
    as.tokens(result)
}

#' @export
as.tokens.tokens <- function(x, ...) {
    x <- upgrade_tokens(x)
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
is.tokens <- function(x) "tokens" %in% class(x)


# utility functions ------------

removals_regex <- function(separators = FALSE,
                           punct = FALSE,
                           symbols = FALSE,
                           numbers = FALSE,
                           url = FALSE) {
    regex <- list()
    if (separators)
        regex[["separators"]] <- "^[\\p{Z}\\p{C}\\p{M}]+$"
    if (punct)
        regex[["punct"]] <- "^\\p{P}$"
    if (symbols)
        regex[["symbols"]] <- "^\\p{S}$"
    if (numbers) # includes currency amounts and those containing , or . digit separators, and 100bn
        regex[["numbers"]] <- "^\\p{Sc}{0,1}\\p{N}+([.,]*\\p{N})*\\p{Sc}{0,1}$"
    if (url) 
        regex[["url"]] <- "^([-a-zA-Z0-9+.]{2,20}://|www\\.)|^([-+a-zA-Z0-9_.]+@[-a-zA-Z0-9]+(\\.[-a-zA-Z0-9]+)*\\.[a-z]+)$"
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
    types <- unlist_character(x, unique = TRUE, use.names = FALSE)
    # remove empty types and control characters
    types <- types[nzchar(types) & !stri_detect_regex(types, "^[\\p{Cf}]+$")]
    types <- union(types_reserved, types) # append new types

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
#' unclass(tokens_select(toks3, dict, padding = TRUE))
#'
#' # ngrams
#' unclass(tokens_ngrams(toks3, n = 2:3))
#'
#' @keywords internal tokens
tokens_recompile <- function(x, method = c("C++", "R")) {

    method <- match.arg(method)
    attrs <- attributes(x)
    type <- attr(x, "types")
    if (method == "C++") {
        x <- as.tokens_xptr(x)
        cpp_recompile(x)
        x <- as.tokens(x)
    } else {

        # Check for padding
        index_unique <- unique(unlist(unclass(x), use.names = FALSE))
        padding <- index_unique == 0
        attrs[["padding"]] <- any(padding) # add padding flag
        index_unique <- index_unique[!padding] # exclude padding

        # Remove gaps in the type index, if any, remap index
        if (any(is.na(match(seq_len(length(type)), index_unique)))) {
            type_new <- type[index_unique]
            index_new <- c(0, seq_along(index_unique)) # padding index is zero but not in types
            index_unique <- c(0, index_unique) # padding index is zero but not in types
            x <- lapply(unclass(x), function(y) index_new[fastmatch::fmatch(y, index_unique)])
            attributes(x) <- attrs
            type <- type_new
        }

        # Reindex duplicates, if any
        if (any(duplicated(type))) {
            type_unique <- unique(type)
            index_mapping <- match(type, type_unique)
            index_mapping <- c(0, index_mapping) # padding index is zero but not in types
            x <- lapply(unclass(x), function(y) index_mapping[y + 1]) # shift index for padding
            attributes(x) <- attrs
            type <- type_unique
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
#' head(types(toks), 20)
types <- function(x) {
    UseMethod("types")
}

#' @export
types.default <- function(x) {
    check_class(class(x), "types")
}

#' @export
types.tokens <- function(x) {
    attr(x, "types")
}

"types<-" <- function(x, value) {
    set_types(x) <- value
    #UseMethod("types<-")
}

# "types<-.tokens" <- function(x, value) {
#     set_types(x) <- value
# }
# 
# "types<-.tokens_xptr" <- function(x, value) {
#     set_types(x) <- value
# }


# concatenator functions --------------

#' Return the concatenator character from an object
#'
#' Get the concatenator character from a [tokens] object.
#' @param x a [tokens] object
#' @returns a character of length 1
#' @details The concatenator character is a special delimiter used to link
#' separate tokens in multi-token phrases.  It is embedded in the meta-data of
#' tokens objects and used in downstream operations, such as [tokens_compound()]
#' or [tokens_lookup()].  It can be extracted using [concat()] and set using
#' `tokens(x, concatenator = ...)` when `x` is a tokens object.
#'
#' The default `_` is recommended since it will not be removed during normal
#' cleaning and tokenization (while nearly all other punctuation characters, at
#' least those in the Unicode punctuation class `[P]` will be removed).
#' @export
#' @examples
#' toks <- tokens(data_corpus_inaugural[1:5])
#' concat(toks)
#' 
concat <- function(x) {
    UseMethod("concat")
}

#' @export
concat.default <- function(x) {
    check_class(class(x), "concat")
}

#' @export
concat.tokens <- function(x) {
    get_concatenator(x)
}

#' @rdname concat
#' @export
concatenator <- function(x) {
    UseMethod("concatenator")
}

#' @export
concatenator.default <- function(x) {
    check_class(class(x), "concatenator")
}

#' @export
concatenator.tokens <- function(x) {
    get_concatenator(x)
}
