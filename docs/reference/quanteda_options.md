# Get or set package options for quanteda

Get or set global options affecting functions across quanteda.

## Usage

``` r
quanteda_options(..., reset = FALSE, initialize = FALSE)
```

## Arguments

- ...:

  options to be set, as key-value pair, same as
  [`options()`](https://rdrr.io/r/base/options.html). This may be a list
  of valid key-value pairs, useful for setting a group of options at
  once (see examples).

- reset:

  logical; if `TRUE`, reset all quanteda options to their default values

- initialize:

  logical; if `TRUE`, reset only the quanteda options that are not
  already defined. Used for setting initial values when some have been
  defined previously, such as in `.Rprofile`.

## Value

When called using a `key = value` pair (where `key` can be a label or
quoted character name)), the option is set and `TRUE` is returned
invisibly.

When called with no arguments, a named list of the package options is
returned.

When called with `reset = TRUE` as an argument, all arguments are
options are reset to their default values, and `TRUE` is returned
invisibly.

## Details

Currently available options are:

- `verbose`:

  logical; if `TRUE` then use this as the default for all functions with
  a `verbose` argument

- `threads`:

  integer; specifies the number of threads to use in parallelized
  functions; defaults to the maximum number of threads

- `print_dfm_max_ndoc`, `print_corpus_max_ndoc`,
  `print_tokens_max_ndoc`:

  integer; specify the number of documents to display when using the
  defaults for printing a dfm, corpus, or tokens object

- `print_dfm_max_nfeat`, `print_corpus_max_nchar`,
  `print_tokens_max_ntoken`:

  integer; specifies the number of features to display when printing a
  dfm, the number of characters to display when printing corpus
  documents, or the number of tokens to display when printing tokens
  objects

- `print_dfm_summary`:

  integer; specifies the number of documents to display when using the
  defaults for printing a dfm

- `print_dictionary_max_nkey`, `print_dictionary_max_nval`:

  the number of keys or values (respectively) to display when printing a
  dictionary

- `print_kwic_max_nrow`:

  the number of rows to display when printing a kwic object

- `base_docname`:

  character; stem name for documents that are unnamed when a corpus,
  tokens, or dfm are created or when a dfm is converted from another
  object

- `base_featname`:

  character; stem name for features that are unnamed when they are
  added, for whatever reason, to a dfm through an operation that adds
  features

- `base_compname`:

  character; stem name for components that are created by matrix
  factorization

- `language_stemmer`:

  character; language option for
  [`char_wordstem()`](https://quanteda.io/reference/tokens_wordstem.md),
  [`tokens_wordstem()`](https://quanteda.io/reference/tokens_wordstem.md),
  and
  [`dfm_wordstem()`](https://quanteda.io/reference/tokens_wordstem.md)

- `pattern_hashtag`, `pattern_username`:

  character; regex patterns for (social media) hashtags and usernames
  respectively, used to avoid segmenting these in the default internal
  "word" tokenizer

- `tokens_block_size`:

  integer; specifies the number of documents to be tokenized at a time
  in blocked tokenization. When the number is large, tokenization
  becomes faster but also memory-intensive.

- `tokens_locale`:

  character; specify locale in stringi boundary detection in
  tokenization and corpus reshaping. See
  [`stringi::stri_opts_brkiter()`](https://rdrr.io/pkg/stringi/man/stri_opts_brkiter.html).

- `tokens_tokenizer_word`:

  character; the current word tokenizer version used as a default for
  `what = "word"` in
  [`tokens()`](https://quanteda.io/reference/tokens.md), one of
  `"word1"`, `"word2"`, `"word3"` (same as `"word2"`), or `"word4"`.

## Examples

``` r
(opt <- quanteda_options())
#> $threads
#> [1] 28
#> 
#> $verbose
#> [1] FALSE
#> 
#> $print_dfm_max_ndoc
#> [1] 6
#> 
#> $print_dfm_max_nfeat
#> [1] 10
#> 
#> $print_dfm_summary
#> [1] TRUE
#> 
#> $print_corpus_max_ndoc
#> [1] 6
#> 
#> $print_corpus_max_nchar
#> [1] 60
#> 
#> $print_corpus_summary
#> [1] TRUE
#> 
#> $print_tokens_max_ndoc
#> [1] 6
#> 
#> $print_tokens_max_ntoken
#> [1] 12
#> 
#> $print_tokens_summary
#> [1] TRUE
#> 
#> $print_dictionary_max_nkey
#> [1] 6
#> 
#> $print_dictionary_max_nval
#> [1] 20
#> 
#> $print_dictionary_summary
#> [1] TRUE
#> 
#> $print_kwic_max_nrow
#> [1] 100
#> 
#> $print_kwic_summary
#> [1] TRUE
#> 
#> $base_docname
#> [1] "text"
#> 
#> $base_featname
#> [1] "feat"
#> 
#> $base_compname
#> [1] "comp"
#> 
#> $language_stemmer
#> [1] "english"
#> 
#> $pattern_hashtag
#> [1] "#[\\p{L}\\p{N}_]+#?"
#> 
#> $pattern_username
#> [1] "@[a-zA-Z0-9_]+"
#> 
#> $tokens_block_size
#> [1] 100000
#> 
#> $tokens_locale
#> [1] "en_US@ss=standard"
#> 
#> $tokens_tokenizer_word
#> [1] "word4"
#> 
# \donttest{
quanteda_options(verbose = TRUE)
quanteda_options("verbose" = FALSE)
quanteda_options("threads")
#> [1] 28
quanteda_options(print_dfm_max_ndoc = 50L)
# reset to defaults
quanteda_options(reset = TRUE)
# reset to saved options
quanteda_options(opt)
# }
```
