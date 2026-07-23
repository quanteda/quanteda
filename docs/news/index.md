# Changelog

## quanteda 4.5

### Bug fixes

- Fix [`dictionary()`](https://quanteda.io/reference/dictionary.md) to
  handle `separator` correctly when a YAML file is provided.

### Changes and additions

- Added
  [`tokens_recompile()`](https://quanteda.io/reference/tokens_recompile.md)
  to reassign token IDs and remove duplicate or unused types from
  `tokens` and `tokens_xptr` objects.
  ([\#2506](https://github.com/quanteda/quanteda/issues/2506))

- Query functions such as
  [`types()`](https://quanteda.io/reference/types.md),
  [`ntype()`](https://quanteda.io/reference/ntoken.md), and
  [`ntoken()`](https://quanteda.io/reference/ntoken.md) no longer
  recompile `tokens_xptr` objects. Subsetting documents now preserves
  the complete type table, consistently with `dfm` objects.
  ([\#2506](https://github.com/quanteda/quanteda/issues/2506))

- Improve `print` methods for corpus, tokens, dfm, fcm, dictionary and
  kwic objects to wrap long lines at `getOption("width")`.

- Added
  [`tokens_match()`](https://quanteda.io/reference/tokens_match.md) to
  match token IDs in separately tokenized objects.

- Added an [`as.matrix()`](https://rdrr.io/r/base/matrix.html) method
  for `tokens` objects, returning a dense \[document × position\]
  integer matrix of token IDs, with `length`, `extract`, and `drop`
  arguments.
  ([\#2497](https://github.com/quanteda/quanteda/issues/2497))

- [`as.tensor()`](https://quanteda.io/reference/as.tokens.md) for
  `tokens` now returns a dense tensor rather than a sparse COO tensor,
  gains an `extract` argument for selecting documents, and passes `...`
  through to
  [`torch::torch_tensor()`](https://torch.mlverse.org/docs/reference/torch_tensor.html).
  ([\#2497](https://github.com/quanteda/quanteda/issues/2497))

- Added [`docvars()`](https://quanteda.io/reference/docvars.md) and
  `docvars<-()` methods for `tokens_xptr` objects, allowing document
  variables to be read and assigned without recompiling the tokens.
  ([\#2497](https://github.com/quanteda/quanteda/issues/2497))

- Added `trim = TRUE` to [`dfm()`](https://quanteda.io/reference/dfm.md)
  to allow users to disable trimming of features.

- Add the `drop0()` method for dfm and fcm
  ([\#2460](https://github.com/quanteda/quanteda/issues/2460)).

- Added the `max_n` argument to
  [`dfm_trim()`](https://quanteda.io/reference/dfm_trim.md) and
  [`tokens_trim()`](https://quanteda.io/reference/tokens_trim.md) to
  keep only the most frequent features, with ties broken by feature
  order. ([\#2496](https://github.com/quanteda/quanteda/issues/2496))

- Improved the C++ code for `tokens` and `tokens_xptr` objects to reduce
  memory usage.

## quanteda 4.4

CRAN release: 2026-04-06

### Changes and additions

- Added `levels` argument to
  [`dictionary()`](https://quanteda.io/reference/dictionary.md) to
  select levels
  ([\#2001](https://github.com/quanteda/quanteda/issues/2001)).

- Added `normalize` argument to
  [`tokens()`](https://quanteda.io/reference/tokens.md) to replace
  Unicode quotation marks and hyphens with their ASCII equivalent
  ([\#2480](https://github.com/quanteda/quanteda/issues/2480)).

- Added [`as.tensor()`](https://quanteda.io/reference/as.tokens.md)
  method for tokens objects to convert tokens to sparse tensors
  compatible with the package
  ([\#2474](https://github.com/quanteda/quanteda/issues/2474)).

- Added
  [`tokens_annotate()`](https://quanteda.io/reference/tokens_annotate.md)
  to insert tags to a tokens object using a dictionary.

- Added `tokenize` argument to
  [`dictionary()`](https://quanteda.io/reference/dictionary.md) to match
  dictionary values and tokens more easily in Japanese and Chinese
  ([\#2476](https://github.com/quanteda/quanteda/issues/2476)).

- Update
  [`corpus_reshape()`](https://quanteda.io/reference/corpus_reshape.md)
  to segment paragraphs more accurately
  ([\#2468](https://github.com/quanteda/quanteda/issues/2468)).

- Update [`index()`](https://quanteda.io/reference/index.md) and
  [`kwic()`](https://quanteda.io/reference/kwic.md) to skip nested
  matches in a similar way as
  [`tokens_lookup()`](https://quanteda.io/reference/tokens_lookup.md)
  ([\#2063](https://github.com/quanteda/quanteda/issues/2063)).

- Update [`corpus()`](https://quanteda.io/reference/corpus.md) to always
  append segment numbers to docnames when `unique_docnames = FALSE`.
  This argument is also added to
  [`as.tokens()`](https://quanteda.io/reference/as.tokens.md) and
  `docnames<-` for the same behaviour
  ([\#2486](https://github.com/quanteda/quanteda/issues/2486)).

## quanteda 4.3.1

CRAN release: 2025-07-10

### Changes and additions

- Improved the speed of
  [`tokens_trim()`](https://quanteda.io/reference/tokens_trim.md) when
  tokens are very large.

### Bug fixes

- Fixed the Trump 2025 speech in `data_corpus_inaugural`, which was
  mistakenly a duplicate of Biden 2021.

## quanteda 4.3.0

CRAN release: 2025-05-20

### Changes and additions

- Added
  [`corpus_chunk()`](https://quanteda.io/reference/corpus_chunk.md) for
  chunking texts into smaller documents.

- Significantly reduce the memory usage for the `c` operation on large
  `tokens` and `tokens_xptr` objects.

- Further improvements to the verbose messages for corpus, tokens, dfm
  and fcm objects.

- [`tokens_ngrams()`](https://quanteda.io/reference/tokens_ngrams.md)
  now includes a new argument `apply_if`, functioning similar to this
  argument in
  [`tokens_compound()`](https://quanteda.io/reference/tokens_compound.md)
  and
  [`tokens_lookup()`](https://quanteda.io/reference/tokens_lookup.md)
  ([\#2390](https://github.com/quanteda/quanteda/issues/2390)).

- Replaced `remove_unigram` with `match_pattern` in
  [`object2id()`](https://quanteda.io/reference/object2id.md) to control
  the matching of single-word patterns or multi-word patterns.

- `data_corpus_inaugural` now updated for Trump 2025.

## quanteda 4.2.0

CRAN release: 2025-01-08

### Changes and additions

- Made the `c` operation on `tokens` and `tokens_xptr` objects
  significantly faster.

- New, and more consistent verbose messages for tokens and dfm objects.

- Preserve the default `concatenator` of tokens objects in
  [`tokens_compound()`](https://quanteda.io/reference/tokens_compound.md)
  ([\#2432](https://github.com/quanteda/quanteda/issues/2432)).

- Make the `c` operation on `tokens` and `tokens_xptr` objects
  significantly faster.

### Bug fixes and stability enhancements

- Fix a bug in
  [`dfm_lookup()`](https://quanteda.io/reference/dfm_lookup.md) that
  leads to wrong feature names when `exclusive = TRUE`
  ([\#2424](https://github.com/quanteda/quanteda/issues/2424)).

## quanteda 4.1

### Bug fixes and stability enhancements

- Improved the handling of invisible control characters causing some
  tokens operations to crash
  ([\#2407](https://github.com/quanteda/quanteda/issues/2407)).

- Addressed [\#2358](https://github.com/quanteda/quanteda/issues/2358)
  and [\#2359](https://github.com/quanteda/quanteda/issues/2359) more
  thoroughly, at the C++ level, to convert list(integer(), integer(),…)
  to std::vector\<std::vector. This function removes negative values and
  NA_INTEGER.

- Removed **RcppArmadillo** as a dependency in an effort to avoid UBSAN
  warnings in
  [\#2417](https://github.com/quanteda/quanteda/issues/2417).

### Changes and additions

- Added [`tokens_trim()`](https://quanteda.io/reference/tokens_trim.md)
  function similar to
  [`dfm_trim()`](https://quanteda.io/reference/dfm_trim.md)
  ([\#2419](https://github.com/quanteda/quanteda/issues/2419)).

- Added `keep_unigrams` argument to
  [`tokens_compound()`](https://quanteda.io/reference/tokens_compound.md),
  to keep in the returned object the unigrams that are to be compounded
  ([\#2399](https://github.com/quanteda/quanteda/issues/2399)).

- [`print.tokens()`](https://quanteda.io/reference/print-methods.md) now
  allows passing arguments to base
  [`print()`](https://rdrr.io/r/base/print.html) via `...`, providing
  for instance the ability to print tokens without surround quotes using
  `quote = FALSE`
  ([\#2381](https://github.com/quanteda/quanteda/issues/2381)).

## quanteda 4.0.2

CRAN release: 2024-04-24

### Bug fixes and stability enhancements

- Rewrite the recompilation function to serialize tokens more
  systematically
  ([\#2375](https://github.com/quanteda/quanteda/issues/2375)).

## quanteda 4.0.1

CRAN release: 2024-04-08

### Bug fixes and stability enhancements

- Make detection of Intel oneAPI Threads Building Blocks (TBB) library
  more reliable.

## quanteda 4.0.0

CRAN release: 2024-04-04

### Changes and additions

- Introduces the `tokens_xptr` objects that extend the `tokens` objects
  with external pointers for a greater efficiency. Once `tokens` objects
  are converted to `tokens_xptr` objects using
  [`as.tokens_xptr()`](https://quanteda.io/reference/tokens_xptr.md),
  `tokens_*.tokens_xptr()` methods are called automatically.

- Improved C++ functions to allow the users to change the number of
  threads for parallel computing in more flexible manner using
  [`quanteda_options()`](https://quanteda.io/reference/quanteda_options.md).
  The value of `threads` can be changed in the middle of analysis
  pipeline.

- Makes `"word4"` the default (word) tokeniser, with improved
  efficiency, language handling, and customisation options.

- Replaced all occurrences of the **magrittr** `%>%` pipe with the R
  pipe `|>` introduced in R 4.1, although the `%>%` pipe is still
  re-exported and therefore available to all users of **quanteda**
  without loading any additional packages.

- Added `min_ntoken` and `max_ntoken` to
  [`tokens_subset()`](https://quanteda.io/reference/tokens_subset.md)
  and [`dfm_subset()`](https://quanteda.io/reference/dfm_subset.md) to
  extract documents based on number of tokens easily. It is equivalent
  to selecting documents using
  [`ntoken()`](https://quanteda.io/reference/ntoken.md).

- Added a new argument `apply_if` that allows a tokens-based operation
  to apply only to documents that meet a logical condition. This
  argument has been added to
  [`tokens_select()`](https://quanteda.io/reference/tokens_select.md),
  [`tokens_compound()`](https://quanteda.io/reference/tokens_compound.md),
  [`tokens_replace()`](https://quanteda.io/reference/tokens_replace.md),
  [`tokens_split()`](https://quanteda.io/reference/tokens_split.md), and
  [`tokens_lookup()`](https://quanteda.io/reference/tokens_lookup.md).
  This is similar to applying
  [`purrr::map_if()`](https://purrr.tidyverse.org/reference/map_if.html)
  to a tokens object, but is implemented within the function so that it
  can be performed efficiently in C++.

- Added new arguments `append_key`, `separator` and `concatenator` to
  [`tokens_lookup()`](https://quanteda.io/reference/tokens_lookup.md).
  These allow tokens matched by dictionary values to be retained with
  their keys appended to them, separated by `separator`. The addition of
  the `concatenator` argument allows additional control at the lookup
  stage for tokens that will be concatenated from having matched
  multi-word dictionary values.
  ([\#2324](https://github.com/quanteda/quanteda/issues/2324))

- Added a new argument `remove_padding` to
  [`ntoken()`](https://quanteda.io/reference/ntoken.md) and
  [`ntype()`](https://quanteda.io/reference/ntoken.md) that allows for
  not counting padding that might have been left over from
  `tokens_remove(x, padding = TRUE`). This changes the previous number
  of types from [`ntype()`](https://quanteda.io/reference/ntoken.md)
  when pads exist, by counting pads by default.
  ([\#2336](https://github.com/quanteda/quanteda/issues/2336))

- Removed dependency on **RcppParallel** to improve the stability of the
  C++ code. This change requires the users of Linux-like OS to install
  the Intel TBB library manually to enable parallel computing.

### Removals

- [`bootstrap_dfm()`](https://quanteda.io/reference/bootstrap_dfm.md)
  was removed for character and corpus objects. The correct way to
  bootstrap sentences is not to tokenize them as sentences and then
  bootstrap them from the dfm. This is consistent with requiring the
  user to tokenise objects prior to forming dfms or other “downstream”
  objects.

- [`dfm()`](https://quanteda.io/reference/dfm.md) no longer works on
  character or corpus objects, only on tokens or other dfm objects. This
  was deprecated in v3 and removed in v4.

- Very old arguments to [`dfm()`](https://quanteda.io/reference/dfm.md)
  options that were not visible but worked with warnings (such as
  `stem = TRUE`) are removed.

- Deprecated or renamed arguments formerly passed in
  [`tokens()`](https://quanteda.io/reference/tokens.md) that formerly
  mapped to the v3 arguments with a warning are removed.

- Methods for **readtext** objects are removed, since these are
  data.frame objects that are straightforward to convert into a `corpus`
  object.

- [`topfeatures()`](https://quanteda.io/reference/topfeatures.md) no
  longer works on an fcm object.
  ([\#2141](https://github.com/quanteda/quanteda/issues/2141))

### Deprecations

- Some on-the-fly calculations applied to character or corpus objects
  that require a temporary tokenisation are now deprecated. This
  includes:
  - [`nsentence()`](https://quanteda.io/reference/nsentence.md) – use
    `lengths(tokens(x, what = "sentence"))` instead;\
  - [`ntype()`](https://quanteda.io/reference/ntoken.md) – use
    `ntype(tokens(x))` instead; and.
  - [`ntoken()`](https://quanteda.io/reference/ntoken.md) – use
    `ntoken(tokens(x))` instead.
  - [`char_ngrams()`](https://quanteda.io/reference/tokens_ngrams.md) –
    use `tokens_ngrams(tokens(x))` instead.
- [`corpus.kwic()`](https://quanteda.io/reference/corpus.md) is
  deprecated, with the suggestion to form a corpus from using
  `tokens_select(x, window = ...)` instead.

### Bug fixes and stability enhancements

- [`tokens_group()`](https://quanteda.io/reference/tokens_group.md)
  works efficiently even when the number of documents and groups are
  very large.

## quanteda 3.3.1

CRAN release: 2023-05-18

### Bug fixes and stability enhancements

- Fixed a potential crash when calling
  [`tokens_compound()`](https://quanteda.io/reference/tokens_compound.md)
  with patterns containing paddings
  ([\#2254](https://github.com/quanteda/quanteda/issues/2254)).

- Updated for compatibility with (forthcoming) Matrix 1.5.5 handling of
  dimnames() for empty dimensions.

- restores `readtext` object class method extensions, to work better
  with the **readtext** package.

- Removes some unused internal methods, such as `docvars.kwic()` that
  were not exported despite matching exported generics.

## quanteda 3.3.0

CRAN release: 2023-04-07

### Changes and additions

- Implements a `"word4"` tokeniser that is based on new RBBI
  (RuleBasedBreakIterator) rules, implemented in a new .yml file that
  can be edited and changed by users, but whose defaults represent a
  significant improvement in pattern handling for words, sentences, and
  other forms of patterns. These rules are customised from the ICU rules
  for breaks, with the standard and customised rules found now in the
  `breakrules/` system folder, so that they could, in principle, be
  modified by the user.

- Other minor changes:

  - changes how elapsed time is recorded, by creating a global
    environment to record these in (aaa.R)
  - improves several of the R-coded patterns that apply to `"word2"`:
    - the hashtag pattern (\`pattern_hashtag)
    - the separator pattern (by adding `\\p{M}`).
    - the URL pattern
  - creates a new tokens_restore(), implemented in C++, to replace the
    older `preserve_special()` that rejoined splits created by the
    default stringi tokeniser machinery.\
  - makes some technical improvements to internal tokenisation
    functions, such as moving the ellipsis to the end of the function,
    to allow more modularity in developing future tokenisers.

### Bug fixes and stability enhancements

- [`dfm_group()`](https://quanteda.io/reference/dfm_group.md) now works
  correctly with an empty dfm
  ([\#2225](https://github.com/quanteda/quanteda/issues/2225)).
- `convert(x, to = "stm")` no longer vulnerable to large numbers of
  removed features as in
  [\#2189](https://github.com/quanteda/quanteda/issues/2189).

## quanteda 3.2.5

### Changes and additions

- [`segid()`](https://quanteda.io/reference/docnames.md) is added to
  extract document serial numbers from corpus, tokens or dfm objects.

## quanteda 3.2.4

CRAN release: 2022-12-08

### Bug fixes and stability enhancements

Fixes test failures caused by recent changes to **Matrix** package
behaviours on some operating systems.

## quanteda 3.2.3

CRAN release: 2022-08-29

### Bug fixes and stability enhancements

- **Matrix** package calls updated for compatibility with **Matrix**
  1.4.2. ([\#2182](https://github.com/quanteda/quanteda/issues/2182))
- Changes to C++ code for
  [`fcm()`](https://quanteda.io/reference/fcm.md) to prevent some
  (chance) errors downstream in **LSX**.
  ([\#2181](https://github.com/quanteda/quanteda/issues/2181))

## quanteda 3.2.2

CRAN release: 2022-08-09

### Bug fixes and stability enhancements

- [`fcm()`](https://quanteda.io/reference/fcm.md) computes the marginal
  frequency of upper-case tokens correctly
  ([\#2176](https://github.com/quanteda/quanteda/issues/2176)).
- [`tokens_chunk()`](https://quanteda.io/reference/tokens_chunk.md)
  keeps all the docid, including those of empty documents, in the
  original object.
- [`tokens_select()`](https://quanteda.io/reference/tokens_select.md)
  recycles values when the length of `startpos` or `endpos` is less than
  `ndoc(x)`.
- [`tokens_lookup()`](https://quanteda.io/reference/tokens_lookup.md)
  and [`dfm_lookup()`](https://quanteda.io/reference/dfm_lookup.md) can
  apply very large dictionaries (more than 100,000 keys).

## quanteda 3.2.1

CRAN release: 2022-03-01

### Bug fixes and stability enhancements

- [`dfm_lookup()`](https://quanteda.io/reference/dfm_lookup.md) ignores
  matches of multiple dictionary values in the same key in a similar way
  as [`tokens_lookup()`](https://quanteda.io/reference/tokens_lookup.md)
  ([\#2159](https://github.com/quanteda/quanteda/issues/2159)).

### Changes and additions

- A new `split_tags` argument has been added to
  [`tokens()`](https://quanteda.io/reference/tokens.md), to provide the
  user with an option not to preserve social media tags (addresses
  [\#2156](https://github.com/quanteda/quanteda/issues/2156)).

## quanteda 3.2

### Bug fixes and stability enhancements

- [`dfm()`](https://quanteda.io/reference/dfm.md) returns a dfm with the
  identical column order even if
  [`tokens_compound()`](https://quanteda.io/reference/tokens_compound.md)
  or [`tokens_ngrams()`](https://quanteda.io/reference/tokens_ngrams.md)
  is used in the upstream
  ([\#2100](https://github.com/quanteda/quanteda/issues/2100)).
- [`dfm_group()`](https://quanteda.io/reference/dfm_group.md) with NA
  values in a grouping variable now drops those, similar to the
  behaviour of
  [`tokens_group()`](https://quanteda.io/reference/tokens_group.md) and
  [`corpus_group()`](https://quanteda.io/reference/corpus_group.md)
  ([\#2134](https://github.com/quanteda/quanteda/issues/2134)).

### Changes and additions

- [`char_wordstem()`](https://quanteda.io/reference/tokens_wordstem.md)
  now has a a new argument `check_whitespace`, which will not throw an
  error when lower-casing text containing a whitespace character.
- [`dfm_remove()`](https://quanteda.io/reference/dfm_select.md) now has
  a new argument `padding = FALSE` that when `TRUE`, collects counts of
  the removed features in the first column. This produces results
  consistent with what is compiled as a dfm built from tokens where some
  have been removed with `padding = TRUE`
  ([\#2152](https://github.com/quanteda/quanteda/issues/2152)).

## quanteda 3.1

### Bug fixes and stability enhancements

- Improved and more consistent handling of empty corpus, tokens and dfm
  objects, to address
  [\#2110](https://github.com/quanteda/quanteda/issues/2110).
- [`rbind.dfm()`](https://quanteda.io/reference/cbind.dfm.md) now
  preserves docvars
  ([\#2109](https://github.com/quanteda/quanteda/issues/2109)).
- Document name for Biden’s 2021 Inaugural Address in
  `data_corpus_inaugural` is now consistent with all other documents.
- Fix [\#2127](https://github.com/quanteda/quanteda/issues/2127) that
  caused subsetting to change document names.

### Changes and additions

- [`phrase()`](https://quanteda.io/reference/phrase.md) now has a
  `separator` argument
  ([\#2124](https://github.com/quanteda/quanteda/issues/2124)).

### Deprecations

- [`phrase()`](https://quanteda.io/reference/phrase.md) methods for
  tokens, collocations, and lists are deprecated in favour of
  [`as.phrase()`](https://quanteda.io/reference/phrase.md)
  ([\#2129](https://github.com/quanteda/quanteda/issues/2129)).

## quanteda 3.0

**quanteda** 3.0 is a major release that improves functionality,
completes the modularisation of the package begun in v2.0, further
improves function consistency by removing previously deprecated
functions, and enhances workflow stability and consistency by
deprecating some shortcut steps built into some functions.

### Changes and additions

- Modularisation: We have now separated the `textplot_*()` functions
  from the main package into a separate package **quanteda.textplots**,
  and the `textstat_*()` functions from the main package into a separate
  package **quanteda.textstats**. This completes the modularisation
  begun in v2 with the move of the `textmodel_*()` functions to the
  separate package **quanteda.textmodels**. **quanteda** now consists of
  core functions for textual data processing and management.

- The package dependency structure is now greatly reduced, by
  eliminating some unnecessary package dependencies, through
  modularisation, and by addressing complex downstream dependencies in
  packages such as **stopwords**. v3 should serve as a more lightweight
  and more consistent platform for other text analysis packages to build
  on.

- We have added non-standard evaluation for `by` and `groups` arguments
  to access object docvars:

  - The `*_sample()` functions’ argument `by`, and `groups` in the
    `*_group()` functions, now take unquoted document variable (docvar)
    names directly, similar to the way the `subset` argument works in
    the `*_subset()` functions.
  - Quoted docvar names no longer work, as these will be evaluated
    literally.
  - The `by = "document"` formerly sampled from `docid(x)`, but this
    functionality is now removed. Instead, use `by = docid(x)` to
    replicate this functionality.
  - For `groups`, the default is now `docid(x)`, which is now documented
    more completely. See
    [`?groups`](https://quanteda.io/reference/groups.md) and
    [`?docid`](https://quanteda.io/reference/docnames.md).

- [`dfm()`](https://quanteda.io/reference/dfm.md) has a new argument,
  `remove_padding`, for removing the “pads” left behind after removing
  tokens with `padding = TRUE`. (For other extensive changes to
  [`dfm()`](https://quanteda.io/reference/dfm.md), see “Deprecated”
  below.)

- [`tokens_group()`](https://quanteda.io/reference/tokens_group.md),
  formerly internal-only, is now exported.

- [`corpus_sample()`](https://quanteda.io/reference/corpus_sample.md),
  [`dfm_sample()`](https://quanteda.io/reference/dfm_sample.md), and
  [`tokens_sample()`](https://quanteda.io/reference/tokens_sample.md)
  now work consistently
  ([\#2023](https://github.com/quanteda/quanteda/issues/2023)).

- The [`kwic()`](https://quanteda.io/reference/kwic.md) return object
  structure has been redefined, and built with an option to use a new
  function [`index()`](https://quanteda.io/reference/index.md) that
  returns token spans following a pattern search.
  ([\#2045](https://github.com/quanteda/quanteda/issues/2045) and
  [\#2065](https://github.com/quanteda/quanteda/issues/2065))

- The punctuation regular expression and that for matching social media
  usernames has now been redefined so that the valid Twitter username
  `@_` is now counted as a “tag” rather than as “punctuation”.
  ([\#2049](https://github.com/quanteda/quanteda/issues/2049))

- The data object `data_corpus_inaugural` has been updated to include
  the Biden 2021 inaugural address.

- A new system of validators for input types now provides better
  argument type and value checking, with more consistent error messages
  for invalid types or values.

- Upon startup, we now message the console with the Unicode and ICU
  version information. Because we removed our redefinition of
  [`View()`](https://rdrr.io/r/utils/View.html) (see below), the former
  conflict warning is now gone.

- [`as.character.corpus()`](https://quanteda.io/reference/as.character.corpus.md)
  now has a `use.names = TRUE` argument, similar to
  [`as.character.tokens()`](https://quanteda.io/reference/as.tokens.md)
  (but with a different default value).

### Deprecations

The main potentially breaking changes in version 3 relate to the
deprecation or elimination of shortcut steps that allowed functions that
required tokens inputs to skip the tokens creation step. We did this to
require users to take more direct control of tokenization options, or to
substitute the alternative tokeniser of their choice (and then coercing
it to tokens via \[as.tokens()\]). This also allows our function
behaviour to be more consistent, with each function performing a single
task, rather than combining functions (such as tokenisation *and*
constructing a matrix).

The most common example involves constructing a dfm directly from a
character or corpus object. Formerly, this would construct a tokens
object internally before creating the dfm, and allowed passing arguments
to [`tokens()`](https://quanteda.io/reference/tokens.md) via `...`. This
is now deprecated, although still functional with a warning.

We strongly encourage either creating a tokens object first, or piping
the tokens return to [`dfm()`](https://quanteda.io/reference/dfm.md)
using `%>%`. (See examples below.)

We have also deprecated direct character or corpus inputs to \[kwic()\],
since this also requires a tokenised input.

The full listing of deprecations is:

- `dfm.character()` and `dfm.corpus()` are deprecated. Users should
  create a tokens object first, and input that to
  [`dfm()`](https://quanteda.io/reference/dfm.md).

- [`dfm()`](https://quanteda.io/reference/dfm.md): As of version 3, only
  tokens objects are supported as inputs to
  [`dfm()`](https://quanteda.io/reference/dfm.md). Calling
  [`dfm()`](https://quanteda.io/reference/dfm.md) for character or
  corpus objects is still functional, but issues a warning. Convenience
  passing of arguments to
  [`tokens()`](https://quanteda.io/reference/tokens.md) via `...` for
  [`dfm()`](https://quanteda.io/reference/dfm.md) is also deprecated,
  but undocumented, and functions only with a warning. Users should now
  create a tokens object (using
  [`tokens()`](https://quanteda.io/reference/tokens.md) from character
  or corpus inputs before calling
  [`dfm()`](https://quanteda.io/reference/dfm.md).

- [`kwic()`](https://quanteda.io/reference/kwic.md): As of version 3,
  only tokens objects are supported as inputs to
  [`kwic()`](https://quanteda.io/reference/kwic.md). Calling
  [`kwic()`](https://quanteda.io/reference/kwic.md) for character or
  corpus objects is still functional, but issues a warning. Passing
  arguments to [`tokens()`](https://quanteda.io/reference/tokens.md) via
  `...` in [`kwic()`](https://quanteda.io/reference/kwic.md) is now
  disabled. Users should now create a tokens object (using
  [`tokens()`](https://quanteda.io/reference/tokens.md) from character
  or corpus inputs before calling
  [`kwic()`](https://quanteda.io/reference/kwic.md).

- Shortcut arguments to [`dfm()`](https://quanteda.io/reference/dfm.md)
  are now deprecated. These are still active, with a warning, although
  they are no longer documented. These are:

  - `stem` – use
    [`tokens_wordstem()`](https://quanteda.io/reference/tokens_wordstem.md)
    or
    [`dfm_wordstem()`](https://quanteda.io/reference/tokens_wordstem.md)
    instead.
  - `select`, `remove` – use
    [`tokens_select()`](https://quanteda.io/reference/tokens_select.md)
    / [`dfm_select()`](https://quanteda.io/reference/dfm_select.md) or
    [`tokens_remove()`](https://quanteda.io/reference/tokens_select.md)
    / [`dfm_remove()`](https://quanteda.io/reference/dfm_select.md)
    instead.
  - `dictionary`, `thesaurus` – use
    [`tokens_lookup()`](https://quanteda.io/reference/tokens_lookup.md)
    or [`dfm_lookup()`](https://quanteda.io/reference/dfm_lookup.md)
    instead.
  - `valuetype`, `case_insensitive` – these are disabled; for the
    deprecated arguments that take these qualifiers, they are fixed to
    the defaults `"glob"` and `TRUE`.
  - `groups` – use
    [`tokens_group()`](https://quanteda.io/reference/tokens_group.md) or
    [`dfm_group()`](https://quanteda.io/reference/dfm_group.md) instead.

- [`texts()`](https://quanteda.io/reference/texts.md) and `texts<-` are
  deprecated.

  - Use
    [`as.character.corpus()`](https://quanteda.io/reference/as.character.corpus.md)
    to turn a corpus into a simple named character vector.
  - Use
    [`corpus_group()`](https://quanteda.io/reference/corpus_group.md)
    instead of `texts(x, groups = ...)` to aggregate texts by a grouping
    variable.
  - Use `[<-` instead of `texts()<-` for replacing texts in a corpus
    object.

### Removals

- See note above under “Changes” about the `textplot_*()` and
  `textstat_*()` functions.

- The following functions have been removed:

  - all methods for defunct `corpuszip` objects.
  - [`View()`](https://rdrr.io/r/utils/View.html) functions
  - `as.wfm()` and `as.DocumentTermMatrix()` (the same functionality is
    available via
    [`convert()`](https://quanteda.io/reference/convert.md))
  - `metadoc()` and `metacorpus()`
  - `corpus_trimsentences()` (replaced by
    [`corpus_trim()`](https://quanteda.io/reference/corpus_trim.md))
  - all of the `tortl` functions
  - all legacy functions related to the ancient “corpuszip” corpus
    variant.

- `dfm` objects can no longer be used as a `pattern` in
  [`dfm_select()`](https://quanteda.io/reference/dfm_select.md)
  (formerly deprecated).

- [`dfm_sample()`](https://quanteda.io/reference/dfm_sample.md):

  - no longer has a `margin` argument. Instead,
    [`dfm_sample()`](https://quanteda.io/reference/dfm_sample.md) now
    samples only on documents, the same as
    [`corpus_sample()`](https://quanteda.io/reference/corpus_sample.md)
    and
    [`tokens_sample()`](https://quanteda.io/reference/tokens_sample.md);
    and
  - no longer works with `by = "document"` – use `by = docid(x)`
    instead.

- `dictionary_edit()`, `char_edit()`, and `list_edit()` are removed.

- [`dfm_weight()`](https://quanteda.io/reference/dfm_weight.md) -
  formerly deprecated `"scheme"` options are now removed.

- [`tokens()`](https://quanteda.io/reference/tokens.md) - formerly
  deprecated options `remove_hyphens` and `remove_twitter` are now
  removed. (Use `split_hyphens` instead, and the default tokenizer
  always now preserves Twitter and other social media tags.)

- Special versions of [`head()`](https://rdrr.io/r/utils/head.html) and
  [`tail()`](https://rdrr.io/r/utils/head.html) for corpus, dfm, and fcm
  objects are now removed, since the base methods work fine for these
  objects. The main consequence was the removal of the `nf` option from
  the methods for dfm and fcm objects, which limited the number of
  features. This can be accomplished using the index operator `[`
  instead, or for printing, by specifying `print(x, max_nfeat = 6L)`
  (for instance).

### Bug fixes and stability enhancements

- Fixed a bug causing `topfeatures(x, group = something)` to fail with
  weighted dfms
  ([\#2032](https://github.com/quanteda/quanteda/issues/2032)).

- [`kwic()`](https://quanteda.io/reference/kwic.md) is more stable and
  does not crash when a vector is supplied as the `window` argument
  ([\#2008](https://github.com/quanteda/quanteda/issues/2008)).

- Allow use of multi-threading with more than two threads by fixing
  [`quanteda_options()`](https://quanteda.io/reference/quanteda_options.md).

- Mentions of the now-removed `ngrams` option in `dfm(x, ...)` has now
  been removed from the dfm documentation.
  ([\#1990](https://github.com/quanteda/quanteda/issues/1990))

- Handling for some early-cycle v2 dfm object is improved, to ensure
  that they are updated to the latest object format.
  ([\#2097](https://github.com/quanteda/quanteda/issues/2097))

## quanteda 2.1.2

CRAN release: 2020-09-23

### Changes

- [`textstat_keyness()`](https://quanteda.io/reference/textstat_keyness.html)
  performance is now improved through implementation in (multi-threaded)
  C++.

### Bug fixes and stability enhancements

- Fixes breaking tests and examples on Solaris platform as well as other
  changes introduced by changes to the stringi package.

## quanteda 2.1.1

CRAN release: 2020-07-27

### Changes

### Bug fixes and stability enhancements

- [`corpus_reshape()`](https://quanteda.io/reference/corpus_reshape.md)
  now allows reshaping back to documents even when segmented texts were
  of zero length.
  ([\#1978](https://github.com/quanteda/quanteda/issues/1978))
- Special handling applied for Solaris to some issues breaking on that
  build, relating to the cacheing in
  [`summary.corpus()`](https://quanteda.io/reference/summary.corpus.md)/[`textstat_summary()`](https://quanteda.io/reference/textstat_summary.html).

## quanteda 2.1.0

CRAN release: 2020-07-05

### Changes

- Added `block_size` to
  [`quanteda_options()`](https://quanteda.io/reference/quanteda_options.md)
  to control the number of documents in blocked tokenization.
- Fixed `print.dictionary2()` to control the printing of nested levels
  with `max_nkey`
  ([\#1967](https://github.com/quanteda/quanteda/issues/1967))
- Added
  [`textstat_summary()`](https://quanteda.io/reference/textstat_summary.html)
  to provide detailed information about dfm, tokens and corpus objects.
  It will replace [`summary()`](https://rdrr.io/r/base/summary.html) in
  future versions.
- Fixed a performance issue causing slowdowns in tokenizing (using the
  default `what = "word"`) corpora with large numbers of documents that
  contain social media tags and URLs that needed to be preserved (such a
  large corpus of Tweets).
- Updated the (default) “word” tokenizer to preserve hashtags and
  usernames better with non-ASCII text, and made these patterns
  user-configurable in
  [`quanteda_options()`](https://quanteda.io/reference/quanteda_options.md).
  The following are now preserved: “#政治” as well as Weibo-style
  hashtags such as “#英国首相#”.
- `convert(x, to = "data.frame")` now outputs the first column as
  “doc_id” rather than “document” since “document” is a commonly
  occurring term in many texts.
  ([\#1918](https://github.com/quanteda/quanteda/issues/1918))
- Added new methods
  [`char_select()`](https://quanteda.io/reference/char_select.md),
  [`char_keep()`](https://quanteda.io/reference/char_select.md), and
  [`char_remove()`](https://quanteda.io/reference/char_select.md) for
  easy manipulation of character vectors.
- Added `dictionary_edit()` for easy, interactive editing of
  dictionaries, plus the functions `char_edit()` and `list_edit()` for
  editing character and list of character objects.
- Added a method to
  [`textplot_wordcloud()`](https://rdrr.io/pkg/quanteda.textplots/man/textplot_wordcloud.html)
  that plots objects from
  [`textstat_keyness()`](https://quanteda.io/reference/textstat_keyness.html),
  to visualize keywords either by comparison or for the target category
  only.
- Improved the performance of
  [`kwic()`](https://quanteda.io/reference/kwic.md)
  ([\#1840](https://github.com/quanteda/quanteda/issues/1840)).
- Added new `logsmooth` scheme to
  [`dfm_weight()`](https://quanteda.io/reference/dfm_weight.md).
- Added new
  [`textstat_summary()`](https://quanteda.io/reference/textstat_summary.html)
  method, which returns summary information about the
  tokens/types/features etc in an object. It also caches summary
  information so that this can be retrieved on subsequent calls, rather
  than re-computed.

### Bug fixes and stability enhancements

- Stopped returning `NA` for non-existent features when `n` \>
  `nfeat(x)` in `textstat_frequency(x, n)`.
  ([\#1929](https://github.com/quanteda/quanteda/issues/1929))
- Fixed a problem in
  [`dfm_lookup()`](https://quanteda.io/reference/dfm_lookup.md) and
  [`tokens_lookup()`](https://quanteda.io/reference/tokens_lookup.md) in
  which an error was caused when no dictionary key returned a single
  match ([\#1946](https://github.com/quanteda/quanteda/issues/1946)).
- Fixed a bug that caused a `textstat_simil/dist` object converted to a
  data.frame to drop its `document2` labels
  ([\#1939](https://github.com/quanteda/quanteda/issues/1939)).
- Fixed a bug causing
  [`dfm_match()`](https://quanteda.io/reference/dfm_match.md) to fail on
  a dfm that included “pads” (`""`).
  ([\#1960](https://github.com/quanteda/quanteda/issues/1960))
- Updated the `data_dfm_lbgexample` object using more modern dfm
  internals.
- Updates
  [`textstat_readability()`](https://quanteda.io/reference/textstat_readability.html),
  [`textstat_lexdiv()`](https://quanteda.io/reference/textstat_lexdiv.html),
  and [`nscrabble()`](https://quanteda.io/reference/nscrabble.html) so
  that empty texts are not dropped in the result.
  ([\#1976](https://github.com/quanteda/quanteda/issues/1976))

## quanteda 2.0.1

CRAN release: 2020-03-18

### Changes

- Moved `data_corpus_irishbudget2010` and `data_corpus_dailnoconf1991`
  to the **quanteda.textmodels** package.
- Em dashes and double dashes between words, whether surrounded by a
  space or not, are now converted to ” - ” to distinguish them from
  infix hyphens.
  ([\#1889](https://github.com/quanteda/quanteda/issues/1889))
- Verbose output for dfm and tokens creation is now corrected and more
  consistent.
  ([\#1894](https://github.com/quanteda/quanteda/issues/1894))

### Bug fixes and stability enhancements

- Number removal is now both improved and fixed
  ([\#1909](https://github.com/quanteda/quanteda/issues/1909)).
- Fixed an issue causing CRAN errors in pre-v4, related to the new
  default of `stringsAsFactors = FALSE` for data.frame objects.
- An error in the print method for dfm objects is now fixed
  ([\#1897](https://github.com/quanteda/quanteda/issues/1897))
- Fixed a bug in
  [`tokens_replace()`](https://quanteda.io/reference/tokens_replace.md)
  when the pattern was not matched
  ([\#1895](https://github.com/quanteda/quanteda/issues/1895))
- Fixed the names of dimensions not exchanging when a dfm was transposed
  ([\#1903](https://github.com/quanteda/quanteda/issues/1903))

## quanteda 2.0

### Changes

**quanteda** 2.0 introduces some major changes, detailed here.

1.  New corpus object structure.

    The internals of the corpus object have been redesigned, and now are
    based around a character vector with meta- and system-data in
    attributes. These are all updated to work with the existing
    extractor and replacement functions. If you were using these before,
    then you should not even notice the change. Docvars are now handled
    separately from the texts, in the same way that docvars are handled
    for tokens objects.

2.  New metadata handling.

    Corpus-level metadata is now inserted in a user metadata list via
    [`meta()`](https://quanteda.io/reference/meta.md) and `meta<-()`.
    `metacorpus()` is kept as a synonym for
    [`meta()`](https://quanteda.io/reference/meta.md), for backwards
    compatibility. Additional system-level corpus information is also
    recorded, but automatically when an object is created.

    Document-level metadata is deprecated, and now all document-level
    information is simply a “docvar”. For backward compatibility,
    `metadoc()` is kept and will insert document variables (docvars)
    with the name prefixed by an underscore.

3.  Corpus objects now store default summary statistics for efficiency.
    When these are present,
    [`summary.corpus()`](https://quanteda.io/reference/summary.corpus.md)
    retrieves them rather than computing them on the fly.

4.  New index operators for core objects. The main change here is to
    redefine the `$` operator for corpus, tokens, and dfm objects (all
    objects that retain docvars) to allow this operator to access single
    docvars by name. Some other index operators have been redefined as
    well, such as `[.corpus` returning a slice of a corpus, and
    `[[.corpus` returning the texts from a corpus.

    See the full details at
    <https://github.com/quanteda/quanteda/wiki/indexing_core_objects>.

5.  `*_subset()` functions.

    The `subset` argument now must be logical, and the `select` argument
    has been removed. (This is part of
    [`base::subset()`](https://rdrr.io/r/base/subset.html) but has never
    made sense, either in **quanteda** or **base**.)

6.  Return format from
    [`textstat_simil()`](https://quanteda.io/reference/textstat_simil.html)
    and
    [`textstat_dist()`](https://quanteda.io/reference/textstat_simil.html).

    Now defaults to a sparse matrix from the **Matrix** package, but
    coercion methods are provided for
    [`as.data.frame()`](https://rdrr.io/r/base/as.data.frame.html), to
    make these functions return a data.frame just like the other
    textstat functions. Additional coercion methods are provided for
    [`as.dist()`](https://rdrr.io/r/stats/dist.html), `as.simil()`, and
    [`as.matrix()`](https://rdrr.io/r/base/matrix.html).

7.  settings functions (and related slots and object attributes) are
    gone. These are now replaced by a new `meta(x, type = "object")`
    that records object-specific meta-data, including settings such as
    the `n` for tokens (to record the `ngrams`).

8.  All included data objects are upgraded to the new formats. This
    includes the three corpus objects, the single dfm data object, and
    the LSD 2015 dictionary object.

9.  New print methods for core objects (corpus, tokens, dfm, dictionary)
    now exist, each with new global options to control the number of
    documents shown, as well as the length of a text snippet (corpus),
    the tokens (tokens), dfm cells (dfm), or keys and values
    (dictionary). Similar to the extended printing options for dfm
    objects, printing of corpus objects now allows for brief summaries
    of the texts to be printed, and for the number of documents and the
    length of the previews to be controlled by new global options.

10. All textmodels and related functions have been moved to a new
    package **quanteda.textmodels**. This makes them easier to maintain
    and update, and keeps the size of the core package down.

11. **quanteda** v2 implements major changes to the
    [`tokens()`](https://quanteda.io/reference/tokens.md) constructor.
    These are designed to simplify the code and its maintenance in
    **quanteda**, to allow users to work with other (external)
    tokenizers, and to improve consistency across the tokens processing
    options. Changes include:

    - A new method `tokens.list(x, ...)` constructs a `tokens` object
      from named list of characters, allowing users to tokenize texts
      using some other function (or package) such as `tokenize_words()`,
      `tokenize_sentences()`, or `tokenize_tweets()` from the
      **tokenizers** package, or the list returned by
      [`spacyr::spacy_tokenize()`](http://spacyr.quanteda.io/reference/spacy_tokenize.md).
      This allows users to use their choice of tokenizer, as long as it
      returns a named list of characters. With `tokens.list()`, all
      tokens processing (`remove_*`) options can be applied, or the list
      can be converted directly to a `tokens` object without processing
      using `as.tokens.list()`.

    - All tokens options are now *intervention* options, to split or
      remove things that by default are not split or removed. All
      `remove_*` options to
      [`tokens()`](https://quanteda.io/reference/tokens.md) now remove
      them from tokens objects by calling `tokens.tokens()`, after
      constructing the object. “Pre-processing” is now actually
      post-processing using `tokens_*()` methods internally, after a
      conservative tokenization on token boundaries. This both improves
      performance and improves consistency in handling special
      characters (e.g. Twitter characters) across different tokenizer
      engines.
      ([\#1503](https://github.com/quanteda/quanteda/issues/1503),
      [\#1446](https://github.com/quanteda/quanteda/issues/1446),
      [\#1801](https://github.com/quanteda/quanteda/issues/1801))

    Note that `tokens.tokens()` will remove what is found, but cannot
    “undo” a removal – for instance it cannot replace missing
    punctuation characters if these have already been removed.

    - The option `remove_hyphens` is removed and deprecated, but
      replaced by `split_hyphens`. This preserves infix (internal)
      hyphens rather than splitting them. This behaviour is implemented
      in both the `what = "word"` and `what = "word2"` tokenizer
      options. This option is `FALSE` by default.

    - The option `remove_twitter` has been removed. The new
      `what = "word"` is a smarter tokenizer that preserves social media
      tags, URLs, and email-addresses. “Tags” are defined as valid
      social media hashtags and usernames (using Twitter rules for
      validity) rather than removing the `#` and `@` punctuation
      characters, even if `remove_punct = TRUE`.

### New features

- Changed the default value of the `size` argument in
  [`dfm_sample()`](https://quanteda.io/reference/dfm_sample.md) to the
  number of features, not the number of documents.
  ([\#1643](https://github.com/quanteda/quanteda/issues/1643))
- Fixes a few CRAN-related issues (compiler warnings on Solaris and
  encoding warnings on r-devel-linux-x86_64-debian-clang.)
- Added `startpos` and `endpos` arguments to
  [`tokens_select()`](https://quanteda.io/reference/tokens_select.md),
  for selecting on token positions relative to the start or end of the
  tokens in each document.
  ([\#1475](https://github.com/quanteda/quanteda/issues/1475))
- Added a [`convert()`](https://quanteda.io/reference/convert.md) method
  for corpus objects, to convert them into data.frame or json formats.
- Added a `spacy_tokenize()` method for corpus objects, to provide
  direct access via the **spacyr** package.

### Behaviour changes

- Added a `force = TRUE` option and error checking for the situations of
  applying [`dfm_weight()`](https://quanteda.io/reference/dfm_weight.md)
  or [`dfm_group()`](https://quanteda.io/reference/dfm_group.md) to a
  dfm that has already been weighted.
  ([\#1545](https://github.com/quanteda/quanteda/issues/1545)) The
  function
  [`textstat_frequency()`](https://quanteda.io/reference/textstat_frequency.html)
  now allows passing this argument to
  [`dfm_group()`](https://quanteda.io/reference/dfm_group.md) via `...`.
  ([\#1646](https://github.com/quanteda/quanteda/issues/1646))
- [`textstat_frequency()`](https://quanteda.io/reference/textstat_frequency.html)
  now has a new argument for resolving ties when ranking term
  frequencies, defaulting to the “min” method.
  ([\#1634](https://github.com/quanteda/quanteda/issues/1634))
- New docvars accessor and replacement functions are available for
  corpus, tokens, and dfm objects via `$`. (See Index Operators for Core
  Objects above.)
- [`textstat_entropy()`](https://quanteda.io/reference/textstat_entropy.html)
  now produces a data.frame that is more consistent with other
  `textstat` methods.
  ([\#1690](https://github.com/quanteda/quanteda/issues/1690))

### Bug fixes and stability enhancements

- docnames now enforced to be character (formerly, could be numeric for
  some objects).
- docnames are now enforced to be strictly unique for all object
  classes.
- Grouping operations in
  [`tokens_group()`](https://quanteda.io/reference/tokens_group.md) and
  [`dfm_group()`](https://quanteda.io/reference/dfm_group.md) are more
  robust to using multiple grouping variables, and preserve these
  correctly as docvars in the new dfm.
  ([\#1809](https://github.com/quanteda/quanteda/issues/1809))
- Some fixes to documented … objects in two functions that were
  previously causing CRAN check failures on the release of 1.5.2.

### Other improvements

- All of the (three) included corpus objects have been cleaned up and
  augmented with improved meta-data and docvars. The inaugural speech
  corpus, for instance, now includes the President’s political party
  affiliation.

## quanteda 1.5.2

CRAN release: 2019-11-26

### New features

- Added Yule’s I to
  [`textstat_lexdiv()`](https://quanteda.io/reference/textstat_lexdiv.html).
- Added forward compatibility for newer (v2) corpus class objects.
- Added a new function
  [`featfreq()`](https://quanteda.io/reference/featfreq.md) to compute
  the overall feature frequencies from a dfm.

### Bug fixes

- Fixed a bug in
  [`tokens_lookup()`](https://quanteda.io/reference/tokens_lookup.md)
  when `exclusive = FALSE` and the tokens object has paddings.
  ([\#1743](https://github.com/quanteda/quanteda/issues/1743))
- Fixed a bug in
  [`tokens_replace()`](https://quanteda.io/reference/tokens_replace.md)
  ([\#1765](https://github.com/quanteda/quanteda/issues/1765)).

## quanteda 1.5.1

CRAN release: 2019-07-30

### New features

- Added `omit_empty` as an argument to
  [`convert()`](https://quanteda.io/reference/convert.md), to allow the
  user to control whether empty documents are excluded from converted
  dfm objects for certain formats.
  ([\#1660](https://github.com/quanteda/quanteda/issues/1660))

### Bug fixes and stability enhancements

- Fixed a bug that affects the new
  [`textstat_dist()`](https://quanteda.io/reference/textstat_simil.html)
  and
  [`textstat_simil()`](https://quanteda.io/reference/textstat_simil.html).
  ([\#1730](https://github.com/quanteda/quanteda/issues/1730))
- Fixed a bug in how
  [`textstat_dist()`](https://quanteda.io/reference/textstat_simil.html)
  and
  [`textstat_simil()`](https://quanteda.io/reference/textstat_simil.html)
  class symmetric matrices.

## quanteda 1.5.0

CRAN release: 2019-07-04

### New features

- Add `flatten` and `levels` arguments to `as.list.dictionary2()` to
  enable more flexible conversion of dictionary objects.
  ([\#1661](https://github.com/quanteda/quanteda/issues/1661))
- In
  [`corpus_sample()`](https://quanteda.io/reference/corpus_sample.md),
  the `size` now works with the `by` argument, to control the size of
  units sampled from each group.
- Improvements to
  [`textstat_dist()`](https://quanteda.io/reference/textstat_simil.html)
  and
  [`textstat_simil()`](https://quanteda.io/reference/textstat_simil.html),
  see below.
- Long tokens are not discarded automatically in the call to
  [`tokens()`](https://quanteda.io/reference/tokens.md).
  ([\#1713](https://github.com/quanteda/quanteda/issues/1713))

### Behaviour changes

- [`textstat_dist()`](https://quanteda.io/reference/textstat_simil.html)
  and
  [`textstat_simil()`](https://quanteda.io/reference/textstat_simil.html)
  now return sparse symmetric matrix objects using classes from the
  **Matrix** package. This replaces the former structure based on the
  `dist` class. Computation of these classes is now also based on the
  fast implementation in the **proxyC** package. When computing
  similarities, the new `min_simil` argument allows a user to ignore
  certain values below a specified similarity threshold. A new coercion
  method `as.data.frame.textstat_simildist()` now exists for converting
  these returns into a data.frame of pairwise comparisons. Existing
  methods such as [`as.matrix()`](https://rdrr.io/r/base/matrix.html),
  [`as.dist()`](https://rdrr.io/r/stats/dist.html), and
  [`as.list()`](https://rdrr.io/r/base/list.html) work as they did
  before.
- We have removed the “faith”, “chi-squared”, and “kullback” methods
  from
  [`textstat_dist()`](https://quanteda.io/reference/textstat_simil.html)
  and
  [`textstat_simil()`](https://quanteda.io/reference/textstat_simil.html)
  because these were either not symmetric or not invariant to document
  or feature ordering. Finally, the `selection` argument has been
  deprecated in favour of a new `y` argument.\
- [`textstat_readability()`](https://quanteda.io/reference/textstat_readability.html)
  now defaults to `measure = "Flesch"` if no measure is supplied. This
  makes it consistent with
  [`textstat_lexdiv()`](https://quanteda.io/reference/textstat_lexdiv.html)
  that also takes a default measure (“TTR”) if none is supplied.
  ([\#1715](https://github.com/quanteda/quanteda/issues/1715))
- The default values for `max_nchar` and `min_nchar` in
  [`tokens_select()`](https://quanteda.io/reference/tokens_select.md)
  are now NULL, meaning they are not applied if the user does not supply
  values. Fixes
  [\#1713](https://github.com/quanteda/quanteda/issues/1713).

### Bug fixes and stability enhancements

- `kwic.corpus()` and `kwic.tokens()` behaviour now aligned, meaning
  that dictionaries are correctly faceted by key instead of by value.
  ([\#1684](https://github.com/quanteda/quanteda/issues/1684))
- Improved formatting of
  [`tokens()`](https://quanteda.io/reference/tokens.md) verbose output.
  ([\#1683](https://github.com/quanteda/quanteda/issues/1683))
- Subsetting and printing of subsetted kwic objects is more robust.
  ([\#1665](https://github.com/quanteda/quanteda/issues/1665))
- The “Bormuth” and “DRP” measures are now fixed for
  [`textstat_readability()`](https://quanteda.io/reference/textstat_readability.html).
  ([\#1701](https://github.com/quanteda/quanteda/issues/1701))

## quanteda 1.4.1

CRAN release: 2019-02-26

### Bug fixes and stability enhancements

- Fixed an issue with special handling of whitespace variants that
  caused a test to fail when running Ubuntu 18.10 system with libicu-dev
  version 63.1
  ([\#1604](https://github.com/quanteda/quanteda/issues/1604)).
- Fixed the operation of `docvars<-.corpus()` in a way that solves
  [\#1603](https://github.com/quanteda/quanteda/issues/1603)
  (reassignment of docvar names).

## quanteda 1.4.0

CRAN release: 2019-01-30

### Bug fixes and stability enhancements

- Fixed bug in
  [`dfm_compress()`](https://quanteda.io/reference/dfm_compress.md) and
  [`dfm_group()`](https://quanteda.io/reference/dfm_group.md) that
  changed or deleted docvars attributes of dfm objects
  ([\#1506](https://github.com/quanteda/quanteda/issues/1506)).
- Fixed a bug in
  [`textplot_xray()`](https://rdrr.io/pkg/quanteda.textplots/man/textplot_xray.html)
  that caused incorrect facet labels when a pattern contained multiple
  list elements or values
  ([\#1514](https://github.com/quanteda/quanteda/issues/1514)).
- [`kwic()`](https://quanteda.io/reference/kwic.md) now correctly
  returns the pattern associated with each match as the `"keywords"`
  attribute, for all `pattern` types
  ([\#1515](https://github.com/quanteda/quanteda/issues/1515))
- Implemented some improvements in efficiency and computation of unusual
  edge cases for
  [`textstat_simil()`](https://quanteda.io/reference/textstat_simil.html)
  and
  [`textstat_dist()`](https://quanteda.io/reference/textstat_simil.html).

### New features

- [`textstat_lexdiv()`](https://quanteda.io/reference/textstat_lexdiv.html)
  now works on tokens objects, not just dfm objects. New methods of
  lexical diversity now include MATTR (the Moving-Average Type-Token
  Ratio, Covington & McFall 2010) and MSTTR (Mean Segmental Type-Token
  Ratio).
- New function
  [`tokens_split()`](https://quanteda.io/reference/tokens_split.md)
  allows splitting single into multiple tokens based on a pattern match.
  ([\#1500](https://github.com/quanteda/quanteda/issues/1500))
- New function
  [`tokens_chunk()`](https://quanteda.io/reference/tokens_chunk.md)
  allows splitting tokens into new documents of equally-sized “chunks”.
  ([\#1520](https://github.com/quanteda/quanteda/issues/1520))
- New function
  [`textstat_entropy()`](https://quanteda.io/reference/textstat_entropy.html)
  now computes entropy for a dfm across feature or document margins.
- The documentation for
  [`textstat_readability()`](https://quanteda.io/reference/textstat_readability.html)
  is vastly improved, now providing detailing all formulas and providing
  full references.
- New function
  [`dfm_match()`](https://quanteda.io/reference/dfm_match.md) allows a
  user to specify the features in a dfm according to a fixed vector of
  feature names, including those of another dfm. Replaces
  `dfm_select(x, pattern)` where `pattern` was a dfm.
- A new argument `vertex_labelsize` added to
  [`textplot_network()`](https://rdrr.io/pkg/quanteda.textplots/man/textplot_network.html)
  to allow more precise control of label sizes, either globally or
  individually.

### Behaviour changes

- `tokens.tokens(x, remove_hyphens = TRUE)` where `x` was generated with
  `remove_hyphens = FALSE` now behaves similarly to how the same tokens
  would be handled had this option been called on character input as
  `tokens.character(x, remove_hyphens = TRUE)`.
  ([\#1498](https://github.com/quanteda/quanteda/issues/1498))

## quanteda 1.3.14

CRAN release: 2018-11-19

### Bug fixes and stability enhancements

- Improved the robustness of
  [`textstat_keyness()`](https://quanteda.io/reference/textstat_keyness.html)
  ([\#1482](https://github.com/quanteda/quanteda/issues/1482)).
- Improved the accuracy of sparsity reporting for the print method of a
  dfm ([\#1473](https://github.com/quanteda/quanteda/issues/1473)).
- Diagonals on a
  [`textstat_simil()`](https://quanteda.io/reference/textstat_simil.html)
  return object coerced to matrix now default to 1.0, rather than 0.0
  ([\#1494](https://github.com/quanteda/quanteda/issues/1494)).

### New Features

- Added the following measures to
  [`textstat_lexdiv()`](https://quanteda.io/reference/textstat_lexdiv.html):
  Yule’s *K*, Simpson’s *D*, and Herdan’s *Vm*.

## quanteda 1.3.13

CRAN release: 2018-11-01

### Bug fixes and stability enhancements

- Fixed a bug causing incorrect counting in `fcm(x, ordered = TRUE)`.
  ([\#1413](https://github.com/quanteda/quanteda/issues/1413)) Also set
  the condition that `window` can be of size 1 (formerly the limit was 2
  or greater).
- Fixed deprecation warnings from adding a dfm as docvars, and this now
  imports the feature names as docvar names automatically. (related to
  [\#1417](https://github.com/quanteda/quanteda/issues/1417))
- Fixed behaviour from
  `tokens(x, what = "fasterword", remove_separators = TRUE)` so that it
  correctly splits words separated by `\n` and `\t` characters.
  ([\#1420](https://github.com/quanteda/quanteda/issues/1420))
- Add error checking for functions taking dfm inputs in case a dfm has
  empty features
  ([\#1419](https://github.com/quanteda/quanteda/issues/1419)).
- For
  [`textstat_readability()`](https://quanteda.io/reference/textstat_readability.html),
  fixed a bug in Dale-Chall-based measures and in the Spache word list
  measure. These were caused by an incorrect lookup mechanism but also
  by limited implementation of the wordlists. The new wordlists include
  all of the variations called for in the original measures, but using
  fast fixed matching.
  ([\#1410](https://github.com/quanteda/quanteda/issues/1410))
- Fixed problems with basic dfm operations
  ([`rowMeans()`](https://rdrr.io/r/base/colSums.html),
  [`rowSums()`](https://rdrr.io/r/base/colSums.html),
  [`colMeans()`](https://rdrr.io/r/base/colSums.html),
  [`colSums()`](https://rdrr.io/r/base/colSums.html)) caused by not
  having access to the **Matrix** package methods.
  ([\#1428](https://github.com/quanteda/quanteda/issues/1428))
- Fixed problem in
  [`textplot_scale1d()`](https://rdrr.io/pkg/quanteda.textplots/man/textplot_scale1d.html)
  when input a predicted wordscores object with `se.fit = TRUE`
  ([\#1440](https://github.com/quanteda/quanteda/issues/1440)).
- Improved the stability of
  [`textplot_network()`](https://rdrr.io/pkg/quanteda.textplots/man/textplot_network.html).
  ([\#1460](https://github.com/quanteda/quanteda/issues/1460))

### New Features

- Added new argument `intermediate` to
  `textstat_readability(x, measure, intermediate = FALSE)`, which if
  `TRUE` returns intermediate quantities used in the computation of
  readability statistics. Useful for verification or direct use of the
  intermediate quantities.
- Added a new `separator` argument to
  [`kwic()`](https://quanteda.io/reference/kwic.md) to allow a user to
  define which characters will be added between tokens returned from a
  keywords in context search.
  ([\#1449](https://github.com/quanteda/quanteda/issues/1449))
- Reimplemented
  [`textstat_dist()`](https://quanteda.io/reference/textstat_simil.html)
  and
  [`textstat_simil()`](https://quanteda.io/reference/textstat_simil.html)
  in C++ for enhanced performance.
  ([\#1210](https://github.com/quanteda/quanteda/issues/1210))
- Added a
  [`tokens_sample()`](https://quanteda.io/reference/tokens_sample.md)
  function ([\#1478](https://github.com/quanteda/quanteda/issues/1478)).

### Behaviour changes

- Removed the Hamming distance method from
  [`textstat_dist()`](https://quanteda.io/reference/textstat_simil.html)
  ([\#1443](https://github.com/quanteda/quanteda/issues/1443)), based on
  the reasoning in
  [\#1442](https://github.com/quanteda/quanteda/issues/1442).
- Removed the “chisquared” and “chisquared2” distance measures from
  [`textstat_simil()`](https://quanteda.io/reference/textstat_simil.html).
  ([\#1442](https://github.com/quanteda/quanteda/issues/1442))

## quanteda 1.3.4

CRAN release: 2018-07-15

### Bug fixes and stability enhancements

- Keep encodings of types when a tokens object is recompiled.
  ([\#1387](https://github.com/quanteda/quanteda/issues/1387))
- More robust handling in `predict.textmodel_worscores()` when training
  and test feature sets are difference
  ([\#1380](https://github.com/quanteda/quanteda/issues/1380)).\
- [`char_segment()`](https://quanteda.io/reference/corpus_segment.md)
  and
  [`corpus_segment()`](https://quanteda.io/reference/corpus_segment.md)
  are more robust to whitespace characters preceding a pattern
  ([\#1394](https://github.com/quanteda/quanteda/issues/1394)).\
- [`tokens_ngrams()`](https://quanteda.io/reference/tokens_ngrams.md) is
  more robust to handling large numbers of documents
  ([\#1395](https://github.com/quanteda/quanteda/issues/1395)).\
- [`corpus.data.frame()`](https://quanteda.io/reference/corpus.md) is
  now robust to handling data.frame inputs with improper or missing
  variable names
  ([\#1388](https://github.com/quanteda/quanteda/issues/1388)).

### New Features

- Added `as.igraph.fcm()` method for converting an fcm object into an
  **igraph** graph object.
- Added a `case_insensitive` argument to
  [`char_segment()`](https://quanteda.io/reference/corpus_segment.md)
  and
  [`corpus_segment()`](https://quanteda.io/reference/corpus_segment.md).

## quanteda 1.3.0

CRAN release: 2018-06-05

### New Features

- Added `to = "tripletlist"` output type for
  [`convert()`](https://quanteda.io/reference/convert.md), to convert a
  dfm into a simple triplet list.
  ([\#1321](https://github.com/quanteda/quanteda/issues/1321))
- Added `tokens_tortl()` and `char_tortl()` to add markers for
  right-to-left language tokens and character objects.
  ([\#1322](https://github.com/quanteda/quanteda/issues/1322))

### Behaviour changes

- Improved [`corpus.kwic()`](https://quanteda.io/reference/corpus.md) by
  adding new arguments `split_context` and `extract_keyword`.
- `dfm_remove(x, selection = anydfm)` is now equivalent to
  `dfm_remove(x, selection = featnames(anydfm))`.
  ([\#1320](https://github.com/quanteda/quanteda/issues/1320))
- Improved consistency of `predict.textmodel_nb()` returns, and added
  `type =` argument.
  ([\#1329](https://github.com/quanteda/quanteda/issues/1329))

### Bug fixes

- Fixed a bug in
  [`textmodel_affinity()`](https://rdrr.io/pkg/quanteda.textmodels/man/textmodel_affinity.html)
  that caused failure when the input dfm had been compiled with
  `tolower = FALSE`.
  ([\#1338](https://github.com/quanteda/quanteda/issues/1338))
- Fixed a bug affecting
  [`tokens_lookup()`](https://quanteda.io/reference/tokens_lookup.md)
  and [`dfm_lookup()`](https://quanteda.io/reference/dfm_lookup.md) when
  `nomatch` is used.
  ([\#1347](https://github.com/quanteda/quanteda/issues/1347))
- Fixed a problem whereby NA texts created a “document” (or tokens)
  containing `"NA"`
  ([\#1372](https://github.com/quanteda/quanteda/issues/1372))

## quanteda 1.2.0

CRAN release: 2018-04-15

### New Features

- Added an [`nsentence()`](https://quanteda.io/reference/nsentence.md)
  method for **spacyr** parsed objects.
  ([\#1289](https://github.com/quanteda/quanteda/issues/1289))

### Bug fixes and stability enhancements

- Fix bug in `nsyllable()` that incorrectly handled cased words, and
  returned wrong names with `use.names = TRUE`.
  ([\#1282](https://github.com/quanteda/quanteda/issues/1282))
- Fix the overwriting of `summary.character()` caused by previous import
  of the **network** package namespace.
  ([\#1285](https://github.com/quanteda/quanteda/issues/1285))
- [`dfm_smooth()`](https://quanteda.io/reference/dfm_weight.md) now
  correctly sets the smooth value in the dfm
  ([\#1274](https://github.com/quanteda/quanteda/issues/1274)).
  Arithmetic operations on dfm objects are now much more consistent and
  do not drop attributes of the dfm, as sometimes happened with earlier
  versions.

### Behaviour changes

- [`tokens_toupper()`](https://quanteda.io/reference/tokens_tolower.md)
  and
  [`tokens_tolower()`](https://quanteda.io/reference/tokens_tolower.md)
  no longer remove unused token types. Solves
  [\#1278](https://github.com/quanteda/quanteda/issues/1278).
- [`dfm_trim()`](https://quanteda.io/reference/dfm_trim.md) now takes
  more options, and these are implemented more consistently.
  `min_termfreq` and `max_termfreq` have replaced `min_count` and
  `max_count`, and these can be modified using a `termfreq_type`
  argument. (Similar options are implemented for `docfreq_type`.) Solves
  [\#1253](https://github.com/quanteda/quanteda/issues/1253),
  [\#1254](https://github.com/quanteda/quanteda/issues/1254).
- [`textstat_simil()`](https://quanteda.io/reference/textstat_simil.html)
  and
  [`textstat_dist()`](https://quanteda.io/reference/textstat_simil.html)
  now take valid dfm indexes for the relevant margin for the `selection`
  argument. Previously, this could also be a direct vector or matrix for
  comparison, but this is no longer allowed. Solves
  [\#1266](https://github.com/quanteda/quanteda/issues/1266).
- Improved performance for
  [`dfm_group()`](https://quanteda.io/reference/dfm_group.md)
  ([\#1295](https://github.com/quanteda/quanteda/issues/1295)).

## quanteda 1.1.1

CRAN release: 2018-03-07

### New Features

- Added [`as.dfm()`](https://quanteda.io/reference/as.dfm.md) methods
  for **tm** `DocumentTermMatrix` and `TermDocumentMatrix` objects.
  ([\#1222](https://github.com/quanteda/quanteda/issues/1222))
- `predict.textmodel_wordscores()` now includes an `include_reftexts`
  argument to exclude training texts from the predicted model object
  ([\#1229](https://github.com/quanteda/quanteda/issues/1229)). The
  default behaviour is `include_reftexts = TRUE`, producing the same
  behaviour as existed before the introduction of this argument. This
  allows rescaling based on the reference documents (since rescaling
  requires prediction on the reference documents) but provides an easy
  way to exclude the reference documents from the predicted quantities.
- [`textplot_wordcloud()`](https://rdrr.io/pkg/quanteda.textplots/man/textplot_wordcloud.html)
  now uses code entirely internal to **quanteda**, instead of using the
  **wordcloud** package.

### Bug fixes and stability enhancements

- Fixed a problem in the examples for
  [`textplot_scale1d()`](https://rdrr.io/pkg/quanteda.textplots/man/textplot_scale1d.html)
  by adjusting the refscores for `data_corpus_irishbudget2010`.
- Eliminated unnecessary dependency on the **digest** package.
- Updated the vignette title to be less generic.
- Improved the robustness of
  [`dfm_trim()`](https://quanteda.io/reference/dfm_trim.md) and
  [`dfm_weight()`](https://quanteda.io/reference/dfm_weight.md) for
  previously weighted dfm objects and when supplied thresholds are
  proportions instead of counts.
  ([\#1237](https://github.com/quanteda/quanteda/issues/1237))
- Fixed a problem in `summary.corpus(x, n = 101)` when `ndoc(x) > 100`
  ([\#1242](https://github.com/quanteda/quanteda/issues/1242)).
- Fixed a problem in `predict.textmodel_wordscores(x, rescaling = "mv")`
  that always reset the reference values for rescaling to the first and
  second documents
  ([\#1251](https://github.com/quanteda/quanteda/issues/1251)).
- Issues in the color generation and labels for
  [`textplot_keyness()`](https://rdrr.io/pkg/quanteda.textplots/man/textplot_keyness.html)
  are now resolved
  ([\#1233](https://github.com/quanteda/quanteda/issues/1233),
  [\#1233](https://github.com/quanteda/quanteda/issues/1233)).

### Performance improvements

- textmodel methods are now exported, to facilitate extension packages
  for other textmodel methods (e.g. wordshoal).

### Behaviour changes

- Changed the default in
  [`textmodel_wordfish()`](https://rdrr.io/pkg/quanteda.textmodels/man/textmodel_wordfish.html)
  to `sparse = FALSE`, in response to
  [\#1216](https://github.com/quanteda/quanteda/issues/1216).
- [`dfm_group()`](https://quanteda.io/reference/dfm_group.md) now
  preserves docvars that are constant for the group aggregation
  ([\#1228](https://github.com/quanteda/quanteda/issues/1228)).
- The default threads is now 2, to comply with CRAN policies. (The user
  can increase this via `quanteda_options(threads = ...)`.

## quanteda 1.0.0

CRAN release: 2018-01-28

### New Features

- Added `vertex_labelfont` to
  [`textplot_network()`](https://rdrr.io/pkg/quanteda.textplots/man/textplot_network.html).
- Added
  [`textmodel_lsa()`](https://rdrr.io/pkg/quanteda.textmodels/man/textmodel_lsa.html)
  for Latent Semantic Analysis models.\
- Added
  [`textmodel_affinity()`](https://rdrr.io/pkg/quanteda.textmodels/man/textmodel_affinity.html)
  for the Perry and Benoit (2017) class affinity scaling model.
- Added Chinese stopwords.
- Added a pkgdown vignette for applications in the Chinese language.
- Added
  [`textplot_network()`](https://rdrr.io/pkg/quanteda.textplots/man/textplot_network.html)
  function.
- The [`stopwords()`](https://rdrr.io/pkg/stopwords/man/stopwords.html)
  function and the associated internal data object `data_char_stopwords`
  have been removed from **quanteda**, and replaced by equivalent
  functionality in the **stopwords** package.
- Added
  [`tokens_subset()`](https://quanteda.io/reference/tokens_subset.md),
  now consistent with other `*_subset()` functions
  ([\#1149](https://github.com/quanteda/quanteda/issues/1149)).

### Bug fixes and stability enhancements

- Performance has been improved for
  [`fcm()`](https://quanteda.io/reference/fcm.md) and for
  [`textmodel_wordfish()`](https://rdrr.io/pkg/quanteda.textmodels/man/textmodel_wordfish.html).
- [`dfm()`](https://quanteda.io/reference/dfm.md) now correctly passes
  through all `...` arguments to
  [`tokens()`](https://quanteda.io/reference/tokens.md).
  ([\#1121](https://github.com/quanteda/quanteda/issues/1121))
- All `dfm_*()` functions now work correctly with empty dfm objects.
  ([\#1133](https://github.com/quanteda/quanteda/issues/1133))
- Fixed a bug in
  [`dfm_weight()`](https://quanteda.io/reference/dfm_weight.md) for
  named weight vectors
  ([\#1150](https://github.com/quanteda/quanteda/issues/1150))
- Fixed a bug preventing
  [`textplot_influence()`](https://rdrr.io/pkg/quanteda.textmodels/man/textplot_influence.html)
  from working
  ([\#1116](https://github.com/quanteda/quanteda/issues/1116)).

### Behaviour Changes

- The convenience wrappers to
  [`convert()`](https://quanteda.io/reference/convert.md) are simplified
  and no longer exported. To convert a dfm,
  [`convert()`](https://quanteda.io/reference/convert.md) is now the
  only official function.
- [`nfeat()`](https://quanteda.io/reference/ndoc.md) replaces
  `nfeature()`, which is now deprecated.
  ([\#1134](https://github.com/quanteda/quanteda/issues/1134))
- `textmodel_wordshoal()` has been removed, and relocated to a new
  package (**wordshoal**).
- The generic wrapper function `textmodel()`, which used to be a gateway
  to specific `textmodel_*()` functions, has been removed.
- (Most of) the `textmodel_*()` have been reimplemented to make their
  behaviour consistent with the `lm/glm()` families of models, including
  especially how the `predict`, `summary`, and `coef` methods work
  ([\#1007](https://github.com/quanteda/quanteda/issues/1007),
  [\#108](https://github.com/quanteda/quanteda/issues/108)).
- The GitHub home for the repository has been moved to
  <https://github.com/quanteda/quanteda>.

## quanteda 0.99.12

CRAN release: 2017-10-06

### New Features

- [`tokens_segment()`](https://quanteda.io/reference/tokens_segment.md)
  has a new `window` argument, permitting selection within an asymmetric
  window around the `pattern` of selection.
  ([\#521](https://github.com/quanteda/quanteda/issues/521))
- [`tokens_replace()`](https://quanteda.io/reference/tokens_replace.md)
  now allows token types to be substituted directly and quickly.
- [`textmodel_affinity()`](https://rdrr.io/pkg/quanteda.textmodels/man/textmodel_affinity.html)
  now adds functionality to fit the Perry and Benoit (2017) class
  affinity model.
- Added a `spacy_parse` method for corpus objects. Also restored
  quanteda methods for **spacyr** `spacy_parsed` objects.

### Bug fixes and stability enhancements

- Improved documentation for
  [`textmodel_nb()`](https://rdrr.io/pkg/quanteda.textmodels/man/textmodel_nb.html)
  ([\#1010](https://github.com/quanteda/quanteda/issues/1010)), and made
  output quantities from the fitted NB model regular matrix objects
  instead of **Matrix** classes.

### Behaviour Changes

- All of the deprecated functions are now removed.
  ([\#991](https://github.com/quanteda/quanteda/issues/991))
- [`tokens_group()`](https://quanteda.io/reference/tokens_group.md) is
  now significantly faster.
- The deprecated “list of characters”
  [`tokenize()`](https://quanteda.io/reference/tokenize_internal.md)
  function and all methods associated with the `tokenizedTexts` object
  types have been removed.
- Added convenience functions for keeping tokens or features:
  [`tokens_keep()`](https://quanteda.io/reference/tokens_select.md),
  [`dfm_keep()`](https://quanteda.io/reference/dfm_select.md), and
  [`fcm_keep()`](https://quanteda.io/reference/dfm_select.md).
  ([\#1037](https://github.com/quanteda/quanteda/issues/1037))
- `textmodel_NB()` has been replaced by
  [`textmodel_nb()`](https://rdrr.io/pkg/quanteda.textmodels/man/textmodel_nb.html).

## quanteda 0.99.9

CRAN release: 2017-09-22

### New Features

- Added methods for changing the docnames of tokens and dfm objects
  ([\#987](https://github.com/quanteda/quanteda/issues/987)).
- Added new function
  [`textmodel_lsa()`](https://rdrr.io/pkg/quanteda.textmodels/man/textmodel_lsa.html)
  for Latent Semantic Analysis.

### Bug fixes and stability enhancements

- The computation of tfidf has been more thoroughly described in the
  documentation for this function
  ([\#997](https://github.com/quanteda/quanteda/issues/997)).
- Fixed a bug discovered in
  [\#1011](https://github.com/quanteda/quanteda/issues/1011) for unused
  keys in `tokens_lookup(..., exclusive = FALSE)`.

## quanteda 0.99

CRAN release: 2017-08-15

### New Features

- Added
  [`tokens_segment()`](https://quanteda.io/reference/tokens_segment.md),
  which works on tokens objects in the same way as
  [`corpus_segment()`](https://quanteda.io/reference/corpus_segment.md)
  does on corpus objects
  ([\#902](https://github.com/quanteda/quanteda/issues/902)).
- Added **magrittr** pipe support
  ([\#927](https://github.com/quanteda/quanteda/issues/927)). `%>%` can
  now be used with **quanteda** without needing to attach **magrittr**
  (or, as many users apparently believe, the entire tidyverse.)\
- [`corpus_segment()`](https://quanteda.io/reference/corpus_segment.md)
  now behaves more logically and flexibly, and is clearly differentiated
  from
  [`corpus_reshape()`](https://quanteda.io/reference/corpus_reshape.md)
  in terms of its functionality. Its documentation is also vastly
  improved. ([\#908](https://github.com/quanteda/quanteda/issues/908))
- Added `data_dictionary_LSD2015`, the Lexicoder Sentiment 2015
  dictionary ([\#963](https://github.com/quanteda/quanteda/issues/963)).
- Significant improvements to the performance of
  [`tokens_lookup()`](https://quanteda.io/reference/tokens_lookup.md)
  and [`dfm_lookup()`](https://quanteda.io/reference/dfm_lookup.md)
  ([\#960](https://github.com/quanteda/quanteda/issues/960)).
- New functions `head.corpus()`, `tail.corpus()` provide fast subsetting
  of the first or last documents in a corpus.
  ([\#952](https://github.com/quanteda/quanteda/issues/952))

### Bug fixes and stability enhancements

- Fixed a problem when applying
  [`purrr::map()`](https://purrr.tidyverse.org/reference/map.html) to
  [`dfm()`](https://quanteda.io/reference/dfm.md)
  ([\#928](https://github.com/quanteda/quanteda/issues/928)).
- Added documentation for `regex2fixed()` and associated functions.
- Fixed a bug in `textstat_collocations.tokens()` caused by “documents”
  containing only `""` as tokens.
  ([\#940](https://github.com/quanteda/quanteda/issues/940))
- Fixed a bug caused by
  [`cbind.dfm()`](https://quanteda.io/reference/cbind.dfm.md) when
  features shared a name starting with
  `quanteda_options("base_featname")`
  ([\#946](https://github.com/quanteda/quanteda/issues/946))
- Improved dictionary handling and creation now correctly handles nested
  LIWC 2015 categories.
  ([\#941](https://github.com/quanteda/quanteda/issues/941))
- Number of threads now set correctly by
  [`quanteda_options()`](https://quanteda.io/reference/quanteda_options.md).
  ([\#966](https://github.com/quanteda/quanteda/issues/966))

### Behaviour changes

- [`summary.corpus()`](https://quanteda.io/reference/summary.corpus.md)
  now generates a special data.frame, which has its own print method,
  rather than requiring `verbose = FALSE` to suppress output
  ([\#926](https://github.com/quanteda/quanteda/issues/926)).
- [`textstat_collocations()`](https://quanteda.io/reference/textstat_collocations.html)
  is now multi-threaded.
- [`head.dfm()`](https://quanteda.io/reference/head.dfm.md),
  [`tail.dfm()`](https://quanteda.io/reference/head.dfm.md) now behave
  consistently with base R methods for matrix, with the added argument
  `nfeature`. Previously, these methods printed the subset and invisibly
  returned it. Now, they simply return the subset.
  ([\#952](https://github.com/quanteda/quanteda/issues/952))
- Dictionary keys are now unique, and if multiple, identical keys are
  defined for a dictionary when constructed, the values will be merged
  into the consolidated key.
  ([\#959](https://github.com/quanteda/quanteda/issues/959))

## quanteda 0.9.9-65

CRAN release: 2017-05-26

### New features

- Improvements and consolidation of methods for detecting multi-word
  expressions, now active only through
  [`textstat_collocations()`](https://quanteda.io/reference/textstat_collocations.html),
  which computes only the `lambda` method for now, but does so
  accurately and efficiently.
  ([\#753](https://github.com/quanteda/quanteda/issues/753),
  [\#803](https://github.com/quanteda/quanteda/issues/803)). This
  function is still under development and likely to change further.
- Added new `quanteda_options` that affect the maximum documents and
  features displayed by the dfm print method
  ([\#756](https://github.com/quanteda/quanteda/issues/756)).
- `ngram` formation is now significantly faster, including with skips
  (skipgrams).
- Improvements to
  [`topfeatures()`](https://quanteda.io/reference/topfeatures.md):
  - now accepts a `groups` argument that can be used to generate lists
    of top (or bottom) features in a group of texts, including by
    document ([\#336](https://github.com/quanteda/quanteda/issues/336)).
  - new argument `scheme` that takes the default of (frequency)
    `"count"` but also a new `"docfreq"` value
    ([\#408](https://github.com/quanteda/quanteda/issues/408)).
- New wrapper [`phrase()`](https://quanteda.io/reference/phrase.md)
  converts whitespace-separated multi-word patterns into a list of
  patterns. This affects the feature/pattern matching in
  `tokens/dfm_select/remove`, `tokens_compound`, `tokens/dfm_lookup`,
  and `kwic`. [`phrase()`](https://quanteda.io/reference/phrase.md) and
  the associated changes also make the behaviour of using character
  vectors, lists of characters, dictionaries, and collocation objects
  for pattern matches far more consistent. (See
  [\#820](https://github.com/quanteda/quanteda/issues/820),
  [\#787](https://github.com/quanteda/quanteda/issues/787),
  [\#740](https://github.com/quanteda/quanteda/issues/740),
  [\#837](https://github.com/quanteda/quanteda/issues/837),
  [\#836](https://github.com/quanteda/quanteda/issues/836),
  [\#838](https://github.com/quanteda/quanteda/issues/838))
- [`corpus.Corpus()`](https://quanteda.io/reference/corpus.md) for
  creating a corpus from a **tm** Corpus now works with more complex
  objects that include document-level variables, such as data from the
  **manifestoR** package
  ([\#849](https://github.com/quanteda/quanteda/issues/849)).
- New plot function
  [`textplot_keyness()`](https://rdrr.io/pkg/quanteda.textplots/man/textplot_keyness.html)
  plots term “keyness”, the association of words with contrasting
  classes as measured by
  [`textstat_keyness()`](https://quanteda.io/reference/textstat_keyness.html).
- Added corpus constructor for corpus objects
  ([\#690](https://github.com/quanteda/quanteda/issues/690)).
- Added dictionary constructor for dictionary objects
  ([\#690](https://github.com/quanteda/quanteda/issues/690)).
- Added a tokens constructor for tokens objects
  ([\#690](https://github.com/quanteda/quanteda/issues/690)), including
  updates to [`tokens()`](https://quanteda.io/reference/tokens.md) that
  improve the consistency and efficiency of the tokenization.
- Added new
  [`quanteda_options()`](https://quanteda.io/reference/quanteda_options.md):
  `language_stemmer` and `language_stopwords`, now used for default in
  `*_wordstem` functions and
  [`stopwords()`](https://rdrr.io/pkg/stopwords/man/stopwords.html) for
  defaults, respectively. Also uses this option in
  [`dfm()`](https://quanteda.io/reference/dfm.md) when `stem = TRUE`,
  rather than hard-wiring in the “english” stemmer
  ([\#386](https://github.com/quanteda/quanteda/issues/386)).
- Added a new function
  [`textstat_frequency()`](https://quanteda.io/reference/textstat_frequency.html)
  to compile feature frequencies, possibly by groups.
  ([\#825](https://github.com/quanteda/quanteda/issues/825))
- Added `nomatch` option to
  [`tokens_lookup()`](https://quanteda.io/reference/tokens_lookup.md)
  and [`dfm_lookup()`](https://quanteda.io/reference/dfm_lookup.md), to
  provide tokens or feature counts for categories not matched to any
  dictionary key.
  ([\#496](https://github.com/quanteda/quanteda/issues/496))

### Behaviour changes

- The functions `sequences()` and `collocations()` have been removed and
  replaced by
  [`textstat_collocations()`](https://quanteda.io/reference/textstat_collocations.html).
- (Finally) we added “will” to the list of English stopwords
  ([\#818](https://github.com/quanteda/quanteda/issues/818)).
- `dfm` objects with one or both dimensions having zero length, and
  empty `kwic` objects now display more appropriately in their print
  methods (per
  [\#811](https://github.com/quanteda/quanteda/issues/811)).
- Pattern matches are now implemented more consistently across
  functions. In functions such as `*_select`, `*_remove`,
  `tokens_compound`, `features` has been replaced by `pattern`, and in
  `kwic`, `keywords` has been replaced by `pattern`. These all behave
  consistently with respect to `pattern`, which now has a unified single
  help page and parameter
  description.([\#839](https://github.com/quanteda/quanteda/issues/839))
  See also above new features related to
  [`phrase()`](https://quanteda.io/reference/phrase.md).
- We have improved the performance of the C++ routines that handle many
  of the `tokens_*` functions using hashed tokens, making some of them
  10x faster ([\#853](https://github.com/quanteda/quanteda/issues/853)).
- Upgrades to the
  [`dfm_group()`](https://quanteda.io/reference/dfm_group.md) function
  now allow “empty” documents to be created using the `fill = TRUE`
  option, for making documents conform to a selection (similar to how
  [`dfm_select()`](https://quanteda.io/reference/dfm_select.md) works
  for features, when supplied a dfm as the pattern argument). The
  `groups` argument now behaves consistently across the functions where
  it is used. ([\#854](https://github.com/quanteda/quanteda/issues/854))
- [`dictionary()`](https://quanteda.io/reference/dictionary.md) now
  requires its main argument to be a list, not a series of elements that
  can be used to build a list.
- Some changes to the behaviour of
  [`tokens()`](https://quanteda.io/reference/tokens.md) have improved
  the behaviour of `remove_hyphens = FALSE`, which now behaves more
  correctly regardless of the setting of `remove_punct`
  ([\#887](https://github.com/quanteda/quanteda/issues/887)).
- Improved [`cbind.dfm()`](https://quanteda.io/reference/cbind.dfm.md)
  function allows cbinding vectors, matrixes, and (recyclable) scalars
  to dfm objects.

### Bug fixes and stability enhancements

- For the underlying methods behind
  [`textstat_collocations()`](https://quanteda.io/reference/textstat_collocations.html),
  we corrected the word matching, and lambda and z calculation methods,
  which were slightly incorrect before. We also removed the chi2, G2,
  and pmi statistics, because these were incorrectly calculated for size
  \> 2.\
- LIWC-formatted dictionary import now robust to assignment to term
  assignment to missing categories.
- `textmodel_NB(x, y, distribution = "Bernoulli")` was previously
  inactive even when this option was set. It has now been fully
  implemented and tested
  ([\#776](https://github.com/quanteda/quanteda/issues/776),
  [\#780](https://github.com/quanteda/quanteda/issues/780)).
- Separators including rare spacing characters are now handled more
  robustly by the `remove_separators` argument in
  [`tokens()`](https://quanteda.io/reference/tokens.md). See
  [\#796](https://github.com/quanteda/quanteda/issues/796).
- Improved memory usage when computing
  [`ntoken()`](https://quanteda.io/reference/ntoken.md) and
  [`ntype()`](https://quanteda.io/reference/ntoken.md).
  ([\#795](https://github.com/quanteda/quanteda/issues/795))
- Improvements to
  [`quanteda_options()`](https://quanteda.io/reference/quanteda_options.md)
  now does not throw an error when **quanteda** functions are called
  directly without attaching the package. In addition, **quanteda**
  options can be set now in .Rprofile and will not be overwritten when
  the options initialization takes place when attaching the package.
- Fixed a bug in
  [`textstat_readability()`](https://quanteda.io/reference/textstat_readability.html)
  that wrongly computed the number of words with fewer than 3 syllables
  in a text; this affected the `FOG.NRI` and the `Linsear.Write`
  measures only.
- Fixed mistakes in the computation of two docfreq schemes: `"logave"`
  and `"inverseprob"`.
- Fixed a bug in the handling of multi-thread options where the settings
  using
  [`quanteda_options()`](https://quanteda.io/reference/quanteda_options.md)
  did not actually set the number of threads. In addition, we fixed a
  bug causing threading to be turned off on macOS (due to a check for a
  gcc version that is not used for compiling the macOS binaries)
  prevented multi-threading from being used at all on that platform.
- Fixed a bug causing failure when functions that use
  [`quanteda_options()`](https://quanteda.io/reference/quanteda_options.md)
  are called without the namespace or package being attached or loaded
  ([\#864](https://github.com/quanteda/quanteda/issues/864)).
- Fixed a bug in overloading the View method that caused all named
  objects in the RStudio/Source pane to be named “x”.
  ([\#893](https://github.com/quanteda/quanteda/issues/893))

## quanteda 0.9.9-50

CRAN release: 2017-04-20

### New features

- Corpus construction using
  [`corpus()`](https://quanteda.io/reference/corpus.md) now works for a
  [`tm::SimpleCorpus`](https://rdrr.io/pkg/tm/man/SimpleCorpus.html)
  object. ([\#680](https://github.com/quanteda/quanteda/issues/680))
- Added [`corpus_trim()`](https://quanteda.io/reference/corpus_trim.md)
  and [`char_trim()`](https://quanteda.io/reference/corpus_trim.md)
  functions for selecting documents or subsets of documents based on
  sentence, paragraph, or document lengths.
- Conversion of a dfm to an stm object now passes docvars through in the
  `$meta` of the return object.
- New `dfm_group(x, groups = )` command, a convenience wrapper around
  `dfm.dfm(x, groups = )`
  ([\#725](https://github.com/quanteda/quanteda/issues/725)).
- Methods for extending quanteda functions to readtext objects updated
  to match CRAN release of readtext package.
- Corpus constructor methods for data.frame objects now conform to the
  “text interchange format” for corpus data.frames, automatically
  recognizing `doc_id` and `text` fields, which also provides
  interoperability with the **readtext** package. corpus construction
  methods are now more explicitly tailored to input object classes.

### Bug fixes and stability enhancements

- [`dfm_lookup()`](https://quanteda.io/reference/dfm_lookup.md) behaves
  more robustly on different platforms, especially for keys whose values
  match no features
  ([\#704](https://github.com/quanteda/quanteda/issues/704)).
- [`textstat_simil()`](https://quanteda.io/reference/textstat_simil.html)
  and
  [`textstat_dist()`](https://quanteda.io/reference/textstat_simil.html)
  no longer take the `n` argument, as this was not sorting features in
  correct order.
- Fixed failure of `tokens(x, what = "character")` when `x` included
  Twitter characters `@` and `#`
  ([\#637](https://github.com/quanteda/quanteda/issues/637)).
- Fixed bug [\#707](https://github.com/quanteda/quanteda/issues/707)
  where `ntype.dfm()` produced an incorrect result.
- Fixed bug [\#706](https://github.com/quanteda/quanteda/issues/706)
  where
  [`textstat_readability()`](https://quanteda.io/reference/textstat_readability.html)
  and
  [`textstat_lexdiv()`](https://quanteda.io/reference/textstat_lexdiv.html)
  for single-document returns when `drop = TRUE`.
- Improved the robustness of
  [`corpus_reshape()`](https://quanteda.io/reference/corpus_reshape.md).
- `print`, and `head`, and `tail` methods for `dfm` are more robust
  ([\#684](https://github.com/quanteda/quanteda/issues/684)).
- Fixed bug in `convert(x, to = "stm")` caused by zero-count documents
  and zero-count features in a dfm
  ([\#699](https://github.com/quanteda/quanteda/issues/699),
  [\#700](https://github.com/quanteda/quanteda/issues/700),
  [\#701](https://github.com/quanteda/quanteda/issues/701)). This also
  removes docvar rows from `$meta` when this is passed through the dfm,
  for zero-count documents.
- Corrected broken handling of nested Yoshikoder dictionaries in
  [`dictionary()`](https://quanteda.io/reference/dictionary.md).
  ([\#722](https://github.com/quanteda/quanteda/issues/722))
- `dfm_compress` now preserves a dfm’s docvars if collapsing only on the
  features margin, which means that
  [`dfm_tolower()`](https://quanteda.io/reference/dfm_tolower.md) and
  [`dfm_toupper()`](https://quanteda.io/reference/dfm_tolower.md) no
  longer remove the docvars.
- [`fcm_compress()`](https://quanteda.io/reference/dfm_compress.md) now
  retains the fcm class, and generates and error when an asymmetric
  compression is attempted
  ([\#728](https://github.com/quanteda/quanteda/issues/728)).
- [`textstat_collocations()`](https://quanteda.io/reference/textstat_collocations.html)
  now returns the collocations as character, not as a factor
  ([\#736](https://github.com/quanteda/quanteda/issues/736))
- Fixed a bug in `dfm_lookup(x, exclusive = FALSE)` wherein an empty dfm
  ws returned with there was no no match
  ([\#116](https://github.com/quanteda/quanteda/issues/116)).
- Argument passing through
  [`dfm()`](https://quanteda.io/reference/dfm.md) to
  [`tokens()`](https://quanteda.io/reference/tokens.md) is now robust,
  and preserves variables defined in the calling environment
  ([\#721](https://github.com/quanteda/quanteda/issues/721)).
- Fixed issues related to dictionaries failing when applying
  [`str()`](https://rdrr.io/r/utils/str.html),
  [`names()`](https://rdrr.io/r/base/names.html), or other indexing
  operations, which started happening on Linux and Windows platforms
  following the CRAN move to 3.4.0.
  ([\#744](https://github.com/quanteda/quanteda/issues/744))
- Dictionary import using the LIWC format is more robust to improperly
  formatted input files
  ([\#685](https://github.com/quanteda/quanteda/issues/685)).
- Weights applied using
  [`dfm_weight()`](https://quanteda.io/reference/dfm_weight.md) now
  print friendlier error messages when the weight vector contains
  features not found in the dfm. See [this Stack Overflow
  question](https://stackoverflow.com/q/44132313) for the use case that
  sparked this improvement.

## quanteda 0.9.9-24

CRAN release: 2017-02-13

### New features

- [`corpus_reshape()`](https://quanteda.io/reference/corpus_reshape.md)
  can now go from sentences and paragraph units back to documents.
- Added a `by =` argument to
  [`corpus_sample()`](https://quanteda.io/reference/corpus_sample.md),
  for use in bootstrap resampling of sub-document units.
- Added an experimental method
  [`bootstrap_dfm()`](https://quanteda.io/reference/bootstrap_dfm.md) to
  generate a list of dimensionally-equivalent dfm objects based on
  sentence-level resampling of the original documents.
- Added option to [`tokens()`](https://quanteda.io/reference/tokens.md)
  and [`dfm()`](https://quanteda.io/reference/dfm.md) for passing
  docvars through to to tokens and dfm objects, and added
  [`docvars()`](https://quanteda.io/reference/docvars.md) and
  `metadoc()` methods for tokens and dfm class objects. Overall, the
  code for docvars and metadoc is now more robust and consistent.\
- [`docvars()`](https://quanteda.io/reference/docvars.md) on eligible
  objects that contain no docvars now returns an empty 0 x 0 data.frame
  (in the spirit of
  [\#242](https://github.com/quanteda/quanteda/issues/242)).
- Redesigned `textmodel_scale1d` now produces sorted and grouped
  document positions for fitted wordfish models, and produces a ggplot2
  plot object.
- [`textmodel_wordfish()`](https://rdrr.io/pkg/quanteda.textmodels/man/textmodel_wordfish.html)
  now preserves sparsity while processing the dfm, and uses a fast
  approximation to an SVD to get starting values. This also dramatically
  improves performance in computing this model.
  ([\#482](https://github.com/quanteda/quanteda/issues/482),
  [\#124](https://github.com/quanteda/quanteda/issues/124))
- The speed of [`kwic()`](https://quanteda.io/reference/kwic.md) is now
  dramatically improved, and also returns an indexed set of tokens that
  makes subsequent commands on a kwic class object much faster.
  ([\#603](https://github.com/quanteda/quanteda/issues/603))
- Package options (for verbose, threads) can now be set or queried using
  [`quanteda_options()`](https://quanteda.io/reference/quanteda_options.md).
- Improved performance and better documentation for
  [`corpus_segment()`](https://quanteda.io/reference/corpus_segment.md).
  ([\#634](https://github.com/quanteda/quanteda/issues/634))
- Added functions `corpus_trimsentences()` and `char_trimsentences()` to
  remove sentences from a corpus or character object, based on token
  length or pattern matching.
- Added options to
  [`textstat_readability()`](https://quanteda.io/reference/textstat_readability.html):
  `min_sentence_length` and `max_sentence_length`.
  ([\#632](https://github.com/quanteda/quanteda/issues/632))
- Indexing now works for dictionaries, for slicing out keys and values
  (`[`), or accessing values directly (`[[`).
  ([\#651](https://github.com/quanteda/quanteda/issues/651))
- Began the consolidation of collocation detection and scoring into a
  new function
  [`textstat_collocations()`](https://quanteda.io/reference/textstat_collocations.html),
  which combines the existing `collocations()` and `sequences()`
  functions. ([\#434](https://github.com/quanteda/quanteda/issues/434))
  Collocations now behave as sequences for other functions (such as
  [`tokens_compound()`](https://quanteda.io/reference/tokens_compound.md))
  and have a greatly improved performance for such uses.

### Behaviour changes

- [`docvars()`](https://quanteda.io/reference/docvars.md) now permits
  direct access to “metadoc” fields (starting with `_`,
  e.g. `_document`)
- `metadoc()` now returns a vector instead of a data.frame for a single
  variable, similar to
  [`docvars()`](https://quanteda.io/reference/docvars.md)
- Most `verbose` options now take the default from
  `getOption("verbose")` rather than fixing the value in the function
  signatures. ([\#577](https://github.com/quanteda/quanteda/issues/577))
- [`textstat_dist()`](https://quanteda.io/reference/textstat_simil.html)
  and
  [`textstat_simil()`](https://quanteda.io/reference/textstat_simil.html)
  now return a matrix if a `selection` argument is supplied, and
  coercion to a list produces a list of distances or similarities only
  for that selection.
- All remaining camelCase arguments are gone. For commonly used ones,
  such as those in
  [`tokens()`](https://quanteda.io/reference/tokens.md), the old
  arguments (e.g. `removePunct`) still produce the same behaviour but
  with a deprecation warning.
- Added `n_target` and `n_reference` columns to
  [`textstat_keyness()`](https://quanteda.io/reference/textstat_keyness.html)
  to return counts for each category being compared for keyness.

### Bug fixes

- Fixed an problem in tokens generation for some irregular characters
  ([\#554](https://github.com/quanteda/quanteda/issues/554)).
- Fixed a problem in setting the parallel thread size on single-core
  machines ([\#556](https://github.com/quanteda/quanteda/issues/556)).
- Fixed problems for [`str()`](https://rdrr.io/r/utils/str.html) on a
  corpus with no docvars
  ([\#571](https://github.com/quanteda/quanteda/issues/571)).
- `removeURL` in [`tokens()`](https://quanteda.io/reference/tokens.md)
  now removes URLs where the first part of the URL is a single letter
  ([\#587](https://github.com/quanteda/quanteda/issues/587)).
- `dfm_select` now works correctly for ngram features
  ([\#589](https://github.com/quanteda/quanteda/issues/589)).
- Fixed a bug crashing corpus constructors for character vectors with
  duplicated names (the cause of
  [\#580](https://github.com/quanteda/quanteda/issues/580)).
- Fixed a bug in the behaviour for `dfm_select(x, features)` when
  `features` was a dfm, that failed to produce the intended featnames
  matches for the output dfm.
- Fixed a bug in `corpus_segment(x, what = "tags")` when a document
  contained a whitespace just before a tag, at the beginning of the
  file, or ended with a tag followed by no text
  ([\#618](https://github.com/quanteda/quanteda/issues/618),
  [\#634](https://github.com/quanteda/quanteda/issues/634)).
- Fixed some problems with dictionary construction and reading some
  dictionary formats
  ([\#454](https://github.com/quanteda/quanteda/issues/454),
  [\#455](https://github.com/quanteda/quanteda/issues/455),
  [\#459](https://github.com/quanteda/quanteda/issues/459)).

## quanteda 0.9.9-17

CRAN release: 2017-01-27

### New features

- [`textstat_keyness()`](https://quanteda.io/reference/textstat_keyness.html)
  now returns a data.frame with p-values as well as the test statistic,
  and rownames containing the feature. This is more consistent with the
  other textstat functions.
- [`tokens_lookup()`](https://quanteda.io/reference/tokens_lookup.md)
  implements new rules for nested and linked sequences in dictionary
  values. See [\#502](https://github.com/quanteda/quanteda/issues/502).
- [`tokens_compound()`](https://quanteda.io/reference/tokens_compound.md)
  has a new `join` argument for better handling of nested and linked
  sequences. See
  [\#517](https://github.com/quanteda/quanteda/issues/517).
- Internal operations on `tokens` are now significantly faster due to a
  reimplementation of the hash table functions in C++.
  ([\#510](https://github.com/quanteda/quanteda/issues/510))
- [`dfm()`](https://quanteda.io/reference/dfm.md) now works with
  multi-word dictionaries and thesauruses, which previously worked only
  with
  [`tokens_lookup()`](https://quanteda.io/reference/tokens_lookup.md).
- [`fcm()`](https://quanteda.io/reference/fcm.md) is now parallelized
  for improved performance on multi-core systems.

### Bug fixes

- Fixed C++ incompatibilities on older platforms due to compiler
  incompatibilities with the required TBB libraries (for
  multi-threading)
  ([\#531](https://github.com/quanteda/quanteda/issues/531),
  [\#532](https://github.com/quanteda/quanteda/issues/532),
  [\#535](https://github.com/quanteda/quanteda/issues/535)), in addition
  to safeguarding against other compiler warnings across a variety of
  new tested undefined behaviours.\
- Fixed a bug in `convert(x, to = "lsa")` that transposed row and column
  names ([\#526](https://github.com/quanteda/quanteda/issues/526))
- Added missing [`fcm()`](https://quanteda.io/reference/fcm.md) method
  for corpus objects
  ([\#538](https://github.com/quanteda/quanteda/issues/538))
- Fixed some minor issues with reading in Lexicoder format dictionaries
  (Improvements to Lexicoder dictionary handling

### quanteda 0.9.9-3

### Bug fixes

- Fixed a bug causing `dfm` and `tokens` to break on \> 10,000
  documents. ([\#438](https://github.com/quanteda/quanteda/issues/438))
- Fixed a bug in
  `tokens(x, what = "character", removeSeparators = TRUE)` that returned
  an empty string.\
- Fixed a bug in `corpus.VCorpus` if the VCorpus contains a single
  document. ([\#445](https://github.com/quanteda/quanteda/issues/445))
- Fixed a bug in `dfm_compress` in which the function failed on
  documents that contained zero feature counts.
  ([\#467](https://github.com/quanteda/quanteda/issues/467))
- Fixed a bug in `textmodel_NB` that caused the class priors `Pc` to be
  refactored alphabetically instead of in the order of assignment
  ([\#471](https://github.com/quanteda/quanteda/issues/471)), also
  affecting predicted classes
  ([\#476](https://github.com/quanteda/quanteda/issues/476)).

### New features

- New textstat function
  [`textstat_keyness()`](https://quanteda.io/reference/textstat_keyness.html)
  discovers words that occur at differential rates between partitions of
  a dfm (using chi-squared, Fisher’s exact test, and the G^2 likelihood
  ratio test to measure the strength of associations).\
- Added 2017-Trump to the inaugural corpus datasets
  (`data_corpus_inaugual` and `data_char_inaugural`).\
- Improved the `groups` argument in
  [`texts()`](https://quanteda.io/reference/texts.md) (and in
  [`dfm()`](https://quanteda.io/reference/dfm.md) that uses this
  function), which will now coerce to a factor rather than requiring
  one.
- Added a dfm constructor from dfm objects, with the option of
  collapsing by groups.
- Added new arguments to `sequences()`: `ordered` and `max_length`, the
  latter to prevent memory leaks from extremely long sequences.\
- [`dictionary()`](https://quanteda.io/reference/dictionary.md) now
  accepts YAML as an input file format.
- `dfm_lookup` and `tokens_lookup` now accept a `levels` argument to
  determine which level of a hierarchical dictionary should be applied.
- Added `min_nchar` and `max_nchar` arguments to `dfm_select`.\
- [`dictionary()`](https://quanteda.io/reference/dictionary.md) can now
  be called on the argument of a
  [`list()`](https://rdrr.io/r/base/list.html) without explicitly
  wrapping it in [`list()`](https://rdrr.io/r/base/list.html).\
- `fcm` now works directly on a dfm object when `context = "documents"`.

This release has some **major changes to the API**, described below.

### Data objects

#### Renamed data objects

| new name               | original name   | notes |
|:-----------------------|:----------------|:------|
| `data_char_sampletext` | `exampleString` |       |
| `data_char_mobydick`   | `mobydickText`  |       |
| `data_dfm_LBGexample`  | `LBGexample`    |       |
| `data_char_sampletext` | `exampleString` |       |

#### Renamed internal data objects

The following objects have been renamed, but will not affect user-level
functionality because they are primarily internal. Their man pages have
been moved to a common ?`data-internal` man page, hidden from the index,
but linked from some of the functions that use them.

| new name | original name | notes |
|:---|:---|:---|
| `data_int_syllables` | `englishSyllables` | (used by `textcount_syllables()`) |
| `data_char_wordlists` | `wordlists` | (used by `readability()`) |
| `data_char_stopwords` | `.stopwords` | (used by [`stopwords()`](https://rdrr.io/pkg/stopwords/man/stopwords.html) |

#### Deprecated data objects

In v.0.9.9 the old names remain available, but are deprecated.

| new name                      | original name  | notes |
|:------------------------------|:---------------|:------|
| `data_char_ukimmig2010`       | `ukimmigTexts` |       |
| `data_corpus_irishbudget2010` | `ie2010Corpus` |       |
| `data_char_inaugural`         | `inaugTexts`   |       |
| `data_corpus_inaugural`       | `inaugCorpus`  |       |

### Deprecated functions

The following functions will still work, but issue a deprecation
warning:

| new function | deprecated function | constructs: |
|:---|:---|:---|
| `tokens` | [`tokenize()`](https://quanteda.io/reference/tokenize_internal.md) | `tokens` class object |
| `corpus_subset` | `subset.corpus` | `corpus` class object |
| `corpus_reshape` | `changeunits` | `corpus` class object |
| `corpus_sample` | `sample` | `corpus` class object |
| `corpus_segment` | `segment` | `corpus` class object |
| `dfm_compress` | `compress` | `dfm` class object |
| `dfm_lookup` | `applyDictionary` | `dfm` class object |
| `dfm_remove` | `removeFeatures.dfm` | `dfm` class object |
| `dfm_sample` | `sample.dfm` | `dfm` class object |
| `dfm_select` | `selectFeatures.dfm` | `dfm` class object |
| `dfm_smooth` | `smoother` | `dfm` class object |
| `dfm_sort` | `sort.dfm` | `dfm` class object |
| `dfm_trim` | `trim.dfm` | `dfm` class object |
| `dfm_weight` | `weight` | `dfm` class object |
| `textplot_wordcloud` | `plot.dfm` | (plot) |
| `textplot_xray` | `plot.kwic` | (plot) |
| `textstat_readability` | `readability` | `data.frame` |
| `textstat_lexdiv` | `lexdiv` | `data.frame` |
| `textstat_simil` | `similarity` | `dist` |
| `textstat_dist` | `similarity` | `dist` |
| `featnames` | `features` | `character` |
| `nsyllable` | `syllables` | (named) `integer` |
| `nscrabble` | `scrabble` | (named) `integer` |
| `tokens_ngrams` | `ngrams` | `tokens` class object |
| `tokens_skipgrams` | `skipgrams` | `tokens` class object |
| `tokens_toupper` | `toUpper.tokens`, `toUpper.tokenizedTexts` | `tokens`, `tokenizedTexts` |
| `tokens_tolower` | `toLower.tokens`, `toLower.tokenizedTexts` | `tokens`, `tokenizedTexts` |
| `char_toupper` | `toUpper.character`, `toUpper.character` | `character` |
| `char_tolower` | `toLower.character`, `toLower.character` | `character` |
| `tokens_compound` | `joinTokens`, `phrasetotoken` | `tokens` class object |

### New functions

The following are new to v0.9.9 (and not associated with deprecated
functions):

| new function | description | output class |
|:---|:---|:---|
| [`fcm()`](https://quanteda.io/reference/fcm.md) | constructor for a feature co-occurrence matrix | `fcm` |
| `fcm_select` | selects features from an `fcm` | `fcm` |
| `fcm_remove` | removes features from an `fcm` | `fcm` |
| `fcm_sort` | sorts an `fcm` in alphabetical order of its features | `fcm` |
| `fcm_compress` | compacts an `fcm` | `fcm` |
| `fcm_tolower` | lowercases the features of an `fcm` and compacts | `fcm` |
| `fcm_toupper` | uppercases the features of an `fcm` and compacts | `fcm` |
| `dfm_tolower` | lowercases the features of a `dfm` and compacts | `dfm` |
| `dfm_toupper` | uppercases the features of a `dfm` and compacts | `dfm` |
| `sequences` | experimental collocation detection | `sequences` |

### Deleted functions and data objects

| new name | reason |
|:---|:---|
| `encodedTextFiles.zip` | moved to the [**readtext**](https://github.com/quanteda/readtext) package |
| `describeTexts` | deprecated several versions ago for `summary.character` |
| `textfile` | moved to package [**readtext**](https://github.com/quanteda/readtext) |
| `encodedTexts` | moved to package [**readtext**](https://github.com/quanteda/readtext), as `data_char_encodedtexts` |
| `findSequences` | replaced by `sequences` |

### Other new features

- `to = "lsa"` functionality added to
  [`convert()`](https://quanteda.io/reference/convert.md)
  ([\#414](https://github.com/quanteda/quanteda/issues/414))\
- Much faster pattern matching in general, through an overhaul of how
  `valuetype` matches work for many functions.\
- Added experimental `View` methods for `kwic` objects, based on
  Javascript Datatables.\
- `kwic` is completely rewritten, now uses fast hashed index matching in
  C++ and fully implements vectorized matches
  ([\#306](https://github.com/quanteda/quanteda/issues/306)) and all
  `valuetype`s
  ([\#307](https://github.com/quanteda/quanteda/issues/307)).
- `tokens_lookup`, `tokens_select`, and `tokens_remove` are faster and
  use parallelization (based on the TBB library).
- `textstat_dist` and `textstat_simil` add fast, sparse, and parallel
  computation of many new distance and similarity matrices.\
- Added `textmodel_wordshoal` fitting function.
- Add `max_docfreq` and `min_docfreq` arguments, and better verbose
  output, to `dfm_trim`
  ([\#383](https://github.com/quanteda/quanteda/issues/383)).
- Added support for batch hashing of tokens through
  [`tokens()`](https://quanteda.io/reference/tokens.md), for more
  memory-efficient token hashing when dealing with very large numbers of
  documents.\
- Added support for in-memory compressed corpus objects.
- Consolidated corpus-level metadata arguments in
  [`corpus()`](https://quanteda.io/reference/corpus.md) through the
  `metacorpus` list argument.\
- Added Greek stopwords. (See
  [\#282](https://github.com/quanteda/quanteda/issues/282)).\
- Added index handling `[`, `[[`, and `$` for (hashed) `tokens`
  objects.\
- Now using ggplot2.
- Added tokens methods for `collocations()` and
  [`kwic()`](https://quanteda.io/reference/kwic.md).
- Much improved performance for
  [`tokens_select()`](https://quanteda.io/reference/tokens_select.md)
  (formerly `selectFeatures.tokens()`).
- Improved `ngrams()` and `joinTokens()` performance for hashed `tokens`
  class objects.
- Improved `dfm.character()` by using new
  [`tokens()`](https://quanteda.io/reference/tokens.md) constructor to
  create hashed tokenized texts by default when creating a dfm,
  resulting in performance gains when constructing a dfm. Creating a dfm
  from a hashed `tokens` object is now 4-5 times faster than the older
  `tokenizedTexts` object.
- Added new (hashed) `tokens` class object.
- Added plot method for fitted `textmodel_wordscores objects`.\
- Added fast
  [`tokens_lookup()`](https://quanteda.io/reference/tokens_lookup.md)
  method (formerly `applyDictionary()`), that also works with
  dictionaries that have multi-word keys. Addresses but does not
  entirely yet solve
  [\#188](https://github.com/quanteda/quanteda/issues/188).
- Added [`sparsity()`](https://quanteda.io/reference/sparsity.md)
  function to compute the sparsity of a dfm.
- Added feature co-occurrence matrix functions (`fcm`).

## quanteda 0.9.8

CRAN release: 2016-07-28

### New Features

- Improved the performance of `selectFeatures.tokenizedTexts()`.\
- Improved the performance of
  [`rbind.dfm()`](https://quanteda.io/reference/cbind.dfm.md).\
- Added support for different docvars when importing multiple files
  using `textfile()`.
  ([\#147](https://github.com/quanteda/quanteda/issues/147))\
- Added support for comparison dispersion plots in `plot.kwic()`.
  ([\#146](https://github.com/quanteda/quanteda/issues/146))\
- Added a corpus constructor method for kwic objects.\
- Substantially improved the performance of `convert(x, to = "stm")` for
  dfm export, including adding an argument for meta-data (docvars, in
  quanteda parlance).
  ([\#209](https://github.com/quanteda/quanteda/issues/209))\
- Internal rewrite of `textfile()`, now supports more file types, more
  wildcard patterns, and is far more robust generally.\
- Add support for loading external dictionary formats:
- Yoshikoder,
- Lexicoder v2 and v3
  ([\#228](https://github.com/quanteda/quanteda/issues/228))
- Autodetect dictionary file format from file extension, so no longer
  require `format` keyword for loading dictionaries
  ([\#227](https://github.com/quanteda/quanteda/issues/227))
- Improved compatibility with rOpenSci guidelines
  ([\#218](https://github.com/quanteda/quanteda/issues/218)):
  - Use httr to get remote files
  - Use [`messages()`](https://quanteda.io/reference/messages.md) to
    display messages rather than `print` or `cat`
  - Reorganise sections in README file\
- Added new `punctuation` argument to `collocations()` to provide new
  options for handling collocations separated by punctuation characters
  ([\#220](https://github.com/quanteda/quanteda/issues/220)).

### Bug fixes

- (0.9.8.7) Solved
  [\#267](https://github.com/quanteda/quanteda/issues/267) in which
  `fcm(x, tri = TRUE)` temporarily created a dense logical matrix.
- (0.9.8.7) Added feature co-occurrence matrix functions (`fcm`).
- (0.9.8.5) Fixed an incompatibility in sequences.cpp with Solaris x86
  ([\#257](https://github.com/quanteda/quanteda/issues/257))
- (0.9.8.4) Fix bug in verbose output of dfm that causes misreporting of
  number of features
  ([\#250](https://github.com/quanteda/quanteda/issues/250))
- (0.9.8.4) Fix a bug in `selectFeatures.dfm()` that ignored
  `case_insensitive = TRUE` settings
  ([\#251](https://github.com/quanteda/quanteda/issues/251)) correct the
  documentation for this function.
- (0.9.8.3) Fix a bug in `tf(x, scheme = "propmax")` that returned a
  wrong computation; correct the documentation for this function.
- (0.9.8.2) Fixed a bug in textfile() causing all texts to have the same
  name, for types using the “textField” argument (a single file
  containing multiple documents).\
- Fixed bug in `phrasetotoken()` where if pattern included a `+` for
  `valuetype = c("glob", "fixed")` it threw a regex error.
  [\#239](https://github.com/quanteda/quanteda/issues/239)\
- Fixed bug in `textfile()` where source is a remote .zip set.
  ([\#172](https://github.com/quanteda/quanteda/issues/172))\
- Fixed bug in `wordstem.dfm()` that caused an error if supplied a dfm
  with a feature whose total frequency count was zero, or with a feature
  whose total docfreq was zero. Fixes
  [\#181](https://github.com/quanteda/quanteda/issues/181).\
- Fix [\#214](https://github.com/quanteda/quanteda/issues/214)
  “mysterious stemmed token” bug in `wordstem.dfm()`, introduced in
  fixing [\#181](https://github.com/quanteda/quanteda/issues/181).\
- Fixed previously non-functional `toLower =` argument in
  `dfm.tokenizedTexts()`.
- Fixed some errors in the computation of a few readability formulas
  ([\#215](https://github.com/quanteda/quanteda/issues/215)).
- Added filenames names to text vectors returned by `textfile`
  ([\#221](https://github.com/quanteda/quanteda/issues/221)).
- [`dictionary()`](https://quanteda.io/reference/dictionary.md) now
  works correctly when reading LIWC dictionaries where all terms belong
  to one key ([\#229](https://github.com/quanteda/quanteda/issues/229)).
- \`convert(x, to = “stm”) now indexes the dfm components from 1, not 0
  ([\#222](https://github.com/quanteda/quanteda/issues/222)).
- Remove temporary stemmed token
  ([\#214](https://github.com/quanteda/quanteda/issues/214)).
- Fixed bug in textmodel_NB() for non-“uniform” priors
  ([\#241](https://github.com/quanteda/quanteda/issues/241))

### Changes

- Added `warn = FALSE` to the
  [`readLines()`](https://rdrr.io/r/base/readLines.html) calls in
  `textfile()`, so that no warnings are issued when files are read that
  are missing a final EOL or that contain embedded nuls.
- `trim()` now prints an output message even when no features are
  removed ([\#223](https://github.com/quanteda/quanteda/issues/223))
- We now skip some platform-dependent tests on CRAN, travis-ci and
  Windows.

## quanteda 0.9.6

- Improved Naive Bayes model and prediction,
  `textmodel(x, y, method = "NB")`, now works correctly on k \> 2.

- Improved tag handling for segment(x, what = “tags”)

- Added valuetype argument to segment() methods, which allows faster and
  more robust segmentation on large texts.

- corpus() now converts all hyphen-like characters to simple hyphen

- segment.corpus() now preserves all existing docvars.

- corpus documentation now removes the description of the corpus
  object’s structure since too many users were accessing these internal
  elements directly, which is strongly discouraged, as we are likely to
  change the corpus internals (soon and often). Repeat after me:
  “encapsulation”.

- Improve robustness of `corpus.VCorpus()` for constructing a corpus
  from a tm Corpus object.

- Add UTF-8 preservation to ngrams.cpp.

- Fix encoding issues for textfile(), improve functionality.

- Added two data objects: Moby Dick is now available as `mobydickText`,
  without needing to access a zipped text file; `encodedTextFiles.zip`
  is now a zipped archive of different encodings of (mainly) the UN
  Declaration of Human Rights, for testing conversions from 8-bit
  encodings in different (non-Roman) languages.

- phrasetotoken() now has a method correctly defined for corpus class
  objects.

- lexdiv() now works just like readability(), and is faster (based on
  data.table) and the code is simpler.

- removed quanteda::df() as a synonym for docfreq(), as this conflicted
  with stats::df().

- added version information when package is attached.

- improved rbind() and cbind() methods for dfm. Both now take any length
  sequence of dfms and perform better type checking.\
  rbind.dfm() also knits together dfms with different features, which
  can be useful for information and retrieval purposes or machine
  learning.

- `selectFeatures(x, anyDfm)` (where the second argument is a dfm) now
  works with a selection = “remove” option.

- tokenize.character adds a removeURL option.

- added a corpus method for data.frame objects, so that a corpus can be
  constructed directly from a data.frame. Requires the addition of a
  `textField` argument (similar to textfile).

- added `compress.dfm()` to combine identically named columns or rows.
  [\#123](https://github.com/quanteda/quanteda/issues/123)

- Much better `phrasetotoken()`, with additional methods for all
  combinations of corpus/character v. dictionary/character/collocations.

- Added a`weight(x, type, ...`) signature where the second argument can
  be a named numeric vector of weights, not just a label for a type of
  weight. Thanks
  <https://stackoverflow.com/questions/36815926/assigning-weights-to-different-features-in-r/36823475#36823475>.

- `as.data.frame` for dfms now passes `...` to `as.data.frame.matrix`.

- Fixed bug in `predict.fitted_textmodel_NB()` that caused a failure
  with k \> 2 classes
  ([\#129](https://github.com/quanteda/quanteda/issues/129))

- Improved `dfm.tokenizedTexts()` performance by taking care of
  zero-token documents more efficiently.

- `dictionary(file = "liwc_formatted_dict.dic", format = "LIWC")` now
  handles poorly formatted dictionary files better, such as the Moral
  Foundations Dictionary in the examples for
  [`?dictionary`](https://quanteda.io/reference/dictionary.md).

- added `as.tokenizedTexts` to coerce any list of characters to a
  tokenizedTexts object.

### Bug fixes \>= 0.9.6-3

- Fix bug in phrasetotoken, signature ‘corpus,ANY’ that was causing an
  infinite loop.

- Fixed bug introduced in commit b88287f (0.9.5-26) that caused a
  failure in dfm() with empty (zero-token) documents. Also fixes Issue
  [\#168](https://github.com/quanteda/quanteda/issues/168).

- Fixed bug that caused dfm() to break if no features or only one
  feature was found.

- Fixed bug in predict.fitted_textmodel_NB() that caused a failure with
  k \> 2 classes
  ([\#129](https://github.com/quanteda/quanteda/issues/129))

### Bug fixes

- Fixed a false-alarm warning message in textmodel_wordfish()

- Argument defaults for readability.corpus() now same as
  readability.character(). Fixes
  [\#107](https://github.com/quanteda/quanteda/issues/107).

- Fixed a bug causing LIWC format dictionary imports to fail if extra
  characters followed the closing % in the file header.

- Fixed a bug in applyDictionary(x, dictionary, exclusive = FALSE) when
  the dictionary produced no matches at all, caused by an attempt to
  negative index a NULL.
  [\#115](https://github.com/quanteda/quanteda/issues/115)

- Fixed [\#117](https://github.com/quanteda/quanteda/issues/117), a bug
  where wordstem.tokenizedTexts() removed attributes from the object,
  causing a failure of dfm.tokenizedTexts().

- Fixed [\#119](https://github.com/quanteda/quanteda/issues/119), a bug
  in selectFeatures.tokenizedTexts(x, features, selection = “remove”)
  that returned a NULL for a document’s tokens when no matching pattern
  for removal was found.

- Improved the behaviour of the `removeHyphens` option to
  [`tokenize()`](https://quanteda.io/reference/tokenize_internal.md)
  when `what = "fasterword"` or `what "fastestword"`.

- readability() now returns measures in order called, not function
  definition order.

- textmodel(x, model = “wordfish”) now removes zero-frequency documents
  and words prior to calling Rcpp.

- Fixed a bug in sample.corpus() that caused an error when no docvars
  existed. [\#128](https://github.com/quanteda/quanteda/issues/128)

## quanteda 0.9.4

CRAN release: 2016-02-21

- Added presidents’ first names to inaugCorpus

- Added textmodel implementation of multinomial and Bernoulli Naive
  Bayes.

- Improved documentation.

- Added [`c.corpus()`](https://quanteda.io/reference/corpus-class.md)
  method for concatenating arbitrarily large sets of corpus objects.

- Default for `similarity()` is now `margin = "documents"` – prevents
  overly massive results if `selection = NULL`.

- Defined [`rowMeans()`](https://rdrr.io/r/base/colSums.html) and
  [`colMeans()`](https://rdrr.io/r/base/colSums.html) methods for dfm
  objects.

- Enhancements to summary.character() and summary.corpus(): Added n = to
  summary.character(); added pass-through options to tokenize() in
  summary.corpus() and summary.character() methods; added toLower as an
  argument to both.

- Enhancements to corpus object indexing, including \[\[ and \[\[\<-.

### Bug fixes

- Fixed a bug preventing `smoother()` from working.

- Fixed a bug in segment.corpus(x, what = “tag”) that was failing to
  recover the tag values after the first text.

- Fix bug in `plot.dfm(x, comparison = TRUE)` method causing warning
  about rowMeans() failing.

- Fixed an issue for
  `mfdict <- dictionary(file = "http://ow.ly/VMRkL", format = "LIWC")`
  causing it to fail because of the irregular combination of tabs and
  spaces in the dictionary file.

- Fixed an exception thrown by wordstem.character(x) if one element of x
  was NA.

- dfm() on a text or tokenized text containing an NA element now returns
  a row with 0 feature counts. Previously it returned a count of 1 for
  an NA feature.

- Fix issue [\#91](https://github.com/quanteda/quanteda/issues/91)
  removeHyphens = FALSE not working in tokenise for some multiple
  intra-word hyphens, such as “one-of-a-kind”

- Fixed a bug in `as.matrix.similMatrix()` that caused scrambled
  conversion when feature sets compared were unequal, which normally
  occurs when setting `similarity(x, n = <something>)` when n \<
  nfeature(x)

- Fixed a bug in which a corpusSource object (from `textfile()`) with
  empty docvars prevented this argument from being supplied to
  `corpus(corpusSourceObject, docvars = something)`.

- Fixed inaccurate documentation for `weight()`, which previously listed
  unavailable options.

- More accurate and complete documentation for
  [`tokenize()`](https://quanteda.io/reference/tokenize_internal.md).

- traps an exception when calling wordstem.tokenizedTexts(x) where x was
  not word tokenized.

- Fixed a bug in `textfile()` that prevented passthrough arguments in …,
  such as `fileEncoding =` or `encoding =`

- Fixed a bug in `textfile()` that caused exceptions with input
  documents containing docvars when there was only a single column of
  docvars (such as .csv files)

## quanteda 0.9.2

- added new methods for similarity(), including sparse matrix
  computation for method = “correlation” and “cosine”. (More planned
  soon.) Also allows easy conversion to a matrix using as.matrix() on
  similarity lists.

- more robust implementation of LIWC-formatted dictionary file imports

- better implementation of tf-idf, and relative frequency weighting,
  especially for very large sparse matrix objects. tf(), idf(), and
  tfidf() now provide relative term frequency, inverse document
  frequency, and tf-idf directly.

- textmodel_wordfish() now accepts an integer `dispersionFloor` argument
  to constrain the phi parameter to a minimum value (of
  underdispersion).

- textfile() now takes a vector of filenames, if you wish to construct
  these yourself. See ?textfile examples.

- removeFeatures() and selectFeatures.collocations() now all use a
  consistent interface and same underlying code, with removeFeatures()
  acting as a wrapper to selectFeatures().

- convert(x, to = “stm”) now about 3-4x faster because it uses index
  positions from the dgCMatrix to convert to the sparse matrix format
  expected by stm.

### Bug fixes

- Fixed a bug in textfile() preventing encodingFrom and encodingTo from
  working properly.

- Fixed a nasty bug problem in `convert(x, to = "stm")` that mixed up
  the word indexes. Thanks Felix Haass for spotting this!

- Fixed a problem where wordstem was not working on ngram=1 tokenized
  objects

- Fixed toLower(x, keepAcronyms = TRUE) that caused an error when x
  contained no acronyms.

- Creating a corpus from a tm VCorpus now works if a “document” is a
  vector of texts rather than a single text

- Fixed a bug in texts(x, groups = MORE THAN ONE DOCVAR) that now groups
  correctly on combinations of multiple groups

## quanteda 0.9.0

- trim() now accepts proportions in addition to integer thresholds. Also
  accepts a new sparsity argument, which works like tm’s
  removeSparseTerms(x, sparse = ) (for those who really want to think of
  sparsity this way).

- \[i\] and \[i, j\] indexing of corpus objects is now possible, for
  extracting texts or docvars using convenient notation. See ?corpus
  Details.

- ngrams() and skipgrams() now use the same underlying function, with
  `skip` replacing the previous `window` argument (where a skip =
  window - 1). For efficiency, both are now implemented in C++.

- tokenize() has a new argument, removeHyphens, that controls the
  treatment of intra-word hyphens.

- Added new measures from readability for mean syllables per word and
  mean words per sentence directly.

- wordstem now works on ngrams (tokenizedTexts and dfm objects).

- Enhanced operation of kwic(), including the definition of a kwic class
  object, and a plot method for this object (produces a dispersion
  plot).

- Lots more error checking of arguments passed to … (and potentially
  misspecified or misspelled). Addresses Issue
  [\#62](https://github.com/quanteda/quanteda/issues/62).

- Almost all methods are now methods defined for objects, from a
  generic.

- texts(x, groups = ) now allows groups to be factors, not just document
  variable labels. There is a new method for texts.character(x, groups =
  ) which is useful for supplying a factor to concatenate character
  objects by group.

### Bug Fixes

- corrected inaccurate printing of valuetype in verbose note of
  selectFeatures.dfm(). (Did not affect functionality.)

- fixed broken quanteda.R demo, expanded demonstration code.

## quanteda 0.8.6

- removeFeatures.dfm(x, stopwords), selectFeatures.dfm(x, features), and
  dfm(x, ignoredFeatures) now work on objects created with ngrams. (Any
  ngram containing a stopword is removed.) Performance on these
  functions is already good but will be improved further soon.

- selectFeatures(x, features = ) is now possible, to produce a selection
  of features from x identical to those in . Not only are only features
  kept in x that are in , but also features in not in x are added to x
  as padded zero counts. This functionality can also be accessed via
  dfm(x, keptFeatures = ). This is useful when new data used in a test
  set needs to have identical features as a training set dfm constructed
  at an earlier stage.

- head.dfm() and tail.dfm() methods added.

- kwic() has new formals and new functionality, including a completely
  flexible set of matching for phrases, as well as control over how the
  texts and matching keyword(s) are tokenized.

- segment(x, what = “sentence”), and changeunits(x, to = “sentences”)
  now uses tokenize(x, what = “sentence”). Annoying warning messages now
  gone.

- smoother() and weight() formal “smooth” now changed to “smoothing” to
  avoid clashes with stats::smooth().

- Updated `corpus.VCorpus()` to work with recent updates to the **tm**
  package.

- added print method for tokenizedTexts

### Bug fixes

- fixed signature error message caused by `weight(x, "relFreq")` and
  `weight(x, "tfidf")`. Both now correctly produce objects of class
  dfmSparse.

- fixed bug in dfm(, keptFeatures = “whatever”) that passed it through
  as a glob rather than a regex to selectFeatures(). Now takes a regex,
  as per the manual description.

- fixed textfeatures() for type json, where now it can call
  jsonlite::fromJSON() on a file directly.

- dictionary(x, format = “LIWC”) now expanded to 25 categories by
  default, and handles entries that are listed on multiple lines in .dic
  files, such as those distributed with the LIWC.

## quanteda 0.8.4

- ngrams() rewritten to accept fully vectorized arguments for `n` and
  for `window`, thus implementing “skip-grams”. Separate function
  skipgrams() behaves in the standard “skipgram” fashion. bigrams(),
  deprecated since 0.7, has been removed from the namespace.

- corpus() no longer checks all documents for text encoding; rather,
  this is now based on a random sample of max()

- wordstem.dfm() both faster and more robust when working with large
  objects.

- toLower.NULL() now allows toLower() to work on texts with no words
  (returns NULL for NULL input)

- textfile() now works on zip archives of \*.txt files, although this
  may not be entirely portable.

### Bug fixes

- fixed bug in selectFeatures() / removeFeatures() that returned zero
  features if no features were found matching removal pattern

- corpus() previously removed document names, now fixed

- non-portable examples now removed completely from all documentation

## quanteda 0.8.2

- 0.8.2-1: Changed R version dependency to 3.2.0 so that Mac binary
  would build on CRAN.

- 0.8.2-1: `sample.corpus()` now samples documents from a corpus, and
  `sample.dfm()` samples documents or features from a dfm. `trim()`
  method for with `nsample` argument now calls `sample.dfm()`.

- `sample.corpus()` now samples documents from a corpus, and
  `sample.dfm()` samples documents or features from a dfm. `trim()`
  method for with `nsample` argument now calls `sample.dfm()`.

- tokenize improvements for what = “sentence”: more robust to specifying
  options, and does not split sentences after common abbreviations such
  as “Dr.”, “Prof.”, etc.

- corpus() no longer automatically converts encodings detected as
  non-UTF-8, as this detection is too imprecise.

- new function `scrabble()` computes English Scrabble word values for
  any text, applying any summary numerical function.

- dfm() now 2x faster, replacing previous data.table matching with
  direct construction of sparse matrix from match().\
  Code is also much simpler, based on using three new functions that are
  also available directly:

  - new “dfm” method for removeFeatures()\
  - new “dfm” method: selectFeatures() that is now how features can be
    added or removed from a dfm, based on vectors of regular
    expressions, globs, or fixed matching\
  - new “dfm” method: applyDictionary() that can replace features
    through matching with values in key-value lists from a dictionary
    class objects, based on vectors of regular expressions, globs, or
    fixed matching for dictionary values. All functionality for applying
    dictionaries now takes place through applyDictionary().

### Bug Fixes

- fixed the problem that document names were getting erased in corpus()
  because stringi functions were removing them
- fixed problem in tokenize(x, “character”, removePunct = TRUE) that
  deleted texts that had no punctuation to begin with
- fixed problem in dictionary(, format = “LIWC”) causing import to fail
  for some LIWC dictionaries.
- fixed problem in tokenize(x, ngrams = N) where N \> length(x). Now
  returns NULL instead of an erroneously tokenized set of ngrams.
- Fixed a bug in `subset.corpus()` related to environments that
  sometimes caused the method to break if nested in function
  environments.

### Deletions

- `clean()` is no more.

### API changes

- `addto` option removed from
  [`dfm()`](https://quanteda.io/reference/dfm.md)

### Imminent Changes

- change behaviour of `ignoredFeatures` and `removeFeatures()` applied
  to ngrams; change behaviour of stem = TRUE applied to ngrams (in
  [`dfm()`](https://quanteda.io/reference/dfm.md))
- create `ngrams.tokenizedTexts()` method, replacing current `ngrams()`,
  `bigrams()`

## quanteda 0.8.0

### Syntax changes and workflow streamlining

The workflow is now more logical and more streamlined, with a new
workflow vignette as well as a design vignette explaining the principles
behind the workflow and the commands that encourage this workflow. The
document also details the development plans and things remaining to be
done on the project.

### Encoding detection and conversion

Newly rewritten command encoding() detects encoding for character,
corpus, and corpusSource objects (created by textfile). When creating a
corpus using corpus(), detection is automatic to UTF-8 if an encoding
other than UTF-8, ASCII, or ISO-8859-1 is detected.

### Major infrastructural changes

The tokenization, cleaning, lower-casing, and dfm construction functions
now use the `stringi` package, based on the ICU library. This results
not only in substantial speed improvements, but also more correctly
handles Unicode characters and strings.

- tokenize() and clean() now using stringi, resulting in much faster
  performance and more consistent behaviour across platforms.

- tokenize() now works on sentences

- summary.corpus() and summary.character() now use the new tokenization
  functions for counting tokens

- dfm(x, dictionary = mydict) now uses stringi and is both more reliable
  and many many times faster.

- phrasetotoken() now using stringi.

- removeFeatures() now using stringi and fixed binary matches on
  tokenized texts

### Other changes

- textfile has a new option, cache = FALSE, for not writing the data to
  a temporary file, but rather storing the object in memory if that is
  preferred.

- language() is removed. (See Encoding… section above for changes to
  encoding().)

- new object encodedTexts contains some encoded character objects for
  testing.

- ie2010Corpus now has UTF-8 encoded texts (previously was Unicode
  escaped for non-ASCII characters)

- texts() and docvars() methods added for corpusSource objects.

- new methods for `tokenizedTexts` objects:
  [`dfm()`](https://quanteda.io/reference/dfm.md), `removeFeatures()`,
  and `syllables()`

- `syllables()` is now much faster, using matching through `stringi` and
  merging using `data.table`.

- added `readability()` to compute (fast!) readability indexes on a text
  or corpus

- tokenize() now creates ngrams of any length, with two new arguments:
  `ngrams =` and `concatenator = "_"`. The new arguments to
  [`tokenize()`](https://quanteda.io/reference/tokenize_internal.md) can
  be passed through from
  [`dfm()`](https://quanteda.io/reference/dfm.md).

### Bug fixes

- fixed a problem in `textfile()` causing it to fail on Windows machines
  when loading `*.txt`

- nsentence() was not counting sentences correctly if the text was
  lower-cased - now issues an error if no upper-case characters are
  detected. This was also causing readability() to fail.

## quanteda 0.7.3

- added an ntoken() method for dfm objects.

- fixed a bug wherein `convert(anydfm, to = "tm")` created a
  DocumentTermMatrix, not a TermDocumentMatrix. Now correctly creates a
  TermDocumentMatrix. (Both worked previously in topicmodels::LDA() so
  many users may not notice the change.)

## quanteda 0.7.2

CRAN release: 2015-04-07

- phrasetotokens works with dictionaries and collocations, to transform
  multi-word expressions into single tokens in texts or corpora

- dictionaries now redefined as S4 classes

- improvements to collocations(), now does not include tokens that are
  separated by punctuation

- created tokenizeOnly\*() functions, for testing tokenizing separately
  from cleaning, and a cleanC(), where both new separate functions are
  implemented in C

- tokenize() now has a new option, cpp=TRUE, to use a C++ tokenizer and
  cleaner, resulting in much faster text tokenization and cleaning,
  including that used in dfm()

- textmodel_wordfish now implemented entirely in C for speed. No std
  errors yet but coming soon. No predict method currently working
  either.

- ie2010Corpus, and exampleString now moved into quanteda (formerly were
  only in quantedaData because of non-ASCII characters in each - solved
  with native2ascii and encodings).

- All dependencies, even conditional, to the quantedaData and austin
  packages have been removed.

## quanteda 0.7.1

Many major changes to the syntax in this version.

- trimdfm, flatten.dictionary, the textfile functions, dictionary
  converters are all gone from the NAMESPACE

- formals changed a bit in clean(), kwic().

- compoundWords() -\> phrasetotoken()

- Cleaned up minor issues in documentation.

- countSyllables data object renamed to englishSyllables.Rdata, and
  function renamed to syllables().

- stopwordsGet() changed to stopwords(). stopwordsRemove() changed to
  removeFeatures().

- new dictionary() constructor function that also does import and
  conversion, replacing old readWStatdict and readLIWCdict functions.

- one function to read in text files, called `textsource`, that does the
  work for different file types based on the filename extension, and
  works also for wildcard expressions (that can link to directories for
  example)

## quanteda 0.7.0

- dfm now sparse by default, implemented as subclasses of the Matrix
  package. Option dfm(…, matrixType=“sparse”) is now the default,
  although matrixType=“dense” will still produce the old S3-class dfm
  based on a regular matrix, and all dfm methods will still work with
  this object.

- Improvements to: weight(), print() for dfms.

- New methods for dfms: docfreq(), weight(), summary(), as.matrix(),
  as.data.frame.

## quanteda 0.6.6

- No more depends, all done through imports. Passes clean check. The
  start of our reliance more on the master branch rather than having
  merges from dev to master happen only once in a blue moon.

- bigrams in dfm() when bigrams=TRUE and ignoredFeatures= now removed if
  any bigram contains an ignoredFeature

- stopwordsRemove() now defined for sparse dfms and for collocations.

- stopwordsRemove() now requires an explicit stopwords= argument, to
  emphasize the user’s responsibility for applying stopwords.

## quanteda 0.6.5

- New engine for dfm now implemented as standard, using data.table and
  Matrix for fast, efficient (sparse) matrixes.

- Added trigram collocations (n=3) to collocations().

- Improvements to clean(): Minor fixes to clean() so that
  removeDigits=TRUE removes “€10bn” entirely and not just the “€10”.
  clean() now removes http and https URLs by default, although does not
  preserve them (yet). clean also handles numbers better, to remove
  1,000,000 and 3.14159 if removeDigits=TRUE but not crazy8 or 4sure.

- dfm works for documents that contain no features, including for
  dictionary counts. Thanks to Kevin Munger for catching this.

## quanteda 0.6.4

- first cut at REST APIs for Twitter and Facebook

- some minor improvements to sentence segmentation

- improvements to package dependencies and imports - but this is
  ongoing!

- Added more functions to dfms, getting there…

- Added the ability to segment a corpus on tags (e.g. ##TAG1 text text,
  \##TAG2) and have the document split using the tags as a delimiter and
  the tag then added to the corpus as a docvar.

## quanteda 0.6.3

- added textmodel_lda support, including LDA, CTM, and STM. Added a
  converter dfm2stmformat() between dfm and stm’s input format.

- as.dfm works now for data.frame objects

- added Arabic to list of stopwords. (Still working on a stemmer for
  Arabic.)

## quanteda 0.6.2

- The first appearance of dfms(), to create a sparse Matrix using the
  Matrix package. Eventually this will become the default format for all
  but small dfms. Not only is this far more efficient, it is also much
  faster.

- Minor speed gains for clean() – but still much more work to be done
  with clean().

## quanteda 0.6.1

- started textmodel_wordfish, textmodel_ca. textmodel_wordfish takes an
  mcmc argument that calls JAGS wordfish.

- now depends on ca, austin rather than importing them

- dfm subsetting with \[,\] now works

- docnames()\[\], \[\]\<-, docvars()\[\] and \[\]\<- now work correctly

## quanteda 0.6.0

- Added textmodel for scaling and prediction methods, including for
  starters, wordscores and naivebayes class models. LIKELY TO BE BUGGY
  AND QUIRKY FOR A WHILE.

- Added smoothdfm() and weight() methods for dfms.

- Fixed a bug in segmentSentence().

## quanteda 0.5.8

### Classification and scaling methods

- New dfm methods for fitmodel(), predict(), and specific model fitting
  and prediction methods called by these, for classification and scaling
  of different “textmodel” types, such as wordscores and Naive Bayes
  (for starters).

## quanteda 0.5.7

- added compoundWords() to turn space-delimited phrases into single
  “tokens”. Works with dfm(, dictionary=) if the text has been
  pre-processed with compoundWords() and the dictionary joins phrases
  with the connector (“\_“). May add this functionality to be more
  automatic in future versions.

- new keep argument for trimdfm() now takes a regular expression for
  which feature labels to retain. New defaults for minDoc and minCount
  (1 each).

- added nfeature() method for dfm objects.

### New arguments for dfm()

- thesaurus: works to record equivalency classes as lists of words or
  regular expressions for a given key/label.

- keep: regular expression pattern match for features to keep

## quanteda 0.5.6

- added readLIWCdict() to read LIWC-formatted dictionaries

- fixed a “bug”/feature in readWStatDict() that eliminated wildcards
  (and all other punctuation marks) - now only converts to lower.

- improved clean() functions to better handle Twitter, punctuation, and
  removing extra whitespace

## quanteda 0.5.5

- fixed broken dictionary option in dfm()

- fixed a bug in dfm() that was preventing clean() options from being
  passed through

- added Dice and point-wise mutual information as association measures
  for collocations()

- added: similarity() to implement similarity measures for documents or
  features as vector representations

- begun: implementing dfm resample methods, but this will need more time
  to work.\
  (Solution: a three way table where the third dim is the resampled
  text.)

- added is.resample() for dfm and corpus objects

- added Twitter functions: getTweets() performs a REST search through
  twitteR, corpus.twitter creates a corpus object with test and docvars
  form each tweet (operational but needs work)

- added various resample functions, including making dfm a
  multi-dimensional object when created from a resampled corpus and
  dfm(, bootstrap=TRUE).

- modified the print.dfm() method.

## quanteda 0.5.4

- updated corpus.directory to allow specification of the file extension
  mask

- updated docvars\<- and metadoc\<- to take the docvar names from the
  assigned data.frame if field is omitted.

- added field to docvars()

- enc argument in corpus() methods now actually converts from enc to
  “UTF-8”

- started working on clean to give it exceptions for @ \# \_ for twitter
  text and to allow preservation of underscores used in
  bigrams/collocations.

- Added: a `+` method for corpus objects, to combine a corpus using this
  operator.

- Changed and fixed: collocations(), which was not only fatally slow and
  inefficient, but also wrong. Now is much faster and O(n) because it
  uses data.table and vector operations only.

- Added: resample() for corpus texts.

## quanteda 0.5.3

- added statLexdiv() to compute the lexical diversity of texts from a
  dfm.

- minor bug fixes; update to print.corpus() output messages.

- added a wrapper function for SnowballC::wordStem, called wordstem(),
  so that this can be imported without loading the whole package.

## quanteda 0.5.2

- Added a corpus constructor method for the VCorpus class object from
  the tm package.

- added zipfiles() to unzip a directory of text files from disk or a
  URL, for easy import into a corpus using corpus.directory(zipfiles())

## quanteda 0.5.1

- Fixed all the remaining issues causing warnings in R CMD CHECK, now
  all are fixed.\
  Mostly these related to documentation.

- Fixed corpus.directory to better implementing naming of docvars, if
  found.

- Moved twitter.R to the R_NEEDFIXING until it can be made to pass
  tests. Apparently setup_twitter_oauth() is deprecated in the latest
  version of the twitteR package.

## quanteda 0.5.0

### Lots of new functions

- plot.dfm method for producing word clouds from dfm objects

- print.dfm, print.corpus, and summary.corpus methods now defined

- new accessor functions defined, such as docnames(), settings(),
  docvars(), metadoc(), metacorpus(), encoding(), and language()

- replacement functions defined that correspond to most of the above
  accessor functions, e.g. encoding(mycorpus) \<- “UTF-8”

- segment(x, to=c(“tokens”, “sentences”, “paragraphs”, “other”, …) now
  provides an easy and powerful method for segmenting a corpus by units
  other than just tokens

- a settings() function has been added to manage settings that would
  commonly govern how texts are converted for processing, so that these
  can be preserved in a corpus and applied to operations that are
  relevant. These settings also propagate to a dfm for both replication
  purposes and to govern operations for which they would be relevant,
  when applied to a dfm.

### Old functions vastly improved

- better ways now exist to manage corpus internals, such as through the
  accessor functions, rather than trying to access the internal
  structure of the corpus directly.

- basic functions such as tokenize(), clean(), etc are now faster,
  neater, and operate generally on vectors and return consistent object
  types

### Better object and class design

- the corpus object has been redesigned with more flexible components,
  including a settings list, better corpus-level metadata, and smarter
  implementation of document-level attributes including user-defined
  variables (docvars) and document- level meta-data (metadoc)

- the dfm now has a proper class definition, including additional
  attributes that hold the settings used to produce the dfm.

- all important functions are now defined as methods for classes of
  built-in (e.g. character) objects, or quanteda objects such as a
  corpus or dfm. Lots of functions operate on both, for instance
  dfm.corpus(x) and dfm.character(x).

### more complete documentation

- all functions are now documented and have working examples

- quanteda.pdf provides a pdf version of the function documentation in
  one easy-to-access document
