# Create a dictionary object

Create a quanteda dictionary object to perform pattern matching on
[tokens](https://quanteda.io/reference/tokens.md),
[dfm](https://quanteda.io/reference/dfm.md) and
[fcm](https://quanteda.io/reference/fcm.md).

## Usage

``` r
dictionary(
  x,
  file = NULL,
  format = NULL,
  separator = " ",
  tolower = TRUE,
  tokenize = FALSE,
  levels = 1:100,
  encoding = "utf-8"
)
```

## Arguments

- x:

  a named list of
  [valuetype](https://quanteda.io/reference/valuetype.md) patterns or an
  existing dictionary object. See examples. This argument should be
  omitted if `file` is specified.

- file:

  file identifier for a foreign dictionary.

- format:

  character identifier for the format of the foreign dictionary. If not
  supplied, the format is guessed from the dictionary file's extension.
  Available options are:

  `"wordstat"`

  : format used by Provalis Research's WordStat software

  `"LIWC"`

  : format used by the Linguistic Inquiry and Word Count software

  `"yoshikoder"`

  : format used by Yoshikoder software

  `"lexicoder"`

  : format used by Lexicoder

  `"YAML"`

  : the standard YAML format

- separator:

  the character in between multi-word dictionary values. This defaults
  to `" "`.

- tolower:

  if `TRUE`, convert all dictionary values to lowercase.

- tokenize:

  if `TRUE` segment dictionary values by separators to using the
  built-in tokenizer. Useful for Japanese and Chinese dictionaries.

- levels:

  integers specifying the levels of entries in `x` or `file` to be
  included in the object.

- encoding:

  additional optional encoding value for reading in imported
  dictionaries. This uses the [iconv](https://rdrr.io/r/base/iconv.html)
  labels for encoding. See the "Encoding" section of the help for
  [file](https://rdrr.io/r/base/connections.html).

## Value

A dictionary class object, essentially a specially classed named list of
characters.

## Details

A dictionary object can include multi-word expressions segmented by
`separator`. When it is applied to tokens object, they match both
sequences of separate tokens and compounded tokens.

Dictionary objects can be subsetted using
[`[`](https://quanteda.io/reference/dictionary-class.md) and
[`[[`](https://quanteda.io/reference/dictionary-class.md), operating the
same as the equivalent
[list](https://quanteda.io/reference/dictionary-class.md) operators. If
`dictionary()` is applied to existing objects, it is possible to select
`levels`.

Dictionary objects can be coerced from and to lists using
[`as.dictionary()`](https://quanteda.io/reference/as.dictionary.md) and
[`as.list()`](https://quanteda.io/reference/dictionary-class.md), and
checked using
[`is.dictionary()`](https://quanteda.io/reference/as.dictionary.md).

Currently supported input file formats are the WordStat, LIWC, Lexicoder
v2 and v3, and Yoshikoder formats. The import using the LIWC format
works with all currently available dictionary files supplied as part of
the LIWC 2001, 2007, and 2015 software (see References).

## References

WordStat dictionaries page, from Provalis Research
<https://provalisresearch.com/products/content-analysis-software/wordstat-dictionary/>.

Pennebaker, J.W., Chung, C.K., Ireland, M., Gonzales, A., & Booth, R.J.
(2007). The development and psychometric properties of LIWC2007.
\[Software manual\]. Austin, TX (<https://www.liwc.app/>).

Yoshikoder page, from Will Lowe
<https://conjugateprior.org/software/yoshikoder/>.

Lexicoder format, <https://www.snsoroka.com/data-lexicoder/>

## See also

[`as.dictionary()`](https://quanteda.io/reference/as.dictionary.md),
[`as.list()`](https://quanteda.io/reference/dictionary-class.md),
[`is.dictionary()`](https://quanteda.io/reference/as.dictionary.md)

## Examples

``` r
corp <- corpus_subset(data_corpus_inaugural, Year > 2000)
toks <- tokens(corp)

dict <- dictionary(list(
   tax = c("tax", "taxes", "taxing"),         # fixed patterns
   economy = list("econom*",                  # glob patterns
                  job = c("work*", "job*")),  # nested keys
   health = c("health care", "public health") # multi-word expressions
))

# compound tokens
tokens_compound(toks, pattern = dict) |>
    dfm() |>
    dfm_select(dict)
#> Document-feature matrix of: 7 documents, 16 features (65.18% sparse) and 4
#> docvars.
#>             features
#> docs         work economy taxes working public_health economic jobs health_care
#>   2001-Bush     4       2     1       1             1        0    0           0
#>   2005-Bush     6       0     0       0             0        1    0           0
#>   2009-Obama    6       3     0       0             0        0    3           1
#>   2013-Obama    4       1     0       0             0        2    2           1
#>   2017-Trump    1       0     1       0             0        0    4           0
#>   2021-Biden    6       0     0       0             0        0    3           1
#>             features
#> docs         worked workers
#>   2001-Bush       0       0
#>   2005-Bush       0       0
#>   2009-Obama      1       2
#>   2013-Obama      0       1
#>   2017-Trump      0       2
#>   2021-Biden      0       0
#> [ reached max_ndoc ... 1 more document, reached max_nfeat ... 6 more features ]

tokens_lookup(toks, dictionary = dict, levels = 1) |>
    dfm()
#> Document-feature matrix of: 7 documents, 3 features (23.81% sparse) and 4
#> docvars.
#>             features
#> docs         tax economy health
#>   2001-Bush    1       7      1
#>   2005-Bush    0       7      0
#>   2009-Obama   0      17      1
#>   2013-Obama   1      12      1
#>   2017-Trump   1       8      0
#>   2021-Biden   0      10      1
#> [ reached max_ndoc ... 1 more document ]

# subset a dictionary
dict[1:2]
#> Dictionary object with 2 primary key entries and 2 nested levels.
#> - [tax]:
#>   - tax, taxes, taxing
#> - [economy]:
#>   - econom*
#>   - [job]:
#>     - work*, job*
dict[c("economy")]
#> Dictionary object with 1 primary key entry and 2 nested levels.
#> - [economy]:
#>   - econom*
#>   - [job]:
#>     - work*, job*

# update a dictionary
dictionary(dict, levels = 2)
#> Dictionary object with 1 key entry.
#> - [job]:
#>   - work*, job*

if (FALSE) { # \dontrun{
dfmat <- dfm(tokens(data_corpus_inaugural))

# import the Laver-Garry dictionary from Provalis Research
download.file("https://provalisresearch.com/Download/LaverGarry.zip",
              tf <- tempfile(), mode = "wb")
unzip(tf, exdir = (td <- tempdir()))
dict_lg <- dictionary(file = paste(td, "LaverGarry.cat", sep = "/"))
dfm_lookup(dfmat, dict_lg)

# import a LIWC formatted dictionary from http://www.moralfoundations.org
download.file("http://bit.ly/37cV95h", tf <- tempfile())
dict_liwc <- dictionary(file = tf, format = "LIWC")
dfm_lookup(dfmat, dict_liwc)
} # }
```
