# Construct a corpus object

Creates a corpus object from available sources. The currently available
sources are:

- a [character](https://rdrr.io/r/base/character.html) vector,
  consisting of one document per element; if the elements are named,
  these names will be used as document names.

- a [data.frame](https://rdrr.io/r/base/data.frame.html) (or a tibble
  `tbl_df`), whose default document id is a variable identified by
  `docid_field`; the text of the document is a variable identified by
  `text_field`; and other variables are imported as document-level
  meta-data. This matches the format of data.frames constructed by the
  the readtext package.

- a [kwic](https://quanteda.io/reference/kwic.md) object constructed by
  [`kwic()`](https://quanteda.io/reference/kwic.md).

- a tm [VCorpus](https://rdrr.io/pkg/tm/man/VCorpus.html) or
  [SimpleCorpus](https://rdrr.io/pkg/tm/man/SimpleCorpus.html) class
  object, with the fixed metadata fields imported as
  [docvars](https://quanteda.io/reference/docvars.md) and corpus-level
  metadata imported as [meta](https://quanteda.io/reference/meta.md)
  information.

- a corpus object.

## Usage

``` r
corpus(x, ...)

# S3 method for class 'corpus'
corpus(
  x,
  docnames = quanteda::docnames(x),
  docvars = quanteda::docvars(x),
  meta = quanteda::meta(x),
  ...
)

# S3 method for class 'character'
corpus(
  x,
  docnames = NULL,
  docvars = NULL,
  meta = list(),
  unique_docnames = TRUE,
  ...
)

# S3 method for class 'data.frame'
corpus(
  x,
  docid_field = "doc_id",
  text_field = "text",
  meta = list(),
  unique_docnames = TRUE,
  ...
)

# S3 method for class 'kwic'
corpus(
  x,
  split_context = TRUE,
  extract_keyword = TRUE,
  meta = list(),
  concatenator = " ",
  ...
)

# S3 method for class 'Corpus'
corpus(x, ...)
```

## Arguments

- x:

  a valid corpus source object

- ...:

  not used directly

- docnames:

  Names to be assigned to the texts. Defaults to the names of the
  character vector (if any); `doc_id` for a data.frame; the document
  names in a tm corpus; or a vector of user-supplied labels equal in
  length to the number of documents. If none of these are round, then
  "text1", "text2", etc. are assigned automatically.

- docvars:

  a data.frame of document-level variables associated with each text

- meta:

  a named list that will be added to the corpus as corpus-level, user
  meta-data. This can later be accessed or updated using
  [`meta()`](https://quanteda.io/reference/meta.md).

- unique_docnames:

  logical; if `TRUE`, enforce strict uniqueness in `docnames`;
  otherwise, rename duplicated docnames using an added serial number,
  and treat them as segments of the same document.

- docid_field:

  optional column index of a document identifier; defaults to "doc_id",
  but if this is not found, then will use the rownames of the
  data.frame; if the rownames are not set, it will use the default
  sequence based on `([quanteda_options]("base_docname")`.

- text_field:

  the character name or numeric index of the source `data.frame`
  indicating the variable to be read in as text, which must be a
  character vector. All other variables in the data.frame will be
  imported as docvars. This argument is only used for `data.frame`
  objects.

- split_context:

  logical; if `TRUE`, split each kwic row into two "documents", one for
  "pre" and one for "post", with this designation saved in a new docvar
  `context` and with the new number of documents therefore being twice
  the number of rows in the kwic.

- extract_keyword:

  logical; if `TRUE`, save the keyword matching `pattern` as a new
  docvar `keyword`

- concatenator:

  character between tokens, default is the whitespace.

## Value

A [corpus](https://quanteda.io/reference/corpus-class.md) class object
containing the original texts, document-level variables, document-level
metadata, corpus-level metadata, and default settings for subsequent
processing of the corpus.

For quanteda \>= 2.0, this is a specially classed character vector. It
has many additional attributes but **you should not access these
attributes directly**, especially if you are another package author. Use
the extractor and replacement functions instead, or else your code is
not only going to be uglier, but also likely to break should the
internal structure of a corpus object change. Using the accessor and
replacement functions ensures that future code to manipulate corpus
objects will continue to work.

## Details

The texts and document variables of corpus objects can also be accessed
using index notation and the `$` operator for accessing or assigning
docvars. For details, see
[`[.corpus()`](https://quanteda.io/reference/corpus-class.md).

## See also

[corpus](https://quanteda.io/reference/corpus-class.md),
[`docvars()`](https://quanteda.io/reference/docvars.md),
[`meta()`](https://quanteda.io/reference/meta.md),
[`as.character.corpus()`](https://quanteda.io/reference/as.character.corpus.md),
[`ndoc()`](https://quanteda.io/reference/ndoc.md),
[`docnames()`](https://quanteda.io/reference/docnames.md)

## Examples

``` r
# create a corpus from texts
corpus(data_char_ukimmig2010)
#> Corpus consisting of 9 documents.
#> BNP :
#> "IMMIGRATION: AN UNPARALLELED CRISIS WHICH ONLY THE BNP CAN S..."
#> 
#> Coalition :
#> "IMMIGRATION. The Government believes that immigration has e..."
#> 
#> Conservative :
#> "Attract the brightest and best to our country. Immigration h..."
#> 
#> Greens :
#> "Immigration. Migration is a fact of life. People have alway..."
#> 
#> Labour :
#> "Crime and immigration The challenge for Britain We will cont..."
#> 
#> LibDem :
#> "firm but fair immigration system Britain has always been an ..."
#> 
#> [ reached max_ndoc ... 3 more documents ]

# create a corpus from texts and assign meta-data and document variables
summary(corpus(data_char_ukimmig2010,
               docvars = data.frame(party = names(data_char_ukimmig2010))), 5)
#> Corpus consisting of 9 documents, showing 5 documents:
#> 
#>          Text Types Tokens Sentences        party
#>           BNP  1125   3280       136          BNP
#>     Coalition   142    260        12    Coalition
#>  Conservative   251    499        21 Conservative
#>        Greens   322    679        30       Greens
#>        Labour   298    683        33       Labour
#> 

# import a tm VCorpus
if (requireNamespace("tm", quietly = TRUE)) {
    data(crude, package = "tm")    # load in a tm example VCorpus
    vcorp <- corpus(crude)
    summary(vcorp)

    data(acq, package = "tm")
    summary(corpus(acq), 5)

    vcorp2 <- tm::VCorpus(tm::VectorSource(data_char_ukimmig2010))
    corp <- corpus(vcorp2)
    summary(corp)
}
#> Corpus consisting of 9 documents, showing 9 documents:
#> 
#>          Text Types Tokens Sentences author       datetimestamp description
#>           BNP  1125   3280       136     NA 2026-07-17 23:37:49          NA
#>     Coalition   142    260        12     NA 2026-07-17 23:37:49          NA
#>  Conservative   251    499        21     NA 2026-07-17 23:37:49          NA
#>        Greens   322    679        30     NA 2026-07-17 23:37:49          NA
#>        Labour   298    683        33     NA 2026-07-17 23:37:49          NA
#>        LibDem   251    483        26     NA 2026-07-17 23:37:49          NA
#>            PC    77    114         5     NA 2026-07-17 23:37:49          NA
#>           SNP    88    134         4     NA 2026-07-17 23:37:49          NA
#>          UKIP   346    723        37     NA 2026-07-17 23:37:49          NA
#>  heading id language origin
#>       NA  1       en     NA
#>       NA  2       en     NA
#>       NA  3       en     NA
#>       NA  4       en     NA
#>       NA  5       en     NA
#>       NA  6       en     NA
#>       NA  7       en     NA
#>       NA  8       en     NA
#>       NA  9       en     NA
#> 

# construct a corpus from a data.frame
dat <- data.frame(letter_factor = factor(rep(letters[1:3], each = 2)),
                  some_ints = 1L:6L,
                  some_text = paste0("This is text number ", 1:6, "."),
                  stringsAsFactors = FALSE,
                  row.names = paste0("fromDf_", 1:6))
dat
#>          letter_factor some_ints              some_text
#> fromDf_1             a         1 This is text number 1.
#> fromDf_2             a         2 This is text number 2.
#> fromDf_3             b         3 This is text number 3.
#> fromDf_4             b         4 This is text number 4.
#> fromDf_5             c         5 This is text number 5.
#> fromDf_6             c         6 This is text number 6.
summary(corpus(dat, text_field = "some_text",
               meta = list(source = "From a data.frame called mydf.")))
#> Corpus consisting of 6 documents, showing 6 documents:
#> 
#>      Text Types Tokens Sentences letter_factor some_ints
#>  fromDf_1     6      6         1             a         1
#>  fromDf_2     6      6         1             a         2
#>  fromDf_3     6      6         1             b         3
#>  fromDf_4     6      6         1             b         4
#>  fromDf_5     6      6         1             c         5
#>  fromDf_6     6      6         1             c         6
#> 
```
