# Remove sentences based on their token lengths or a pattern match

Removes sentences from a corpus or a character vector shorter than a
specified length.

## Usage

``` r
corpus_trim(
  x,
  what = c("sentences", "paragraphs", "documents"),
  min_ntoken = 1,
  max_ntoken = NULL,
  exclude_pattern = NULL
)

char_trim(
  x,
  what = c("sentences", "paragraphs", "documents"),
  min_ntoken = 1,
  max_ntoken = NULL,
  exclude_pattern = NULL
)
```

## Arguments

- x:

  [corpus](https://quanteda.io/reference/corpus.md) or character object
  whose sentences will be selected.

- what:

  units of trimming, `"sentences"` or `"paragraphs"`, or `"documents"`

- min_ntoken, max_ntoken:

  minimum and maximum lengths in word tokens (excluding punctuation).
  Note that these are approximate numbers of tokens based on checking
  for word boundaries, rather than on-the-fly full tokenisation.

- exclude_pattern:

  a stringi regular expression whose match (at the sentence level) will
  be used to exclude sentences

## Value

a [corpus](https://quanteda.io/reference/corpus.md) or character vector
equal in length to the input. If the input was a corpus, then the all
docvars and metadata are preserved. For documents whose sentences have
been removed entirely, a null string (`""`) will be returned.

## Examples

``` r
txt <- c("PAGE 1. This is a single sentence.  Short sentence. Three word sentence.",
         "PAGE 2. Very short! Shorter.",
         "Very long sentence, with multiple parts, separated by commas.  PAGE 3.")
corp <- corpus(txt, docvars = data.frame(serial = 1:3))
corp
#> Corpus consisting of 3 documents and 1 docvar.
#> text1 :
#> "PAGE 1. This is a single sentence. Short sentence. Three wo..."
#> 
#> text2 :
#> "PAGE 2. Very short! Shorter."
#> 
#> text3 :
#> "Very long sentence, with multiple parts, separated by commas..."
#> 

# exclude sentences shorter than 3 tokens
corpus_trim(corp, min_ntoken = 3)
#> Corpus consisting of 2 documents and 1 docvar.
#> text1 :
#> "This is a single sentence. Three word sentence."
#> 
#> text3 :
#> "Very long sentence, with multiple parts, separated by commas..."
#> 
# exclude sentences that start with "PAGE <digit(s)>"
corpus_trim(corp, exclude_pattern = "^PAGE \\d+")
#> Corpus consisting of 3 documents and 1 docvar.
#> text1 :
#> "This is a single sentence. Short sentence. Three word sent..."
#> 
#> text2 :
#> "Very short! Shorter."
#> 
#> text3 :
#> "Very long sentence, with multiple parts, separated by commas..."
#> 

# trimming character objects
char_trim(txt, "sentences", min_ntoken = 3)
#>                                                           text1 
#>              "This is a single sentence.  Three word sentence." 
#>                                                           text3 
#> "Very long sentence, with multiple parts, separated by commas." 
char_trim(txt, "sentences", exclude_pattern = "sentence\\.")
#>                                                                    text1 
#>                                                                "PAGE 1." 
#>                                                                    text2 
#>                                         "PAGE 2.  Very short!  Shorter." 
#>                                                                    text3 
#> "Very long sentence, with multiple parts, separated by commas.  PAGE 3." 
```
