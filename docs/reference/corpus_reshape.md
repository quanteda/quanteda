# Recast the document units of a corpus

For a corpus, reshape (or recast) the documents to a different level of
aggregation. Units of aggregation can be defined as documents,
paragraphs, or sentences. Because the corpus object records its current
"units" status, it is possible to move from recast units back to
original units, for example from documents, to sentences, and then back
to documents (possibly after modifying the sentences).

## Usage

``` r
corpus_reshape(
  x,
  to = c("sentences", "paragraphs", "documents"),
  use_docvars = TRUE,
  verbose = FALSE,
  ...
)
```

## Arguments

- x:

  corpus whose document units will be reshaped.

- to:

  new document units in which the corpus will be recast.

- use_docvars:

  if `TRUE`, repeat the docvar values for each segmented text; if
  `FALSE`, drop the docvars in the segmented corpus. Dropping the
  docvars might be useful in order to conserve space or if these are not
  desired for the segmented corpus.

- verbose:

  if `TRUE`, print timing messages to the console,

- ...:

  additional arguments passed to
  [`tokens()`](https://quanteda.io/reference/tokens.md), since the
  syntactic segmenter uses this function).

## Value

A corpus object with the documents defined as the new units, including
document-level meta-data identifying the original documents.

## Examples

``` r
# simple example
corp1 <- corpus(c(textone = "This is a sentence.  Another sentence.  Yet another.",
                 textwo = "Premiere phrase.  Deuxieme phrase."),
                 docvars = data.frame(country=c("UK", "USA"), year=c(1990, 2000)))
summary(corp1)
#> Corpus consisting of 2 documents, showing 2 documents:
#> 
#>     Text Types Tokens Sentences country year
#>  textone     8     11         3      UK 1990
#>   textwo     4      6         2     USA 2000
#> 
summary(corpus_reshape(corp1, to = "sentences"))
#> Corpus consisting of 5 documents, showing 5 documents:
#> 
#>       Text Types Tokens Sentences country year
#>  textone.1     5      5         1      UK 1990
#>  textone.2     3      3         1      UK 1990
#>  textone.3     3      3         1      UK 1990
#>   textwo.1     3      3         1     USA 2000
#>   textwo.2     3      3         1     USA 2000
#> 

# example with inaugural corpus speeches
(corp2 <- corpus_subset(data_corpus_inaugural, Year>2004))
#> Corpus consisting of 6 documents and 4 docvars.
#> 2005-Bush :
#> "Vice President Cheney, Mr. Chief Justice, President Carter, ..."
#> 
#> 2009-Obama :
#> "My fellow citizens: I stand here today humbled by the task b..."
#> 
#> 2013-Obama :
#> "Vice President Biden, Mr. Chief Justice, Members of the Unit..."
#> 
#> 2017-Trump :
#> "Chief Justice Roberts, President Carter, President Clinton, ..."
#> 
#> 2021-Biden :
#> "Chief Justice Roberts, Vice President Harris, Speaker Pelosi..."
#> 
#> 2025-Trump :
#> "Thank you. Thank you very much, everybody. Wow. Thank you..."
#> 
corp2para <- corpus_reshape(corp2, to = "paragraphs")
corp2para
#> Corpus consisting of 475 documents and 4 docvars.
#> 2005-Bush.1 :
#> "Vice President Cheney, Mr. Chief Justice, President Carter, ..."
#> 
#> 2005-Bush.2 :
#> "At this second gathering, our duties are defined not by the ..."
#> 
#> 2005-Bush.3 :
#> "We have seen our vulnerability, and we have seen its deepest..."
#> 
#> 2005-Bush.4 :
#> "We are led, by events and common sense, to one conclusion: T..."
#> 
#> 2005-Bush.5 :
#> "America's vital interests and our deepest beliefs are now on..."
#> 
#> 2005-Bush.6 :
#> "So it is the policy of the United States to seek and support..."
#> 
#> [ reached max_ndoc ... 469 more documents ]
summary(corp2para, 50, showmeta = TRUE)
#> Corpus consisting of 475 documents, showing 50 documents:
#> 
#>           Text Types Tokens Sentences Year President FirstName      Party
#>    2005-Bush.1    66    100         2 2005      Bush George W. Republican
#>    2005-Bush.2    50     70         3 2005      Bush George W. Republican
#>    2005-Bush.3    64    100         3 2005      Bush George W. Republican
#>    2005-Bush.4    37     50         2 2005      Bush George W. Republican
#>    2005-Bush.5    69    118         6 2005      Bush George W. Republican
#>    2005-Bush.6    89    148         6 2005      Bush George W. Republican
#>    2005-Bush.7    36     51         3 2005      Bush George W. Republican
#>    2005-Bush.8    48     65         3 2005      Bush George W. Republican
#>    2005-Bush.9    78    113         6 2005      Bush George W. Republican
#>   2005-Bush.10    67     98         5 2005      Bush George W. Republican
#>   2005-Bush.11    39     48         3 2005      Bush George W. Republican
#>   2005-Bush.12    25     28         1 2005      Bush George W. Republican
#>   2005-Bush.13    40     44         1 2005      Bush George W. Republican
#>   2005-Bush.14    35     42         2 2005      Bush George W. Republican
#>   2005-Bush.15    43     59         3 2005      Bush George W. Republican
#>   2005-Bush.16    90    138         9 2005      Bush George W. Republican
#>   2005-Bush.17    47     68         2 2005      Bush George W. Republican
#>   2005-Bush.18    61     92         5 2005      Bush George W. Republican
#>   2005-Bush.19    32     41         2 2005      Bush George W. Republican
#>   2005-Bush.20   102    164         6 2005      Bush George W. Republican
#>   2005-Bush.21    72    125         4 2005      Bush George W. Republican
#>   2005-Bush.22    73    107         5 2005      Bush George W. Republican
#>   2005-Bush.23    40     60         3 2005      Bush George W. Republican
#>   2005-Bush.24    88    131         5 2005      Bush George W. Republican
#>   2005-Bush.25    99    151         4 2005      Bush George W. Republican
#>   2005-Bush.26    59     85         4 2005      Bush George W. Republican
#>   2005-Bush.27    16     16         1 2005      Bush George W. Republican
#>   2009-Obama.1     4      4         1 2009     Obama    Barack Democratic
#>   2009-Obama.2    42     53         2 2009     Obama    Barack Democratic
#>   2009-Obama.3    62     86         4 2009     Obama    Barack Democratic
#>   2009-Obama.4    12     15         2 2009     Obama    Barack Democratic
#>   2009-Obama.5    76    108         5 2009     Obama    Barack Democratic
#>   2009-Obama.6    39     46         2 2009     Obama    Barack Democratic
#>   2009-Obama.7    36     46         4 2009     Obama    Barack Democratic
#>   2009-Obama.8    19     22         1 2009     Obama    Barack Democratic
#>   2009-Obama.9    29     33         1 2009     Obama    Barack Democratic
#>  2009-Obama.10    56     82         2 2009     Obama    Barack Democratic
#>  2009-Obama.11    71    105         5 2009     Obama    Barack Democratic
#>  2009-Obama.12    21     21         1 2009     Obama    Barack Democratic
#>  2009-Obama.13    20     24         1 2009     Obama    Barack Democratic
#>  2009-Obama.14    17     20         1 2009     Obama    Barack Democratic
#>  2009-Obama.15    43     51         2 2009     Obama    Barack Democratic
#>  2009-Obama.16    73    107         7 2009     Obama    Barack Democratic
#>  2009-Obama.17    85    143         8 2009     Obama    Barack Democratic
#>  2009-Obama.18    52     61         3 2009     Obama    Barack Democratic
#>  2009-Obama.19    97    146         5 2009     Obama    Barack Democratic
#>  2009-Obama.20    76    115         3 2009     Obama    Barack Democratic
#>  2009-Obama.21    90    137         4 2009     Obama    Barack Democratic
#>  2009-Obama.22    60     83         3 2009     Obama    Barack Democratic
#>  2009-Obama.23    92    141         5 2009     Obama    Barack Democratic
#> 
## Note that Bush 2005 is recorded as a single paragraph because that text
## used a single \n to mark the end of a paragraph.
```
