# Randomly sample documents from a corpus

Take a random sample of documents of the specified size from a corpus,
with or without replacement, optionally by grouping variables or with
probability weights.

## Usage

``` r
corpus_sample(x, size = ndoc(x), replace = FALSE, prob = NULL, by = NULL)
```

## Arguments

- x:

  a [corpus](https://quanteda.io/reference/corpus.md) object whose
  documents will be sampled

- size:

  a positive number, the number of documents to select; when used with
  `by`, the number to select from each group or a vector equal in length
  to the number of groups defining the samples to be chosen in each
  category of `by`. By defining a size larger than the number of
  documents, it is possible to oversample when `replace = TRUE`.

- replace:

  if `TRUE`, sample with replacement

- prob:

  a vector of probability weights for obtaining the elements of the
  vector being sampled. May not be applied when `by` is used.

- by:

  optional grouping variable for sampling. This will be evaluated in the
  docvars data.frame, so that docvars may be referred to by name without
  quoting. This also changes previous behaviours for `by`. See
  `news(Version >= "2.9", package = "quanteda")` for details.

## Value

a [corpus](https://quanteda.io/reference/corpus.md) object (re)sampled
on the documents, containing the document variables for the documents
sampled.

## Examples

``` r
set.seed(123)
# sampling from a corpus
summary(corpus_sample(data_corpus_inaugural, size = 5))
#> Corpus consisting of 5 documents, showing 5 documents:
#> 
#>           Text Types Tokens Sentences Year President      FirstName      Party
#>      1909-Taft  1437   5821       158 1909      Taft William Howard Republican
#>      1845-Polk  1334   5186       153 1845      Polk     James Knox       Whig
#>      1989-Bush   795   2674       144 1989      Bush         George Republican
#>  1841-Harrison  1896   9125       210 1841  Harrison  William Henry       Whig
#>     1797-Adams   826   2577        37 1797     Adams           John Federalist
#> 
summary(corpus_sample(data_corpus_inaugural, size = 10, replace = TRUE))
#> Corpus consisting of 10 documents, showing 10 documents:
#> 
#>               Text Types Tokens Sentences Year  President     FirstName
#>  1953-Eisenhower.1   900   2743       123 1953 Eisenhower     Dwight D.
#>      1985-Reagan.1   925   2909       126 1985     Reagan        Ronald
#>        2001-Bush.1   621   1806        98 2001       Bush     George W.
#>  1957-Eisenhower.1   621   1907        92 1957 Eisenhower     Dwight D.
#>   1933-Roosevelt.1   743   2057        86 1933  Roosevelt   Franklin D.
#>     1993-Clinton.1   642   1833        82 1993    Clinton          Bill
#>    1841-Harrison.1  1896   9125       210 1841   Harrison William Henry
#>        2001-Bush.2   621   1806        98 2001       Bush     George W.
#>   1885-Cleveland.1   676   1816        44 1885  Cleveland        Grover
#>    1889-Harrison.1  1352   4721       157 1889   Harrison      Benjamin
#>       Party
#>  Republican
#>  Republican
#>  Republican
#>  Republican
#>  Democratic
#>  Democratic
#>        Whig
#>  Republican
#>  Democratic
#>  Republican
#> 

# sampling with by
corp <- data_corpus_inaugural
corp$century <- paste(floor(corp$Year / 100) + 1)
corp$century <- paste0(corp$century, ifelse(corp$century < 21, "th", "st"))
corpus_sample(corp, size = 2, by = century) |>
    summary()
#> Corpus consisting of 8 documents, showing 8 documents:
#> 
#>             Text Types Tokens Sentences Year  President   FirstName
#>       1797-Adams   826   2577        37 1797      Adams        John
#>  1793-Washington    96    147         5 1793 Washington      George
#>      1817-Monroe  1040   3677       121 1817     Monroe       James
#>       1873-Grant   552   1472        44 1873      Grant  Ulysses S.
#>     1997-Clinton   773   2436       113 1997    Clinton        Bill
#>   1933-Roosevelt   743   2057        86 1933  Roosevelt Franklin D.
#>       2017-Trump   582   1660        89 2017      Trump   Donald J.
#>       2009-Obama   938   2689       112 2009      Obama      Barack
#>                  Party century
#>             Federalist    18th
#>                   none    18th
#>  Democratic-Republican    19th
#>             Republican    19th
#>             Democratic    20th
#>             Democratic    20th
#>             Republican    21st
#>             Democratic    21st
#> 
# needs drop = TRUE to avoid empty interactions
corpus_sample(corp, size = 1, by = interaction(Party, century, drop = TRUE), replace = TRUE) |>
    summary()
#> Corpus consisting of 10 documents, showing 10 documents:
#> 
#>             Text Types Tokens Sentences Year  President   FirstName
#>       1797-Adams   826   2577        37 1797      Adams        John
#>  1793-Washington    96    147         5 1793 Washington      George
#>   1893-Cleveland   821   2125        58 1893  Cleveland      Grover
#>   1805-Jefferson   804   2380        45 1805  Jefferson      Thomas
#>     1861-Lincoln  1075   3999       138 1861    Lincoln     Abraham
#>        1845-Polk  1334   5186       153 1845       Polk  James Knox
#>   1933-Roosevelt   743   2057        86 1933  Roosevelt Franklin D.
#>     1921-Harding  1169   3719       150 1921    Harding   Warren G.
#>       2013-Obama   814   2317        90 2013      Obama      Barack
#>        2001-Bush   621   1806        98 2001       Bush   George W.
#>                  Party century
#>             Federalist    18th
#>                   none    18th
#>             Democratic    19th
#>  Democratic-Republican    19th
#>             Republican    19th
#>                   Whig    19th
#>             Democratic    20th
#>             Republican    20th
#>             Democratic    21st
#>             Republican    21st
#> 

# sampling sentences by document
corp <- corpus(c(one = "Sentence one.  Sentence two.  Third sentence.",
                 two = "First sentence, doc2.  Second sentence, doc2."),
               docvars = data.frame(var1 = c("a", "a"), var2 = c(1, 2)))
corpus_reshape(corp, to = "sentences") %>%
    corpus_sample(replace = TRUE, by = docid(.))
#> Corpus consisting of 5 documents and 2 docvars.
#> one.2 :
#> "Third sentence."
#> 
#> one.3 :
#> "Third sentence."
#> 
#> one.1 :
#> "Sentence one."
#> 
#> two.2 :
#> "Second sentence, doc2."
#> 
#> two.1 :
#> "First sentence, doc2."
#> 

# oversampling
corpus_sample(corp, size = 5, replace = TRUE)
#> Corpus consisting of 5 documents and 2 docvars.
#> two.1 :
#> "First sentence, doc2. Second sentence, doc2."
#> 
#> two.2 :
#> "First sentence, doc2. Second sentence, doc2."
#> 
#> one.1 :
#> "Sentence one. Sentence two. Third sentence."
#> 
#> one.2 :
#> "Sentence one. Sentence two. Third sentence."
#> 
#> one.3 :
#> "Sentence one. Sentence two. Third sentence."
#> 
```
