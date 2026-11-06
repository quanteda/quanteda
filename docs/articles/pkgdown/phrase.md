# Working with multi-word expressions

**quanteda** has the functionality to select, remove or compound
multi-word expressions, such as phrasal verbs (“try on”, “wake up” etc.)
and place names (“New York”, “South Korea” etc.).

``` r

library(quanteda)
```

## Tokenize

``` r

toks <- tokens(data_corpus_inaugural)
```

## Define multi-word expressions

Functions for tokens objects take a character vector, a dictionary or
collocations as `pattern`. All those three can be used for multi-word
expressions, but you have to be aware their differences.

### Character vector

The most basic way to define multi-word expressions is separating words
by whitespaces and wrap the character vector by
[`phrase()`](https://quanteda.io/reference/phrase.md).

``` r

multiword <- c("United States", "New York")
```

#### Keyword-in-context

[`kwic()`](https://quanteda.io/reference/kwic.md) is useful to find
multi-word expressions in tokens. If you are not sure if “United” and
“States” are separated, check their positions (e.g. “434:435”).

``` r

head(kwic(toks, pattern = phrase(multiword)))
## Keyword-in-context with 6 matches.
##                                                                              
##  [1789-Washington, 433:434]            of the people of the | United States |
##  [1789-Washington, 529:530]          more than those of the | United States |
##       [1797-Adams, 524:525]     saw the Constitution of the | United States |
##     [1797-Adams, 1716:1717]      to the Constitution of the | United States |
##     [1797-Adams, 2480:2481] support the Constitution of the | United States |
##   [1805-Jefferson, 441:442]       sees a taxgatherer of the | United States |
##                                       
##  a Government instituted by themselves
##  . Every step by which                
##  in a foreign country.                
##  , and a conscientious determination  
##  , I entertain no doubt               
##  ? These contributions enable us
```

#### Select tokens

Similarly, you can select or remove multi-word expression using
[`tokens_select()`](https://quanteda.io/reference/tokens_select.md).

``` r

head(tokens_select(toks, pattern = phrase(multiword)))
## Tokens consisting of 6 documents and 4 docvars.
## 1789-Washington :
## [1] "United" "States" "United" "States"
## 
## 1793-Washington :
## character(0)
## 
## 1797-Adams :
## [1] "United" "States" "United" "States" "United" "States"
## 
## 1801-Jefferson :
## character(0)
## 
## 1805-Jefferson :
## [1] "United" "States"
## 
## 1809-Madison :
## [1] "United" "States" "United" "States"
```

#### Compound tokens

[`tokens_compound()`](https://quanteda.io/reference/tokens_compound.md)
joins elements of multi-word expressions by underscore, so they become
“United_States” and “New_York”.

``` r

comp_toks <- tokens_compound(toks, pattern = phrase(multiword))
head(tokens_select(comp_toks, pattern = c("United_States", "New_York")))
## Tokens consisting of 6 documents and 4 docvars.
## 1789-Washington :
## [1] "United_States" "United_States"
## 
## 1793-Washington :
## character(0)
## 
## 1797-Adams :
## [1] "United_States" "United_States" "United_States"
## 
## 1801-Jefferson :
## character(0)
## 
## 1805-Jefferson :
## [1] "United_States"
## 
## 1809-Madison :
## [1] "United_States" "United_States"
```

### Dictionary

Elements of multi-word expressions should be separately by whitespaces
in a dictionary, but you do not use
[`phrase()`](https://quanteda.io/reference/phrase.md) here.

``` r

dict_multiword <- dictionary(list(country = "United States", 
                                  city = "New York"))
```

#### Lookup dictionary

``` r

head(tokens_lookup(toks, dictionary = dict_multiword))
## Tokens consisting of 6 documents and 4 docvars.
## 1789-Washington :
## [1] "country" "country"
## 
## 1793-Washington :
## character(0)
## 
## 1797-Adams :
## [1] "country" "country" "country"
## 
## 1801-Jefferson :
## character(0)
## 
## 1805-Jefferson :
## [1] "country"
## 
## 1809-Madison :
## [1] "country" "country"
```

### Collocations

With
[`textstat_collocations()`](https://quanteda.io/reference/textstat_collocations.html),
it is possible to discover multi-word expressions through statistical
scoring of the associations of adjacent words.

#### Discover collocations

If
[`textstat_collocations()`](https://quanteda.io/reference/textstat_collocations.html)
is applied to a tokens object comprised only of capitalize words, it
usually returns multi-word proper names.

``` r

library("quanteda.textstats")
col <- toks |> 
    tokens_remove(stopwords("en")) |> 
    tokens_select(pattern = "^[A-Z]", valuetype = "regex", 
                  case_insensitive = FALSE, padding = TRUE) |>
    textstat_collocations(min_count = 5, tolower = FALSE)
head(col)
##           collocation count count_nested length   lambda        z
## 1       United States   165            0      2 8.659452 29.54123
## 2  Federal Government    32            0      2 5.614788 22.03405
## 3       Chief Justice    15            0      2 8.931727 19.02304
## 4        Almighty God    15            0      2 7.044082 18.13508
## 5 Constitution United    19            0      2 3.995065 15.91124
## 6         North South     8            0      2 8.082654 15.63148
```

#### Compound collocations

Collocations are automatically recognized as multi-word expressions by
[`tokens_compound()`](https://quanteda.io/reference/tokens_compound.md)
in *case-sensitive fixed pattern matching*. This is the fastest way to
compound large numbers of multi-word expressions, but make sure that
`tolower = FALSE` in
[`textstat_collocations()`](https://quanteda.io/reference/textstat_collocations.html)
to do this.

``` r

comp_toks2 <- tokens_compound(toks, pattern = col)
head(kwic(comp_toks2, pattern = c("United_States", "New_York")))
## Keyword-in-context with 6 matches.
##                                                                          
##  [1789-Washington, 433]            of the people of the | United_States |
##  [1789-Washington, 528]          more than those of the | United_States |
##       [1797-Adams, 524]     saw the Constitution of the | United_States |
##      [1797-Adams, 1715]      to the Constitution of the | United_States |
##      [1797-Adams, 2478] support the Constitution of the | United_States |
##   [1805-Jefferson, 441]       sees a taxgatherer of the | United_States |
##                                       
##  a Government instituted by themselves
##  . Every step by which                
##  in a foreign country.                
##  , and a conscientious determination  
##  , I entertain no doubt               
##  ? These contributions enable us
```

You can use [`phrase()`](https://quanteda.io/reference/phrase.md) on
collocations if more flexibility is needed. This is usually the case if
you compound tokens from different corpus.

``` r

comp_toks3 <- tokens_compound(toks, pattern = phrase(col$collocation))
head(kwic(comp_toks3, pattern = c("United_States", "New_York")))
## Keyword-in-context with 6 matches.
##                                                                          
##  [1789-Washington, 433]            of the people of the | United_States |
##  [1789-Washington, 528]          more than those of the | United_States |
##       [1797-Adams, 524]     saw the Constitution of the | United_States |
##      [1797-Adams, 1715]      to the Constitution of the | United_States |
##      [1797-Adams, 2478] support the Constitution of the | United_States |
##   [1805-Jefferson, 441]       sees a taxgatherer of the | United_States |
##                                       
##  a Government instituted by themselves
##  . Every step by which                
##  in a foreign country.                
##  , and a conscientious determination  
##  , I entertain no doubt               
##  ? These contributions enable us
```
