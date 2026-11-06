# Replace tokens in a tokens object

Substitute token types based on vectorized one-to-one matching. Since
this function is created for lemmatization or user-defined stemming. It
supports substitution of multi-word features by multi-word features, but
substitution is fastest when `pattern` and `replacement` are character
vectors and `valuetype = "fixed"` as the function only substitute types
of tokens. Please use
[`tokens_lookup()`](https://quanteda.io/reference/tokens_lookup.md) with
`exclusive = FALSE` to replace
[dictionary](https://quanteda.io/reference/dictionary.md) values.

## Usage

``` r
tokens_replace(
  x,
  pattern,
  replacement,
  valuetype = "glob",
  case_insensitive = TRUE,
  apply_if = NULL,
  verbose = quanteda_options("verbose")
)
```

## Arguments

- x:

  [tokens](https://quanteda.io/reference/tokens.md) object whose token
  elements will be replaced

- pattern:

  a character vector or list of character vectors. See
  [pattern](https://quanteda.io/reference/pattern.md) for more details.

- replacement:

  a character vector or (if `pattern` is a list) list of character
  vectors of the same length as `pattern`

- valuetype:

  the type of pattern matching: `"glob"` for "glob"-style wildcard
  expressions; `"regex"` for regular expressions; or `"fixed"` for exact
  matching. See [valuetype](https://quanteda.io/reference/valuetype.md)
  for details.

- case_insensitive:

  logical; if `TRUE`, ignore case when matching a `pattern` or
  [dictionary](https://quanteda.io/reference/dictionary.md) values

- apply_if:

  logical vector of length `ndoc(x)`; documents are modified only when
  corresponding values are `TRUE`, others are left unchanged.

- verbose:

  if `TRUE` print the number of tokens and documents before and after
  the function is applied. The number of tokens does not include
  paddings.

## See also

tokens_lookup

## Examples

``` r
toks1 <- tokens(data_corpus_inaugural, remove_punct = TRUE)

# lemmatization
taxwords <- c("tax", "taxing", "taxed", "taxed", "taxation")
lemma <- rep("TAX", length(taxwords))
toks2 <- tokens_replace(toks1, taxwords, lemma, valuetype = "fixed")
kwic(toks2, "TAX") |>
    tail(10)
#> Keyword-in-context with 10 matches.
#>                                                                       
#>   [1981-Reagan, 273]                      for their labor by a | TAX |
#>   [1981-Reagan, 290]             productivity But great as our | TAX |
#>  [1981-Reagan, 1521]               and to lighten our punitive | TAX |
#>   [1985-Reagan, 496]                were right to believe that | TAX |
#>  [1985-Reagan, 1106]                lives We must simplify our | TAX |
#>  [1985-Reagan, 1418] permanently control Government's power to | TAX |
#>  [1985-Reagan, 1438]              spend its citizens money and | TAX |
#>    [2013-Obama, 739]          remake our government revamp our | TAX |
#>   [2025-Trump, 1536]           workers and families Instead of | TAX |
#>   [2025-Trump, 1547]              countries we will tariff and | TAX |
#>                                               
#>  system which penalizes successful achievement
#>  burden is it has not                         
#>  burden And these will be                     
#>  rates have been reduced inflation            
#>  system make it more fair                     
#>  and spend We must act                        
#>  them into servitude when the                 
#>  Code reform our schools and                  
#>  our citizens to enrich other                 
#>  foreign countries to enrich our              

# stemming
type <- types(toks1)
stem <- char_wordstem(type, "porter")
toks3 <- tokens_replace(toks1, type, stem, valuetype = "fixed", case_insensitive = FALSE)
identical(toks3, tokens_wordstem(toks1, "porter"))
#> [1] TRUE

# multi-multi substitution
toks4 <- tokens_replace(toks1, phrase(c("Supreme Court")),
                        phrase(c("Supreme Court of the United States")))
kwic(toks4, phrase(c("Supreme Court of the United States")))
#> Keyword-in-context with 5 matches.
#>                                                               
#>   [1857-Buchanan, 441:446] which legitimately belongs to the |
#>  [1861-Lincoln, 2323:2328]              to be decided by the |
#>  [1861-Lincoln, 2465:2470]         fixed by decisions of the |
#>   [1889-Harrison, 408:413]        by the organization of the |
#>        [2025-Trump, 27:32]   Justice Roberts justices of the |
#>                                                                        
#>  Supreme Court of the United States | of the United States before      
#>  Supreme Court of the United States | nor do I deny that               
#>  Supreme Court of the United States | the instant they are made        
#>  Supreme Court of the United States | shall have been suitably observed
#>  Supreme Court of the United States | of the United States President   
```
