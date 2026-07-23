# Locate keywords-in-context

For a text or a collection of texts (in a quanteda corpus object),
return a list of a keyword supplied by the user in its immediate
context, identifying the source text and the word index number within
the source text. (Not the line number, since the text may or may not be
segmented using end-of-line delimiters.)

## Usage

``` r
kwic(
  x,
  pattern,
  window = 5,
  valuetype = c("glob", "regex", "fixed"),
  separator = " ",
  case_insensitive = TRUE,
  index = NULL,
  ...
)

is.kwic(x)

# S3 method for class 'kwic'
as.data.frame(x, ...)
```

## Arguments

- x:

  a character, [corpus](https://quanteda.io/reference/corpus.md), or
  [tokens](https://quanteda.io/reference/tokens.md) object

- pattern:

  a character vector, list of character vectors,
  [dictionary](https://quanteda.io/reference/dictionary.md), or
  collocations object. See
  [pattern](https://quanteda.io/reference/pattern.md) for details.

- window:

  the number of context words to be displayed around the keyword

- valuetype:

  the type of pattern matching: `"glob"` for "glob"-style wildcard
  expressions; `"regex"` for regular expressions; or `"fixed"` for exact
  matching. See [valuetype](https://quanteda.io/reference/valuetype.md)
  for details.

- separator:

  a character to separate words in the output

- case_insensitive:

  logical; if `TRUE`, ignore case when matching a `pattern` or
  [dictionary](https://quanteda.io/reference/dictionary.md) values

- index:

  an [index](https://quanteda.io/reference/index.md) object to specify
  keywords

- ...:

  unused

## Value

A `kwic` classed data.frame, with the document name (`docname`) and the
token index positions (`from` and `to`, which will be the same for
single-word patterns, or a sequence equal in length to the number of
elements for multi-word phrases).

## Note

`pattern` will be a keyword pattern or phrase, possibly multiple
patterns, that may include punctuation. If a pattern contains
whitespace, it is best to wrap it in
[`phrase()`](https://quanteda.io/reference/phrase.md) to make this
explicit. However if `pattern` is a `collocations` (see
quanteda.textstats or
[dictionary](https://quanteda.io/reference/dictionary.md) object, then
the collocations or multi-word dictionary keys will automatically be
considered phrases where each whitespace-separated element matches a
token in sequence.

## See also

[print-methods](https://quanteda.io/reference/print-methods.md)

## Examples

``` r
# \donttest{
# single token matching
toks <- tokens(data_corpus_inaugural[1:8])
kwic(toks, pattern = "secure*", valuetype = "glob", window = 3)
#> Keyword-in-context with 6 matches.
#>                                                                       
#>       [1797-Adams, 478]  welfare, and | secure  | the blessings of    
#>      [1797-Adams, 1512]  nations, and | secured | immortal glory with 
#>  [1805-Jefferson, 2367]   , and shall | secure  | to you the          
#>     [1817-Monroe, 1754] cherished. To | secure  | us against these    
#>     [1817-Monroe, 1814] defense as to | secure  | our cities and      
#>     [1817-Monroe, 3009]      I can to | secure  | economy and fidelity
kwic(toks, pattern = "secur", valuetype = "regex", window = 3)
#> Keyword-in-context with 10 matches.
#>                                                         
#>  [1789-Washington, 1496] government for the | security |
#>        [1797-Adams, 478]       welfare, and |  secure  |
#>       [1797-Adams, 1512]       nations, and | secured  |
#>   [1805-Jefferson, 2367]        , and shall |  secure  |
#>      [1813-Madison, 321]       seas and the | security |
#>      [1817-Monroe, 1609]      may form some | security |
#>      [1817-Monroe, 1754]      cherished. To |  secure  |
#>      [1817-Monroe, 1814]      defense as to |  secure  |
#>      [1817-Monroe, 3009]           I can to |  secure  |
#>      [1817-Monroe, 3427]           and as a | security |
#>                         
#>  of their union         
#>  the blessings of       
#>  immortal glory with    
#>  to you the             
#>  of an important        
#>  against these dangers  
#>  us against these       
#>  our cities and         
#>  economy and fidelity   
#>  against foreign dangers
kwic(toks, pattern = "security", valuetype = "fixed", window = 3)
#> Keyword-in-context with 4 matches.
#>                                                         
#>  [1789-Washington, 1496] government for the | security |
#>      [1813-Madison, 321]       seas and the | security |
#>      [1817-Monroe, 1609]      may form some | security |
#>      [1817-Monroe, 3427]           and as a | security |
#>                         
#>  of their union         
#>  of an important        
#>  against these dangers  
#>  against foreign dangers

# phrase matching
kwic(toks, pattern = phrase("secur* against"), window = 2)
#> Keyword-in-context with 2 matches.
#>                                                                         
#>  [1817-Monroe, 1609:1610] form some | security against | these dangers  
#>  [1817-Monroe, 3427:3428]      as a | security against | foreign dangers
kwic(toks, pattern = phrase("war against"), valuetype = "regex", window = 2)
#> Keyword-in-context with 1 match.
#>                                                             
#>  [1801-Jefferson, 1268:1269] the surest | bulwarks against |
#>                           
#>  antirepublican tendencies

# use index
idx <- index(toks, phrase("secur* against"))
kwic(toks, index = idx, window = 2)
#> Keyword-in-context with 2 matches.
#>                                                                         
#>  [1817-Monroe, 1609:1610] form some | security against | these dangers  
#>  [1817-Monroe, 3427:3428]      as a | security against | foreign dangers
# }
kw <- kwic(tokens(data_corpus_inaugural[1:20]), "provident*")
is.kwic(kw)
#> [1] TRUE
is.kwic("Not a kwic")
#> [1] FALSE
is.kwic(kw[, c("pre", "post")])
#> [1] TRUE

toks <- tokens(data_corpus_inaugural[1:8])
kw <- kwic(toks, pattern = "secure*", valuetype = "glob", window = 3)
as.data.frame(kw)
#>          docname from   to            pre keyword                 post pattern
#> 1     1797-Adams  478  478  welfare , and  secure     the blessings of secure*
#> 2     1797-Adams 1512 1512  nations , and secured  immortal glory with secure*
#> 3 1805-Jefferson 2367 2367    , and shall  secure           to you the secure*
#> 4    1817-Monroe 1754 1754 cherished . To  secure     us against these secure*
#> 5    1817-Monroe 1814 1814  defense as to  secure       our cities and secure*
#> 6    1817-Monroe 3009 3009       I can to  secure economy and fidelity secure*
```
