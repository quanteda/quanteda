To demonstrate the functionality of `tokens()` that removes or keeps some special characters/symbols. Test on:

    ## quanteda version 0.9.9.39

    ## Using 7 of 8 cores for parallel computing

``` r
poetry <- paste0("I wandered lonely as a cloud,\n",
                 "That floats on high o'er vales and hills.\n",
                 "They stretched in never-ending line.\n",
                 "Tossing their heads in sprightly @LSE and tell us what makes you feel #partofLSE.\n",
                 "\n",
                 "1 $100 £1000 2000+. \n",
                 "Prof. Plum kills Mrs. Peacock. \n",
                 "4u @ http://www.github.com\n")
```

Word tokenization
-----------------

Feature comparison:

|                       | **quanteda**                          | **tokenizers** | Notes |
|-----------------------|---------------------------------------|----------------|-------|
| Numbers: Remove       | `tokens(x, removeNumbers = TRUE)`     | ??             |       |
| Twitter symbols: Keep | `tokens(x, removeTwitter = FALSE)`    | n/a            |       |
| Punctuation: Remove   | `tokens(x, removePunct = TRUE)`       | ??             |       |
| Separators: Keep      | `tokens(x, removeSeparators = FALSE)` | n/a            |       |
| Hyphens: Keep         |                                       |                |       |
| Hyphens: Remove       |                                       |                |       |
| URLs: Remove          |                                       |                |       |

### Preserve words with hyphens

``` r
tokens("They stretched in never-ending line.\n", what = "word", removePunct = TRUE, removeHyphens = FALSE)
```

    ## tokens from 1 document.
    ## Component 1 :
    ## [1] "They"         "stretched"    "in"           "never-ending"
    ## [5] "line"

``` r
tokenize_words("They stretched in never-ending line.\n")
```

    ## [[1]]
    ## [1] "they"      "stretched" "in"        "never"     "ending"    "line"

### Eliminate URLs beginning with "http(s)"

``` r
tokens("4u http://www.github.com\n", what = "word", removePunct = TRUE, removeURL = TRUE)
```

    ## tokens from 1 document.
    ## Component 1 :
    ## [1] "4u"

``` r
tokenize_words("4u http://www.github.com\n")
```

    ## [[1]]
    ## [1] "4u"             "http"           "www.github.com"

### Preserve Twitter characters @ and \#

``` r
tokens("in sprightly @LSE and tell us what makes you feel #partofLSE\n", what = "word", removePunct = TRUE, removeTwitter = FALSE)
```

    ## tokens from 1 document.
    ## Component 1 :
    ##  [1] "in"         "sprightly"  "@LSE"       "and"        "tell"      
    ##  [6] "us"         "what"       "makes"      "you"        "feel"      
    ## [11] "#partofLSE"

``` r
tokenize_words("in sprightly @LSE and tell us what makes you feel #partofLSE\n")
```

    ## [[1]]
    ##  [1] "in"        "sprightly" "lse"       "and"       "tell"     
    ##  [6] "us"        "what"      "makes"     "you"       "feel"     
    ## [11] "partoflse"

### Remove numbers but preserve words starting with digits

``` r
tokens(c("1 $100 £1000 2000+ \n", "4u http://www.github.com\n"), what = "word", removePunct = TRUE, removeNumbers = TRUE)
```

    ## tokens from 2 documents.
    ## Component 1 :
    ## character(0)
    ## 
    ## Component 2 :
    ## [1] "4u"             "http"           "www.github.com"

``` r
tokenize_words(c("1 $100 £1000 2000+ \n", "4u http://www.github.com\n"))
```

    ## [[1]]
    ## [1] "1"    "100"  "1000" "2000"
    ## 
    ## [[2]]
    ## [1] "4u"             "http"           "www.github.com"

### Remove Symbols in the Unicode "Symbol" \[S\] class

``` r
tokens("1 $ 100 £1000 2000+", what = "character", removePunct = TRUE, removeSymbols = TRUE)
```

    ## tokens from 1 document.
    ## Component 1 :
    ##  [1] "1" "1" "0" "0" "1" "0" "0" "0" "2" "0" "0" "0"

``` r
tokenize_characters("1 $ 100 £1000 2000+", strip_non_alphanum = TRUE)
```

    ## [[1]]
    ##  [1] "1" "$" "1" "0" "0" "£" "1" "0" "0" "0" "2" "0" "0" "0" "+"

### Remove Separators in the Unicode "Separator" \[Z\] class but not Punctuations

``` r
tokens("1 $ 100 £1000 2000+\n", what = "word", removePunct = FALSE, removeSeparators = TRUE)
```

    ## tokens from 1 document.
    ## Component 1 :
    ## [1] "1"    "$"    "100"  "£"    "1000" "2000" "+"

``` r
tokenize_words("1 $ 100 £1000 2000+\n")
```

    ## [[1]]
    ## [1] "1"    "100"  "1000" "2000"

### Sentence segmenter handles some exceptions in English

``` r
tokens(poetry, what = "sentence")
```

    ## tokens from 1 document.
    ## Component 1 :
    ## [1] "I wandered lonely as a cloud, That floats on high o'er vales and hills."          
    ## [2] "They stretched in never-ending line."                                             
    ## [3] "Tossing their heads in sprightly @LSE and tell us what makes you feel #partofLSE."
    ## [4] "1 $100 £1000 2000+."                                                              
    ## [5] "Prof. Plum kills Mrs. Peacock.  4u @ http://www.github.com"

``` r
tokenize_sentences(poetry)
```

    ## [[1]]
    ## [1] "I wandered lonely as a cloud, That floats on high o'er vales and hills."          
    ## [2] "They stretched in never-ending line."                                             
    ## [3] "Tossing their heads in sprightly @LSE and tell us what makes you feel #partofLSE."
    ## [4] "1 $100 £1000 2000+."                                                              
    ## [5] "Prof."                                                                            
    ## [6] "Plum kills Mrs."                                                                  
    ## [7] "Peacock.  4u @ http://www.github.com"

### Performance benchmarks

``` r
microbenchmark::microbenchmark(q_tokens = tokens(data_char_inaugural, what = "word", hash = FALSE,                  
                                                 removeSeparators = FALSE, removeTwitter = TRUE),
                               tokenizers = tokenize_words(data_char_inaugural), 
                               times = 20, unit = "relative")
```

    ## Unit: relative
    ##        expr       min       lq     mean   median       uq      max neval
    ##    q_tokens 0.9980465 1.028979 1.208307 1.084156 1.151645 3.244921    20
    ##  tokenizers 1.0000000 1.000000 1.000000 1.000000 1.000000 1.000000    20

``` r
microbenchmark::microbenchmark(q_tokens = tokens(data_char_inaugural, what = "fasterword", hash = FALSE,     
                                                 removeSeparators = FALSE, removeTwitter = TRUE),
                               tokenizers = tokenize_words(data_char_inaugural), 
                               times = 20, unit = "relative")
```

    ## Unit: relative
    ##        expr      min       lq    mean   median       uq      max neval
    ##    q_tokens 1.000000 1.000000 1.00000 1.000000 1.000000 1.000000    20
    ##  tokenizers 2.248852 2.229908 2.21876 2.235659 2.200736 2.117575    20

``` r
microbenchmark::microbenchmark(q_tokens = tokens(data_char_inaugural, what = "fastestword", hash = FALSE, 
                                                 removeSeparators = FALSE, removeTwitter = TRUE),
                               tokenizers = tokenize_words(data_char_inaugural), 
                               times = 20, unit = "relative")
```

    ## Unit: relative
    ##        expr      min       lq    mean   median       uq      max neval
    ##    q_tokens 1.000000 1.000000 1.00000 1.000000 1.000000 1.000000    20
    ##  tokenizers 3.179902 3.104002 2.91172 3.085904 2.829189 2.439512    20
