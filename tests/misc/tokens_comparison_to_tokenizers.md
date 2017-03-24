To demonstrate the functionality of `tokens()` that removes or keeps some special characters/symbols. Test on:

    ## quanteda version 0.9.9.41

    ## Using 7 of 8 cores for parallel computing

    ## Warning: package 'tokenizers' was built under R version 3.3.3

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

|                       | **quanteda**                                                                    | **tokenizers**                                  | Notes               |
|-----------------------|---------------------------------------------------------------------------------|-------------------------------------------------|---------------------|
| Numbers: Remove       | `tokens(x, removeNumbers = TRUE)`                                               | n/a                                             |                     |
| Twitter symbols: Keep | `tokens(x, removeTwitter = FALSE)`                                              | n/a                                             |                     |
| Punctuation: Remove   | `tokens(x, removePunct = TRUE)`                                                 | `tokenize_words(x)`                             |                     |
| Separators: Keep      | `tokens(x, removeSeparators = T, removePunct = FALSE)`                          | n/a                                             | removePunct = FALSE |
| Hyphens: Keep         | `tokens(x, removeHyphens = FALSE)`                                              | n/a                                             |                     |
| Hyphens: Remove       | `tokens(x, removeHyphens = TRUE)`                                               | `tokenize_words(x)`                             |                     |
| URLs: Remove          | `tokens(x, removeURL = TRUE)`                                                   | n/a                                             |                     |
| Lowcase               | `tokens(char_tolower(x))`                                                       | `tokenize_words(x, lowcase = TRUE)`             |                     |
| stopwords             | `removeFeatures(tokens(x, what=”word”, removePunct = T), Stopwords(“english”))` | `tokenize_words(x,stopwords = stopwords(“en”))` |                     |

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

### Keep Separators in the Unicode "Separator" [Z] class

``` r
tokens("1 $ 100 £1000 2000+ wow!\n", what = "word", removePunct = FALSE, removeSeparators = FALSE)
```

    ## tokens from 1 document.
    ## Component 1 :
    ##  [1] "1"    " "    "$"    " "    "100"  " "    "£"    "1000" " "    "2000"
    ## [11] "+"    " "    "wow"  "!"    "\n"

``` r
tokenize_words("1 $ 100 £1000 2000+ wow!\n")
```

    ## [[1]]
    ## [1] "1"    "100"  "1000" "2000" "wow"

Character tokenization
----------------------

Feature comparison:

|                     | **quanteda**                                        | **tokenizers**                                      | Notes |
|---------------------|-----------------------------------------------------|-----------------------------------------------------|-------|
| Punctuation: Remove | `tokens(x, what = "character", removePunct = TRUE)` | `tokenize_characters(x, strip_non_alphanum = TRUE)` |       |
| Separators: Keep    | `tokens(x, removeSeparators = FALSE)`               | n/a                                                 |       |
| Symbol: Remove      | `tokens(x, removeSymbols = TRUE)`                   | n/a                                                 |       |

### Remove Symbols in the Unicode "Symbol" [S] class

``` r
tokens("1 $ 100 £1000 2000+ wow!", what = "character", removePunct = TRUE, removeSymbols = TRUE)
```

    ## tokens from 1 document.
    ## Component 1 :
    ##  [1] "1" "1" "0" "0" "1" "0" "0" "0" "2" "0" "0" "0" "w" "o" "w"

``` r
tokenize_characters("1 $ 100 £1000 2000+ wow!", strip_non_alphanum = TRUE)
```

    ## [[1]]
    ##  [1] "1" "$" "1" "0" "0" "£" "1" "0" "0" "0" "2" "0" "0" "0" "+" "w" "o"
    ## [18] "w"

### Keep Separators in the Unicode "Separator" [Z] class

``` r
tokens("1 $ 100 £1000 2000+ wow!\n", what = "character", removeSeparators = FALSE)
```

    ## tokens from 1 document.
    ## Component 1 :
    ##  [1] "1"  " "  "$"  " "  "1"  "0"  "0"  " "  "£"  "1"  "0"  "0"  "0"  " " 
    ## [15] "2"  "0"  "0"  "0"  "+"  " "  "w"  "o"  "w"  "!"  "\n"

``` r
tokenize_characters("1 $ 100 £1000 2000+ wow!\n")
```

    ## [[1]]
    ##  [1] "1" "$" "1" "0" "0" "£" "1" "0" "0" "0" "2" "0" "0" "0" "+" "w" "o"
    ## [18] "w"

Sentence tokenization
---------------------

Feature comparison:

|                        | **quanteda**                   | **tokenizers** | Notes |
|------------------------|--------------------------------|----------------|-------|
| Handle exceptions: Mr. | `tokens(x, what = "sentence")` | n/a            |       |

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

Performance benchmarks
----------------------

### Word

``` r
microbenchmark::microbenchmark(q_tokens = tokens(data_char_inaugural, what = "word", hash = FALSE,                  
                                                 removeSeparators = FALSE, removeTwitter = TRUE),
                               tokenizers = tokenize_words(data_char_inaugural), 
                               times = 20, unit = "relative")
```

    ## Unit: relative
    ##        expr     min       lq     mean  median       uq     max neval
    ##    q_tokens 1.04311 1.023418 1.186118 1.08501 1.238021 1.05419    20
    ##  tokenizers 1.00000 1.000000 1.000000 1.00000 1.000000 1.00000    20

### fasterword

``` r
microbenchmark::microbenchmark(q_tokens = tokens(data_char_inaugural, what = "fasterword", hash = FALSE,     
                                                 removeSeparators = FALSE, removeTwitter = TRUE),
                               tokenizers = tokenize_words(data_char_inaugural), 
                               times = 20, unit = "relative")
```

    ## Unit: relative
    ##        expr      min      lq     mean   median       uq      max neval
    ##    q_tokens 1.000000 1.00000 1.000000 1.000000 1.000000 1.000000    20
    ##  tokenizers 2.228232 2.20218 2.122551 2.130106 2.754655 1.733513    20

### fastestword

``` r
microbenchmark::microbenchmark(q_tokens = tokens(data_char_inaugural, what = "fastestword", hash = FALSE, 
                                                 removeSeparators = FALSE, removeTwitter = TRUE),
                               tokenizers = tokenize_words(data_char_inaugural), 
                               times = 20, unit = "relative")
```

    ## Unit: relative
    ##        expr      min       lq     mean  median       uq       max neval
    ##    q_tokens 1.000000 1.000000 1.000000 1.00000 1.000000 1.0000000    20
    ##  tokenizers 2.616144 2.639756 1.870359 2.54987 2.330261 0.3900769    20

### character

``` r
microbenchmark::microbenchmark(q_tokens = tokens(data_char_inaugural, what = "character", hash = FALSE, removeSeparators = FALSE),
                                tokenizers = tokenize_characters(data_char_inaugural), 
                                times = 20, unit = "relative")
```

    ## Unit: relative
    ##        expr      min       lq     mean   median       uq      max neval
    ##    q_tokens 1.269958 1.496735 2.127945 2.902657 3.175735 1.382952    20
    ##  tokenizers 1.000000 1.000000 1.000000 1.000000 1.000000 1.000000    20

### sentence

``` r
microbenchmark::microbenchmark(q_tokens = tokens(data_char_inaugural, what = "sentence", hash = FALSE),
                                tokenizers = tokenize_sentences(data_char_inaugural), 
                                times = 20, unit = "relative")
```

    ## Unit: relative
    ##        expr      min      lq     mean  median       uq      max neval
    ##    q_tokens 2.136596 2.13568 1.836958 2.10285 1.295282 2.033388    20
    ##  tokenizers 1.000000 1.00000 1.000000 1.00000 1.000000 1.000000    20
