To demonstrate the functionality of `tokens()` that removes or keeps some special characters/symbols. Test on:

    ## quanteda version 0.9.9.42

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

<table style="width:89%;">
<colgroup>
<col width="33%" />
<col width="20%" />
<col width="23%" />
<col width="11%" />
</colgroup>
<thead>
<tr class="header">
<th></th>
<th><strong>quanteda</strong></th>
<th><strong>tokenizers</strong></th>
<th>Notes</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td>Numbers: Remove</td>
<td><code>tokens(x, removeNumbers = TRUE)</code></td>
<td>n/a</td>
<td></td>
</tr>
<tr class="even">
<td>Twitter symbols: Keep</td>
<td><code>tokens(x, removeTwitter = FALSE)</code></td>
<td>n/a</td>
<td></td>
</tr>
<tr class="odd">
<td>Punctuation: Remove</td>
<td><code>tokens(x, removePunct = TRUE)</code></td>
<td><code>tokenize_words(x)</code></td>
<td></td>
</tr>
<tr class="even">
<td>Separators: Keep</td>
<td><code>tokens(x, removeSeparators = T, removePunct = FALSE)</code></td>
<td>n/a</td>
<td>removePunct = FALSE</td>
</tr>
<tr class="odd">
<td>Hyphens: Keep</td>
<td><code>tokens(x, removeHyphens = FALSE)</code></td>
<td>n/a</td>
<td></td>
</tr>
<tr class="even">
<td>Hyphens: Remove</td>
<td><code>tokens(x, removeHyphens = TRUE)</code></td>
<td><code>tokenize_words(x)</code></td>
<td></td>
</tr>
<tr class="odd">
<td>URLs: Remove</td>
<td><code>tokens(x, removeURL = TRUE)</code></td>
<td>n/a</td>
<td></td>
</tr>
<tr class="even">
<td>Lowcase</td>
<td><code>tokens(char_tolower(x))</code></td>
<td><code>tokenize_words(x, lowcase = TRUE)</code></td>
<td></td>
</tr>
<tr class="odd">
<td>stopwords</td>
<td><code>removeFeatures(tokens(x, what=”word”, removePunct = T), Stopwords(“english”))</code></td>
<td><code>tokenize_words(x,stopwords = stopwords(“en”))</code></td>
<td></td>
</tr>
</tbody>
</table>

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

### Keep Separators in the Unicode "Separator" \[Z\] class

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

<table style="width:89%;">
<colgroup>
<col width="33%" />
<col width="20%" />
<col width="23%" />
<col width="11%" />
</colgroup>
<thead>
<tr class="header">
<th></th>
<th><strong>quanteda</strong></th>
<th><strong>tokenizers</strong></th>
<th>Notes</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td>Punctuation: Remove</td>
<td><code>tokens(x, what = &quot;character&quot;, removePunct = TRUE)</code></td>
<td><code>tokenize_characters(x, strip_non_alphanum = TRUE)</code></td>
<td></td>
</tr>
<tr class="even">
<td>Separators: Keep</td>
<td><code>tokens(x, removeSeparators = FALSE)</code></td>
<td>n/a</td>
<td></td>
</tr>
<tr class="odd">
<td>Symbol: Remove</td>
<td><code>tokens(x, removeSymbols = TRUE)</code></td>
<td>n/a</td>
<td></td>
</tr>
</tbody>
</table>

### Remove Symbols in the Unicode "Symbol" \[S\] class

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

### Keep Separators in the Unicode "Separator" \[Z\] class

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
    ##        expr      min       lq    mean   median       uq       max neval
    ##    q_tokens 1.105108 1.164258 1.11832 1.118052 1.156324 0.9681471    20
    ##  tokenizers 1.000000 1.000000 1.00000 1.000000 1.000000 1.0000000    20
    ##  cld
    ##    a
    ##    a

### fasterword

``` r
microbenchmark::microbenchmark(q_tokens = tokens(data_char_inaugural, what = "fasterword", hash = FALSE,     
                                                 removeSeparators = FALSE, removeTwitter = TRUE),
                               tokenizers = tokenize_words(data_char_inaugural), 
                               times = 20, unit = "relative")
```

    ## Unit: relative
    ##        expr      min       lq     mean   median       uq     max neval cld
    ##    q_tokens 1.000000 1.000000 1.000000 1.000000 1.000000 1.00000    20  a 
    ##  tokenizers 2.410693 2.140916 2.418863 2.078901 2.048608 6.87982    20   b

### fastestword

``` r
microbenchmark::microbenchmark(q_tokens = tokens(data_char_inaugural, what = "fastestword", hash = FALSE, 
                                                 removeSeparators = FALSE, removeTwitter = TRUE),
                               tokenizers = tokenize_words(data_char_inaugural), 
                               times = 20, unit = "relative")
```

    ## Unit: relative
    ##        expr      min       lq     mean   median       uq      max neval
    ##    q_tokens 1.000000 1.000000 1.000000 1.000000 1.000000 1.000000    20
    ##  tokenizers 3.346552 3.298714 2.924473 2.984952 2.560084 2.619707    20
    ##  cld
    ##   a 
    ##    b

### character

``` r
microbenchmark::microbenchmark(q_tokens = tokens(data_char_inaugural, what = "character", hash = FALSE, removeSeparators = FALSE),
                                tokenizers = tokenize_characters(data_char_inaugural), 
                                times = 20, unit = "relative")
```

    ## Unit: relative
    ##        expr      min       lq     mean   median       uq      max neval
    ##    q_tokens 1.324672 1.354467 1.569866 1.371468 1.469847 1.167919    20
    ##  tokenizers 1.000000 1.000000 1.000000 1.000000 1.000000 1.000000    20
    ##  cld
    ##    b
    ##   a

### sentence

``` r
microbenchmark::microbenchmark(q_tokens = tokens(data_char_inaugural, what = "sentence", hash = FALSE),
                                tokenizers = tokenize_sentences(data_char_inaugural), 
                                times = 20, unit = "relative")
```

    ## Unit: relative
    ##        expr     min       lq    mean   median       uq      max neval cld
    ##    q_tokens 1.72892 1.806512 1.76372 1.771886 1.750814 1.739032    20   b
    ##  tokenizers 1.00000 1.000000 1.00000 1.000000 1.000000 1.000000    20  a
