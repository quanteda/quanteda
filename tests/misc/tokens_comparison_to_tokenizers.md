Purpose of this document
------------------------

To demonstrate the functionality of `tokens()` that removes or keeps some special characters/symbols. Test on:

    ## quanteda version 0.9.9.45

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
<td><code>tokens(x, remove_numbers = TRUE)</code></td>
<td>n/a</td>
<td></td>
</tr>
<tr class="even">
<td>Twitter symbols: Keep</td>
<td><code>tokens(x, remove_twitter = FALSE)</code></td>
<td>n/a</td>
<td></td>
</tr>
<tr class="odd">
<td>Punctuation: Remove</td>
<td><code>tokens(x, remove_punct = TRUE)</code></td>
<td><code>tokenize_words(x)</code></td>
<td></td>
</tr>
<tr class="even">
<td>Separators: Keep</td>
<td><code>tokens(x, remove_separators = TRUE, remove_punct = FALSE)</code></td>
<td>n/a</td>
<td>remove_punct = FALSE</td>
</tr>
<tr class="odd">
<td>Hyphens: Keep</td>
<td><code>tokens(x, remove_hyphens = FALSE)</code></td>
<td>n/a</td>
<td></td>
</tr>
<tr class="even">
<td>Hyphens: Remove</td>
<td><code>tokens(x, remove_hyphens = TRUE)</code></td>
<td><code>tokenize_words(x)</code></td>
<td></td>
</tr>
<tr class="odd">
<td>urls: Remove</td>
<td><code>tokens(x, remove_url = TRUE)</code></td>
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
<td><code>tokens_remove(tokens(x, what = &quot;word&quot;&quot;, remove_punct = TRUE), stopwords(&quot;english&quot;&quot;))</code></td>
<td><code>tokenize_words(x, stopwords = stopwords(&quot;en&quot;&quot;))</code></td>
<td></td>
</tr>
</tbody>
</table>

### Preserve words with hyphens

``` r
tokens("They stretched in never-ending line.\n", what = "word", remove_Punct = TRUE, remove_hyphens = FALSE)
```

    ## Warning in tokens.character("They stretched in never-ending line.\n", what
    ## = "word", : Argument remove_Punct not used.

    ## tokens from 1 document.
    ## Component 1 :
    ## [1] "They"      "stretched" "in"        "never"     "-"         "ending"   
    ## [7] "line"      "."

``` r
tokenize_words("They stretched in never-ending line.\n")
```

    ## [[1]]
    ## [1] "they"      "stretched" "in"        "never"     "ending"    "line"

### Eliminate urls beginning with "http(s)"

``` r
tokens("4u http://www.github.com\n", what = "word", remove_punct = TRUE, remove_url = TRUE)
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
txt <- "in sprightly @LSE and tell us what makes you feel #partofLSE\n"
tokens(txt, what = "word", remove_punct = TRUE, remove_twitter = FALSE)
```

    ## tokens from 1 document.
    ## Component 1 :
    ##  [1] "in"         "sprightly"  "@LSE"       "and"        "tell"      
    ##  [6] "us"         "what"       "makes"      "you"        "feel"      
    ## [11] "#partofLSE"

``` r
tokenize_words(txt)
```

    ## [[1]]
    ##  [1] "in"        "sprightly" "lse"       "and"       "tell"     
    ##  [6] "us"        "what"      "makes"     "you"       "feel"     
    ## [11] "partoflse"

### Remove numbers but preserve words starting with digits

``` r
txt <- c("1 $100 £1000 2000+ \n", "4u http://www.github.com\n")
tokens(txt, what = "word", remove_punct = TRUE, remove_numbers = TRUE)
```

    ## tokens from 2 documents.
    ## Component 1 :
    ## character(0)
    ## 
    ## Component 2 :
    ## [1] "4u"             "http"           "www.github.com"

``` r
tokenize_words(txt)
```

    ## [[1]]
    ## [1] "1"    "100"  "1000" "2000"
    ## 
    ## [[2]]
    ## [1] "4u"             "http"           "www.github.com"

### Keep Separators in the Unicode "Separator" \[Z\] class

``` r
txt <- "1 $ 100 £1000 2000+ wow!\n"
tokens(txt, what = "word", remove_punct = FALSE, remove_separators = FALSE)
```

    ## tokens from 1 document.
    ## Component 1 :
    ##  [1] "1"    " "    "$"    " "    "100"  " "    "£"    "1000" " "    "2000"
    ## [11] "+"    " "    "wow"  "!"    "\n"

``` r
tokenize_words(txt)
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
<td><code>tokens(x, what = &quot;character&quot;, remove_punct = TRUE)</code></td>
<td><code>tokenize_characters(x, strip_non_alphanum = TRUE)</code></td>
<td></td>
</tr>
<tr class="even">
<td>Separators: Keep</td>
<td><code>tokens(x, remove_separators = FALSE)</code></td>
<td>n/a</td>
<td></td>
</tr>
<tr class="odd">
<td>Symbols: Remove</td>
<td><code>tokens(x, remove_symbols = TRUE)</code></td>
<td>n/a</td>
<td></td>
</tr>
</tbody>
</table>

### Remove Symbols in the Unicode "Symbol" \[S\] class

``` r
txt <- "1 $ 100 £1000 2000+ wow!"
tokens(txt, what = "character", remove_punct = TRUE, remove_symbols = TRUE)
```

    ## tokens from 1 document.
    ## Component 1 :
    ##  [1] "1" "1" "0" "0" "1" "0" "0" "0" "2" "0" "0" "0" "w" "o" "w"

``` r
tokenize_characters(txt, strip_non_alphanum = TRUE)
```

    ## [[1]]
    ##  [1] "1" "$" "1" "0" "0" "£" "1" "0" "0" "0" "2" "0" "0" "0" "+" "w" "o"
    ## [18] "w"

### Keep Separators in the Unicode "Separator" \[Z\] class

``` r
txt <- "1 $ 100 £1000 2000+ wow!\n"
tokens(txt, what = "character", remove_separators = FALSE)
```

    ## tokens from 1 document.
    ## Component 1 :
    ##  [1] "1"  " "  "$"  " "  "1"  "0"  "0"  " "  "£"  "1"  "0"  "0"  "0"  " " 
    ## [15] "2"  "0"  "0"  "0"  "+"  " "  "w"  "o"  "w"  "!"  "\n"

``` r
tokenize_characters(txt)
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

### words

``` r
microbenchmark::microbenchmark(quanteda_word = tokens(data_char_inaugural, what = "word", hash = FALSE,                  
                                                      remove_punct = TRUE, 
                                                      remove_twitter = TRUE, 
                                                      remove_hyphens = TRUE),
                               quanteda_faster = tokens(data_char_inaugural, what = "fasterword", hash = FALSE,                  
                                                      remove_punct = TRUE, 
                                                      remove_twitter = TRUE, 
                                                      remove_hyphens = TRUE),
                               quanteda_fastest = tokens(data_char_inaugural, what = "fasterword", hash = FALSE,
                                                      remove_punct = TRUE, 
                                                      remove_twitter = TRUE, 
                                                      remove_hyphens = TRUE),
                               tokenizers = tokenize_words(data_char_inaugural), 
                               times = 20, unit = "relative")
```

    ## Unit: relative
    ##              expr       min        lq     mean   median       uq      max
    ##     quanteda_word 1.3111336 1.3167288 1.317703 1.316557 1.328200 1.310493
    ##   quanteda_faster 1.0000000 1.0000000 1.000000 1.000000 1.000000 1.000000
    ##  quanteda_fastest 0.9581739 0.9983004 1.219317 1.012794 1.049090 3.654207
    ##        tokenizers 1.2879339 1.3243397 1.277266 1.279322 1.229943 1.261172
    ##  neval cld
    ##     20   a
    ##     20   a
    ##     20   a
    ##     20   a

### characters

``` r
microbenchmark::microbenchmark(q_tokens = tokens(data_char_inaugural, what = "character", 
                                                 remove_separators = TRUE,
                                                 remove_punct = TRUE, hash = FALSE),
                                tokenizers = tokenize_characters(data_char_inaugural), 
                                times = 20, unit = "relative")
```

    ## Unit: relative
    ##        expr      min       lq     mean   median       uq      max neval
    ##    q_tokens 6.612255 7.456689 7.606318 7.950968 7.540119 8.362242    20
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
    ##        expr      min       lq     mean   median       uq      max neval
    ##    q_tokens 1.877804 1.847079 1.739107 1.706105 1.653021 1.682684    20
    ##  tokenizers 1.000000 1.000000 1.000000 1.000000 1.000000 1.000000    20
    ##  cld
    ##    b
    ##   a

Wishlist
--------

1.  `tokenize_sentences()`: Add an exception list, similar to stopwords, that could be used for exceptions to sentence segmentation. For instance:

    ``` r
        # Replace . delimiter from common title abbreviations, with _pd_
        exceptions <- c("Mr", "Mrs", "Ms", "Dr", "Jr", "Prof", "Ph.D", "M", "MM", "St", "etc")
        findregex <- paste0("\\b(", exceptions, ")\\.")
        txt <- stri_replace_all_regex(txt, findregex, "$1_pd_", vectorize_all = FALSE)

        ## Remove newline chars 
        txt <- lapply(txt, stringi::stri_replace_all_fixed, "\n", " ")

        ## Perform the tokenization
        tok <- stringi::stri_split_boundaries(txt, type = "sentence")

        ## Cleaning
        tok <- lapply(tok, function(x){
            x <- x[which(x != "")] # remove any "sentences" that were completely blanked out
            x <- stringi::stri_trim_right(x) # trim trailing spaces
            x <- stri_replace_all_fixed(x, "_pd_", ".") # replace the non-full-stop "." characters
            return(x)
        } )
    ```

2.  Need to be able to preserve intra-word hyphens, e.g.

    ``` r
    tokenize_words("Keep co-operate as one word.", split_hyphenated = FALSE)
    [[1]]
    [1] "keep"    "co-operate" "as"      "one"     "word"
    ```

3.  Need an option for preserving Twitter characters, e.g.

    ``` r
    # currently
    tokenize_words("@kenbenoit loves #rstats!")
    ```

        ## [[1]]
        ## [1] "kenbenoit" "loves"     "rstats"

    ``` r
    # want this
    tokens("@kenbenoit loves #rstats!", remove_punct = TRUE, remove_twitter = FALSE)
    ```

        ## tokens from 1 document.
        ## Component 1 :
        ## [1] "@kenbenoit" "loves"      "#rstats"

    We do this through a somewhat expensive process of substitute, tokenize, replace.

4.  Would like an option to keep intra-token separators.

    ``` r
    # currently
    tokenize_words("one\ttwo\n  three four")
    ```

        ## [[1]]
        ## [1] "one"   "two"   "three" "four"

    ``` r
    # want this
    tokens("one\ttwo\n  three four", remove_separators = TRUE)
    ```

        ## tokens from 1 document.
        ## Component 1 :
        ## [1] "one"   "two"   "three" "four"

    ``` r
    tokens("one\ttwo\n  three four", remove_separators = FALSE)
    ```

        ## tokens from 1 document.
        ## Component 1 :
        ## [1] "one"   "\t"    "two"   "\n"    " "     " "     "three" " "     "four"

5.  Would like an option to preserve punctuation.

    ``` r
    txt <- "Hey: Y, M, C, A!!"
    # currently
    tokenize_words(txt, lowercase = FALSE)
    ```

        ## [[1]]
        ## [1] "Hey" "Y"   "M"   "C"   "A"

    ``` r
    # want this
    tokens(txt, remove_punct = TRUE)
    ```

        ## tokens from 1 document.
        ## Component 1 :
        ## [1] "Hey" "Y"   "M"   "C"   "A"

    ``` r
    tokens(txt, remove_punct = FALSE)
    ```

        ## tokens from 1 document.
        ## Component 1 :
        ##  [1] "Hey" ":"   "Y"   ","   "M"   ","   "C"   ","   "A"   "!"   "!"

6.  Need URL handling

    ``` r
    txt <- "The URL is http://textworkshop17.ropensci.org#schedule."
    # want this
    tokenize_words(txt, lowercase = FALSE, strip_url = TRUE)
    [[1]]
    [1] "The"            "URL"            "is"
    tokenize_words(txt, lowercase = FALSE, strip_url = FALSE)
    [[1]]
    [1] "The"            "URL"            "is"
    [4] "http://textworkshop17.ropensci.org#schedule"
    ```

Other observations
------------------

-   **On stopwords:** Our view in the **quanteda** world is that stopword removal is a separate function from tokenization. Stopword removal is a form of token selection, after the core activity of identifyin and segmenting tokens.

-   **On lowercase:** Likewise we view this as a transformation of the tokens, which is a separate issue.

We're not hung up on either issue however!
