---
title: "quanteda Design and Work Flow"
author: "Ken Benoit and Paul Nulty"
date: "3 June 2015"
output: rmarkdown::html_vignette
vignette: >
  %\VignetteIndexEntry{Corpus-construction}
  %\VignetteEngine{knitr::rmarkdown}
  \usepackage[utf8]{inputenc}
---

## Basic Principles

1. **Corpus texts should remain *unchanged* during subsequent analysis and processing.**  In other words, after *loading* and *encoding*, we should discourage users from modifying a corpus of texts as a form of processing, so that the corpus can act as a library and record of the original texts, prior to any downstream processing.  This not only aids in replication, but also means that a corpus presents the unmodified texts to which any processing, feature selection, transformations, or sampling may be applied or reapplied, without hard-coding any changes made as part of the process of analyzing the texts.  The only exception is to reshape the units of text in a corpus, but we will record the details of this reshaping to make it relatively easy to reverse unit changes.  Since the definition of a "document" is part of the process of loading texts into a corpus, however, rather than processing, we will take a less stringent line on this aspect of changing a corpus.

2. **A corpus should have *settings*, that can be changed.**  These record what a user wants to do for a project, and affects downstream processing, such as creating a dfm.  Indexing will make use of these settings, and if a setting is changed, the index will need to be rebuilt.

3. **A dfm should have settings, that cannot be changed.**  These provide a record of what was done to the text, and where it came from.

4. **A dfm should consist mainly of a (sparse) matrix,** that can be used for any sort of quantitative analysis.  The basic structure will always be *documents* (or document groups) in rows by *features* in columns.

5. **Encoding of texts** should be done in the corpus, and recorded as meta-data in the corpus.  We should be able to detect encodings and suggest (and perform) and conversion when storing texts in a corpus.  This encoding should be `UTF-8` by default.  We will use the tools available in the `stringi` package to detect and set character encodings, namely `stri_enc_detect()` and `stri_conv()`, with reports and suggestions made at the time of corpus creation.

## Major categories of functions

1.  **Corpus construction and management**.  These operate on a corpus, and return a corpus, or report on a corpus.

    
    ```r
    changeunits
    corpus
    docnames, <-
    docvars, <-
    encoding, <-
    language, <-
    metacorpus, metadoc, <-
    ndoc
    ntoken
    segment      # also works on character vectors
    settings, <-
    subset
    summary
    textfile
    texts, <-    # see Question below
    ```
    
    Question: Should `texts() <-` have a replacement function?  This violates basic principle 1 above.  It is included for convenience, since sometimes texts might need to be modified, and this prevents users from addressing internal corpus object slots directly.
    
    
2.  **Text manipulation.**  These operate on character vectors, and return character vectors.

    1.  Operations on character vectors or the character vector of texts from a corpus.
    
        Returns a list of character vectors:
        
        ```r
        tokenize
        ```
        
        Returning a character vector:
        
        ```r
        phrasetotoken
        ```
    
        Returns a `collocations` object, 
        
        ```r
        collocations
        ```
    
        Returns a screen output and data/frame: 
        
        ```r
        kwic
        ```
    
        Counts the number of tokens:
        
        ```r
        ntoken
        ```
    
    2.  Operations on character vectors of *tokens only*, returning a character vector of tokens:
        
        
        ```r
        syllables
        wordstem
        ```

    4.  Operations on character vectors of *tokens*, but also dfm objects and collocations:
        
        
        ```r
        removeFeatures
        ```
    3.  Operations that *previously* worked (currently work) on character vectors of any size, but that will now be folded into Workflow Step 2 functions (see below) as part of tokenize:
    
        
        ```r
        bigrams
        ngrams
        clean
        ```
    
3.  **`dfm` construction and manipulation.**

    
    ```r
    dfm         # also works directly on (the texts of) a corpus
    convert
    docfreq
    docnames
    features
    lexdiv
    ndoc
    ntoken
    plot
    print, show
    removeFeatures
    similarity
    sort
    textmodel, textmodel_*
    topfeatures
    trim
    weight
    settings
    ```

4.  **Auxiliary functions**.

    
    ```r
    dictionary
    stopwords
    textfile
    ```

5.  **Example datasets and objects**.
    
    Example data objects:
    
    ```r
    exampleString       # character, length 1
    ukimmigTexts        # character, length 14
    inaugTexts          # character, length 57
    ie2010Corpus        # corpus
    inaugCorpus         # corpus
    LBGexample          # dfm
    ```
    
    and some built-in objects used by functions:
    
    ```r
    englishSyllables    # named character vector, length 133245
    stopwords           # named list .stopwords, length 16
    ```
    

## Basic workflow steps


1.  Creating the corpus

    Reading files, probably using `textfile()`, then creating a corpus using `corpus()`, making sure the texts have a common encoding, and adding document variables (`docvars`) and metadata (`metadoc` and `metacorpus`).

1.  Defining and delimiting documents

    Defining what are "texts", for instance using `changeunits` or grouping.  
    
    Suggestion: add a `groups=` option to `texts()`, to extract texts from a corpus concatenated by groups of document variables.  (This functionality is currently only available through `dfm`.)

1.  Defining and delimiting textual features

    This step involves defining and extracting the relevant features from each document, using:
    *   `tokenize`, the main function for this step, involves indentifying instances of defined features ("tokens") and extracting them as vectors.  Usually these will consist of words, but may also consist of:
    *   `bigrams` or `ngrams`, adjacent sequences of words, not separated by punctuation marks or sentence boundaries; including
    *   multi-word expressions, through `phrasetotoken`, for selected word ngrams as identified in selected lists rather than simply using all adjacent word pairs or n-sequences.
    
    By defining the broad class of tokens we wish to extract, in this step we also apply rules that will keep or ignore elements such as punctuation or digits, or special aggregations of word and other characters that make up URLS, Twitter tags, or currency-prefixed digits.  This will involve adding the following options to `tokenize`:
    *   `removeDigits`
    *   `removePunct`
    *   `removeAdditional`
    *   `removeTwitter`*
    *   `removeURL`*
    And for convenience and speed, although we more appropriately view this as part of the next workflow step, we will also add an option:
    *   `toLower`
    
    Since the tokenizer we will use may not distinguish the puncutation characters used in constructs such as URLs, email addresses, Twitter handles, or digits prefixed by currency symbols, we will mostly need to use a substitution strategy to replace these with alternative characters prior to tokenization, and then replace the substitutions with the original characters.  This will slow down processing but will only be active by explicit user request for this type of handling to take place.  We could offer three possible options here, such as for URLs, consisting of `c("ignore", "keep", "remove", "ignore")`, to pretend they do not exist and tokenize come what may, to preserve remove URLs in their entirety as "tokens", or to remove them completely, respectively.
    
    Note that that defining and delimiting features may alao include their *parts of speech*, meaning we will need to add functionality for POS tagging and extraction in this step.


1.  Further feature selection

    Once features have been identified and separated from the texts in the tokenization step, features may be removed from token lists, or 
    handled as part of `dfm` construction.  Features may be:
    *   *eliminated* through use of predefined lists or patterns of *stop words*, using `removeFeatures` or `ignoredFeatures` (`dfm` option)
    *   *kept* through through use of predefined lists or patterns of *stop words*, using `removeFeatures` or `keptFeaures` (`dfm` option)
    *   *collapsed* by:
        *   considering morphological variations as equivalent to a stem or lemma, through the `stem` option in `dfm`
        *   considering lists of features as equivalent to a *dictionary* key, either exclusively (`dfm` option `dictionary`) or as a supplement to uncollapsed features (`dfm` option `thesaurus`)
        *   `toLower` to consider as equivalent the same word features despite having different cases, by converting all features to lower case
        
    It will be sometimes possible, to perform these steps separately from the `dfm` creating stage, but in most cases these steps will be performed as options to the `dfm` function.
    
1.  Analysis of the documents and features

    1.  From a corpus.  
    
        These steps don't necessarily require the processing steps above.
        * `kwic`
        * `lexdiv`
        * `summary`
        
    2.  From a dfm -- after `dfm` on the processed document and features.
    
    
## `dfm`, the Swiss Army knife

1.  Most common use case

    In most cases, users will use the default settings to create a dfm straight from a corpus.  `dfm` will combine steps 3--4, even though basic functions will be available to perform these separately.  All options shown in steps 3--4 will be available from `dfm`.

2.  If separate steps are desired

    We will do our best to ensure that all functions allow piping using the `magrittr` package, e.g.

    
    ```r
    mydfm <- texts(mycorpus, group = "party") %>% toLower %>% tokenize %>% wordstem %>%
                                    removeFeatures(stopwords("english")) %>% dfm
    ```

    We recognize however that not all sequences will make sense, for instance `wordstem` will only work *after* tokenization, and will try to catch these errors and make the proper sequence clear to users.
    
        
