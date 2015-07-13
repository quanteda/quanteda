<!-- README.md is generated from README.Rmd. Please edit that file -->
quanteda: Quantitative Analysis of Textual Data
===============================================

An R package for managing and analyzing text, by Ken Benoit and Paul Nulty.

**quanteda** makes it easy to manage texts in the form of a corpus, defined as a collection of texts that includes document-level variables specific to each text, as well as meta-data for documents and for the collection as a whole. **quanteda** includes tools to make it easy and fast to manuipulate the texts in a corpus, by performing the most common natural language processing tasks simply and quickly, such as tokenizing, stemming, or forming ngrams. **quanteda**'s functions for tokenizing texts and forming multiple tokenized documents into a *document-feature matrix* are both extremely fast and extremely simple to use. **quanteda** can segment texts easily by words, paragraphs, sentences, or even user-supplied delimiters and tags.

Built on the text processing functions in the **stringi** package, which is in turn built on C++ implementation of the [ICU](http://www.icu-project.org/) libraries for Unicode text handling, **quanteda** pays special attention to fast and correct implementation of Unicode and the handling of text in any character set, following conversion internally to UTF-8.

**quanteda** is built for efficiency and speed, through its design around three infrastructures: the **string** package for text processing, the **data.table** package for indexing large documents efficiently, and the **Matrix** package for sparse matrix objects. If you can fit it into memory, **quanteda** will handle it quickly. (And eventually, we will make it possible to process objects even larger than available memory.)

**quanteda** is principally designed to allow users a fast and convenient method to go from a corpus of texts to a selected matrix of documents by features, after defining what the documents and features. The package makes it easy to redefine documents, for instance by splitting them into sentences or paragraphs, or by tags, as well as to group them into larger documents by document variables, or to subset them based on logical conditions or combinations of document variables. The package also implements common NLP feature selection functions, such as removing stopwords and stemming in numerous languages, selecting words found in dictionaries, treating words as equivalent based on a user-defined "thesaurus", and trimming and weighting features based on document frequency, feature frequency, and related measures such as *tf-idf*.

Once constructed, a **quanteda** "dfm"" can be easily analyzed using either quanteda's built-in tools for scaling document positions, or used with a number of other text analytic tools, such as:

-   topic models (including converters for direct use with the **topicmodels**, **LDA**, and **stm** packages)

-   document scaling (using **quanteda**'s own functions for the "wordfish" and "Wordscores" models, direct use with the **ca** package for correspondence analysis, or scaling with the **austin** package)

-   machine learning through a variety of other packages that take matrix or matrix-like inputs.

**Additional features** of quanteda include:

-   the ability to explore texts using *key-words-in-context*;

-   fast computation of a variety of readability indexes;

-   fast computation of a variety of lexical diversity measures;

-   quick computation of word or document association measures, for clustering or to compute similarity scores for other purposes; and

-   a comprehensive suite of descriptive statistics on text such as the number of sentences, words, characters, or syllables per document.

**Planned features** coming soon to **quanteda** are:

-   bootstrapping methods for texts that makes it easy to resample texts from pre-defined units, to facilitate computation of confidence intervals on textual statistics using techniques of non-parametric bootstrapping, but applied to the original texts as data.

-   expansion of the document-feature matrix structure through a standard interface called `textmodel()`. (As of version 0.8.0, textmodel works in a basic fashion only for the "Wordscores" and "wordfish" scaling models.)

How to Install
--------------

As of version 0.8.0, the GitHub master repository will always contain the development version of quanteda, while the CRAN version will contain the latest "stable" version. You therefore have two options for installing the package:

1.  From CRAN, using your R package installer, or simply

        install.packages("quanteda")

2.  (For the development version) From GitHub, using

        devtools::install_github("kbenoit/quanteda")

    Because this compiles some C++ source code, you will need a compiler installed. If you are using a Windows platform, this means you will need also to install the [Rtools](http://cran.r-project.org/bin/windows/Rtools/) software available from CRAN. If you are using OS X, you will probably need to install XCode, available for free from the App Store.

3.  (Optional) You can install some additional corpus data from **quantedaData** using

    ``` s
    ## devtools required to install quanteda from Github
    devtools::install_github("kbenoit/quantedaData")
    ```

Documentation
-------------

In-depth tutorials in the form of a gitbook will be available here [here](http://kbenoit.github.io/quanteda).

Examples for any function can also be seen using (for instance, for `corpus()`):

``` s
example(corpus)
```

There are also some demo functions that show off some of the package capabilities, such as `demo(quanteda)`.

Example
-------

``` r
library(quanteda)
# create a corpus from the immigration texts from UK party platforms
uk2010immigCorpus <- corpus(ukimmigTexts,
                            docvars=data.frame(party=names(ukimmigTexts)),
                            notes="Immigration-related sections of 2010 UK party manifestos",
                            enc="UTF-8")
uk2010immigCorpus
#> Corpus consisting of 9 documents.
summary(uk2010immigCorpus, showmeta=TRUE)
#> Corpus consisting of 9 documents.
#> 
#>   Text Types Tokens Sentences        party
#>  text1  1024   2876       137          BNP
#>  text2   135    235        12    Coalition
#>  text3   235    454        21 Conservative
#>  text4   306    614        30       Greens
#>  text5   276    630        34       Labour
#>  text6   246    443        26       LibDem
#>  text7    74    103         5           PC
#>  text8    82    125         4          SNP
#>  text9   310    641        38         UKIP
#> 
#> Source:  /Users/kbenoit/Dropbox/QUANTESS/quanteda_kenlocal_gh/* on x86_64 by kbenoit.
#> Created: Mon Jul 13 18:53:48 2015.
#> Notes:   Immigration-related sections of 2010 UK party manifestos.

# key words in context for "deport", 3 words of context
kwic(uk2010immigCorpus, "deport", 3)
#>                                               preword         word
#>   [text1, 71]                further immigration, the deportation 
#>  [text1, 139]                            The BNP will    deport   
#> [text1, 1628] long-term resettlement programme.\n\n2.    Deport   
#> [text1, 1633]          illegal immigrants\n\nWe shall    deport   
#> [text1, 1653]                current unacceptably lax deportation 
#> [text1, 1659]                           of people are   deported  
#> [text1, 2169]                     enforced by instant deportation,
#> [text1, 2180]         British immigration laws.\n\n8. Deportation 
#> [text1, 2186]           Foreign Criminals\n\nWe shall    deport   
#> [text1, 2198]                       This includes the deportation 
#>  [text4, 566]                      subject to summary deportation.
#>  [text6, 194]         illegal labour.\n\n- Prioritise deportation 
#>  [text6, 394]                  flight risks.\n\n- End deportations
#>  [text9, 317]                            laws or face deportation.
#>                                   postword
#>   [text1, 71] of all illegal              
#>  [text1, 139] all foreigners convicted    
#> [text1, 1628] all illegal immigrants\n\nWe
#> [text1, 1633] all illegal immigrants      
#> [text1, 1653] policies, thousands of      
#> [text1, 1659] from the UK                 
#> [text1, 2169] for anyone found            
#> [text1, 2180] of all Foreign              
#> [text1, 2186] all criminal entrants,      
#> [text1, 2198] of all Muslim               
#>  [text4, 566] They should receive         
#>  [text6, 194] efforts on criminals,       
#>  [text6, 394] of refugees to              
#>  [text9, 317] Such citizens will

# create a dfm, removing stopwords
mydfm <- dfm(uk2010immigCorpus, ignoredFeatures=c("will", stopwords("english")))
#> Creating a dfm from a corpus ...
#>    ... lowercasing
#>    ... tokenizing
#>    ... indexing 9 documents
#>    ... shaping tokens into data.table, found 6,021 total tokens
#>    ... ignoring 175 feature types, discarding 2,599 total features (43.2%)
#>    ... summing tokens by document
#>    ... indexing 1,477 feature types
#>    ... building sparse matrix
#>    ... created a 9 x 1477 sparse dfm
#>    ... complete. Elapsed time: 0.033 seconds.
dim(mydfm)              # basic dimensions of the dfm
#> [1]    9 1477
topfeatures(mydfm, 20)  # 20 top words
#> immigration     british      people      asylum     britain      system 
#>          68          38          36          29          28          27 
#>          uk  population     country         new      ensure  immigrants 
#>          27          21          20          19          17          17 
#>       shall citizenship        work          eu    national      social 
#>          17          16          15          14          14          14 
#>         bnp     illegal 
#>          13          13
if (Sys.info()['sysname']=="Darwin") quartz() # open nicer window, Mac only
plot(mydfm)             # word cloud     
```

![](README-quanteda_example-1.png)
