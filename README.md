<!-- README.md is generated from README.Rmd. Please edit that file -->
**Development version** [![Build Status](https://travis-ci.org/kbenoit/quanteda.svg?branch=dev)]

See the [Getting Started Vignette](http://htmlpreview.github.com/?https://github.com/kbenoit/quanteda/blob/master/vignettes/quickstart.html).

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
#>          Text Types Tokens Sentences        party
#>           BNP  1023   2851       137          BNP
#>     Coalition   133    231        12    Coalition
#>  Conservative   234    452        21 Conservative
#>        Greens   303    610        30       Greens
#>        Labour   278    620        34       Labour
#>        LibDem   240    435        26       LibDem
#>            PC    72    101         5           PC
#>           SNP    81    124         4          SNP
#>          UKIP   311    633        38         UKIP
#> 
#> Source:  /home/paul/Dropbox/quanteda/* on x86_64 by paul.
#> Created: Fri Jul 31 16:51:40 2015.
#> Notes:   Immigration-related sections of 2010 UK party manifestos.

# key words in context for "deport", 3 words of context
kwic(uk2010immigCorpus, "deport", 3)
#>                                               preword         word
#>     [BNP, 71]                further immigration, the deportation 
#>    [BNP, 139]                            The BNP will    deport   
#>   [BNP, 1628] long-term resettlement programme.\n\n2.    Deport   
#>   [BNP, 1633]          illegal immigrants\n\nWe shall    deport   
#>   [BNP, 1653]                current unacceptably lax deportation 
#>   [BNP, 1659]                           of people are   deported  
#>   [BNP, 2169]                     enforced by instant deportation,
#>   [BNP, 2180]         British immigration laws.\n\n8. Deportation 
#>   [BNP, 2186]           Foreign Criminals\n\nWe shall    deport   
#>   [BNP, 2198]                       This includes the deportation 
#> [Greens, 566]                      subject to summary deportation.
#> [LibDem, 194]         illegal labour.\n\n- Prioritise deportation 
#> [LibDem, 394]                  flight risks.\n\n- End deportations
#>   [UKIP, 317]                            laws or face deportation.
#>                                   postword
#>     [BNP, 71] of all illegal              
#>    [BNP, 139] all foreigners convicted    
#>   [BNP, 1628] all illegal immigrants\n\nWe
#>   [BNP, 1633] all illegal immigrants      
#>   [BNP, 1653] policies, thousands of      
#>   [BNP, 1659] from the UK                 
#>   [BNP, 2169] for anyone found            
#>   [BNP, 2180] of all Foreign              
#>   [BNP, 2186] all criminal entrants,      
#>   [BNP, 2198] of all Muslim               
#> [Greens, 566] They should receive         
#> [LibDem, 194] efforts on criminals,       
#> [LibDem, 394] of refugees to              
#>   [UKIP, 317] Such citizens will

# create a dfm, removing stopwords
mydfm <- dfm(uk2010immigCorpus, ignoredFeatures=c("will", stopwords("english")))
#> Creating a dfm from a corpus ...
#>    ... lowercasing
#>    ... tokenizing
#>    ... indexing 9 documents
#>    ... indexing 1,586 feature types
#>    ... removed 97 features, from 175 supplied feature types
#>    ... created a 9 x 1489 sparse dfm
#>    ... complete. 
#> Elapsed time: 0.065 seconds.
dim(mydfm)              # basic dimensions of the dfm
#> [1]    9 1489
topfeatures(mydfm, 20)  # 20 top words
#> immigration     british      people      asylum     britain          uk 
#>          66          37          35          29          28          27 
#>      system  population     country         new  immigrants      ensure 
#>          27          21          20          19          17          17 
#>       shall citizenship      social    national         bnp     illegal 
#>          17          16          14          14          13          13 
#>        work     percent 
#>          13          12
if (Sys.info()['sysname']=="Darwin") quartz() # open nicer window, Mac only
plot(mydfm)             # word cloud     
#> Warning in wordcloud::wordcloud(features(x), colSums(x), ...): immigration
#> could not be fit on page. It will not be plotted.
#> Warning in wordcloud::wordcloud(features(x), colSums(x), ...): seekers
#> could not be fit on page. It will not be plotted.
#> Warning in wordcloud::wordcloud(features(x), colSums(x), ...): employment
#> could not be fit on page. It will not be plotted.
#> Warning in wordcloud::wordcloud(features(x), colSums(x), ...): controls
#> could not be fit on page. It will not be plotted.
#> Warning in wordcloud::wordcloud(features(x), colSums(x), ...): number could
#> not be fit on page. It will not be plotted.
#> Warning in wordcloud::wordcloud(features(x), colSums(x), ...): agency could
#> not be fit on page. It will not be plotted.
#> Warning in wordcloud::wordcloud(features(x), colSums(x), ...): times could
#> not be fit on page. It will not be plotted.
#> Warning in wordcloud::wordcloud(features(x), colSums(x), ...): measures
#> could not be fit on page. It will not be plotted.
#> Warning in wordcloud::wordcloud(features(x), colSums(x), ...): schools
#> could not be fit on page. It will not be plotted.
#> Warning in wordcloud::wordcloud(features(x), colSums(x), ...): according
#> could not be fit on page. It will not be plotted.
#> Warning in wordcloud::wordcloud(features(x), colSums(x), ...): illegally
#> could not be fit on page. It will not be plotted.
#> Warning in wordcloud::wordcloud(features(x), colSums(x), ...): future could
#> not be fit on page. It will not be plotted.
#> Warning in wordcloud::wordcloud(features(x), colSums(x), ...): immigrants
#> could not be fit on page. It will not be plotted.
#> Warning in wordcloud::wordcloud(features(x), colSums(x), ...): statistics
#> could not be fit on page. It will not be plotted.
#> Warning in wordcloud::wordcloud(features(x), colSums(x), ...): review could
#> not be fit on page. It will not be plotted.
#> Warning in wordcloud::wordcloud(features(x), colSums(x), ...): admitted
#> could not be fit on page. It will not be plotted.
#> Warning in wordcloud::wordcloud(features(x), colSums(x), ...): controlled
#> could not be fit on page. It will not be plotted.
#> Warning in wordcloud::wordcloud(features(x), colSums(x), ...): australian-
#> style could not be fit on page. It will not be plotted.
#> Warning in wordcloud::wordcloud(features(x), colSums(x), ...): policies
#> could not be fit on page. It will not be plotted.
#> Warning in wordcloud::wordcloud(features(x), colSums(x), ...): genuine
#> could not be fit on page. It will not be plotted.
#> Warning in wordcloud::wordcloud(features(x), colSums(x), ...): community
#> could not be fit on page. It will not be plotted.
#> Warning in wordcloud::wordcloud(features(x), colSums(x), ...): many could
#> not be fit on page. It will not be plotted.
#> Warning in wordcloud::wordcloud(features(x), colSums(x), ...): levels could
#> not be fit on page. It will not be plotted.
#> Warning in wordcloud::wordcloud(features(x), colSums(x), ...): points-based
#> could not be fit on page. It will not be plotted.
#> Warning in wordcloud::wordcloud(features(x), colSums(x), ...): improve
#> could not be fit on page. It will not be plotted.
#> Warning in wordcloud::wordcloud(features(x), colSums(x), ...): also could
#> not be fit on page. It will not be plotted.
#> Warning in wordcloud::wordcloud(features(x), colSums(x), ...): making could
#> not be fit on page. It will not be plotted.
#> Warning in wordcloud::wordcloud(features(x), colSums(x), ...): refused
#> could not be fit on page. It will not be plotted.
#> Warning in wordcloud::wordcloud(features(x), colSums(x), ...): correct
#> could not be fit on page. It will not be plotted.
#> Warning in wordcloud::wordcloud(features(x), colSums(x), ...): repeal could
#> not be fit on page. It will not be plotted.
#> Warning in wordcloud::wordcloud(features(x), colSums(x), ...): long-term
#> could not be fit on page. It will not be plotted.
#> Warning in wordcloud::wordcloud(features(x), colSums(x), ...): deliberately
#> could not be fit on page. It will not be plotted.
#> Warning in wordcloud::wordcloud(features(x), colSums(x), ...): deliver
#> could not be fit on page. It will not be plotted.
#> Warning in wordcloud::wordcloud(features(x), colSums(x), ...): canada could
#> not be fit on page. It will not be plotted.
#> Warning in wordcloud::wordcloud(features(x), colSums(x), ...): britons
#> could not be fit on page. It will not be plotted.
#> Warning in wordcloud::wordcloud(features(x), colSums(x), ...): public could
#> not be fit on page. It will not be plotted.
#> Warning in wordcloud::wordcloud(features(x), colSums(x), ...): enforce
#> could not be fit on page. It will not be plotted.
#> Warning in wordcloud::wordcloud(features(x), colSums(x), ...): borders
#> could not be fit on page. It will not be plotted.
#> Warning in wordcloud::wordcloud(features(x), colSums(x), ...): today could
#> not be fit on page. It will not be plotted.
#> Warning in wordcloud::wordcloud(features(x), colSums(x), ...): labour could
#> not be fit on page. It will not be plotted.
#> Warning in wordcloud::wordcloud(features(x), colSums(x), ...): priority
#> could not be fit on page. It will not be plotted.
#> Warning in wordcloud::wordcloud(features(x), colSums(x), ...): primary
#> could not be fit on page. It will not be plotted.
#> Warning in wordcloud::wordcloud(features(x), colSums(x), ...): group could
#> not be fit on page. It will not be plotted.
#> Warning in wordcloud::wordcloud(features(x), colSums(x), ...): respect
#> could not be fit on page. It will not be plotted.
#> Warning in wordcloud::wordcloud(features(x), colSums(x), ...): existence
#> could not be fit on page. It will not be plotted.
#> Warning in wordcloud::wordcloud(features(x), colSums(x), ...): continue
#> could not be fit on page. It will not be plotted.
#> Warning in wordcloud::wordcloud(features(x), colSums(x), ...): already
#> could not be fit on page. It will not be plotted.
#> Warning in wordcloud::wordcloud(features(x), colSums(x), ...): political
#> could not be fit on page. It will not be plotted.
```

![](README-quanteda_example-1.png)
