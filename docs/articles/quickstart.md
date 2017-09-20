---
title: "Getting Started with quanteda"
output: 
  rmarkdown::html_vignette:
    css: mystyle.css
    toc: yes
vignette: >
  %\VignetteIndexEntry{Getting Started Guide}
  %\VignetteEngine{knitr::rmarkdown}
  %\VignetteEncoding{UTF-8}
---




This vignette provides a basic overview of **quanteda**'s features and capabilities.  For additional vignettes, see the [articles at quanteda.io](http://quanteda.io/articles/index.html).  

# Introduction

An R package for managing and analyzing text.

**quanteda** makes it easy to manage texts in the form of a
corpus, defined as a collection of texts that includes document-level
variables specific to each text, as well as meta-data for documents
and for the collection as a whole.  **quanteda** includes tools to make it
easy and fast to manuipulate the texts in a corpus, by performing the most common
natural language processing tasks simply and quickly, such as tokenizing,
stemming, or forming ngrams.  **quanteda**'s functions for tokenizing texts
and forming multiple tokenized documents into a *document-feature matrix* are
both extremely fast and extremely simple to use.  **quanteda** can segment texts
easily by words, paragraphs, sentences, or even user-supplied delimiters and tags.

Built on the text processing
functions in the **stringi** package,
which is in turn built on C++ implementation of the [ICU](http://www.icu-project.org/)
libraries for Unicode text handling, **quanteda** pays special attention to fast and correct
implementation of Unicode and the handling of text in any character set, following conversion
internally to UTF-8.

**quanteda** is built for efficiency and speed, through its design around three infrastructures: the **stringi** package for text processing, the **data.table** package for indexing large documents efficiently, and the **Matrix** package for sparse matrix objects.  If you can fit it into memory, **quanteda** will handle it quickly.  (And eventually, we will make it possible to process objects even larger than available memory.)

**quanteda** is principally designed to allow users a fast and convenient method to go from 
a corpus of texts to a selected matrix of documents by features, after defining what 
the documents and features.  The package makes it easy to redefine documents, for instance by splitting them into sentences or paragraphs, or by tags, as well as to group them into larger documents by document variables, or to subset them based on logical conditions or combinations of document variables.  The package also implements common NLP feature selection functions, such as removing stopwords and stemming in numerous languages, selecting words found in dictionaries, treating words as equivalent based on a user-defined "thesaurus", and trimming and weighting features based on document frequency, feature frequency, and related measures such as *tf-idf*.
 
# quanteda Features

## Corpus management tools

The tools for getting texts into a corpus object include: 

* loading texts from directories of individual files
* loading texts ``manually'' by inserting them into a corpus using
  helper functions
*  managing text encodings and conversions from source files into
  corpus texts
* attaching variables to each text that can be used for grouping,
  reorganizing a corpus, or simply recording additional information to
  supplement quantitative analyses with non-textual data
* recording meta-data about the sources and creation details for
  the corpus.
  
The tools for working with a corpus include:

* summarizing the corpus in terms of its language units
* reshaping the corpus into smaller units or more aggregated units
* adding to or extracting subsets of a corpus
* resampling texts of the corpus, for example for use in
  non-parametric bootstrapping of the texts
* Easy extraction and saving, as a new data frame or corpus, key
    words in context (KWIC)

## Natural-Language Processing tools

For extracting features from a corpus, `quanteda` provides the following tools:

* extraction of word types
* extraction of word n-grams
* extraction of dictionary entries from user-defined dictionaries
* feature selection through
    - stemming
    - random selection
    - document frequency
    - word frequency
* and a variety of options for cleaning word types, such as
    capitalization and rules for handling punctuation.

## Document-Feature Matrix analysis tools

For analyzing the resulting *document-feature* matrix created
when features are abstracted from a corpus, `quanteda` provides:

* scaling methods, such as correspondence analysis, Wordfish, and Wordscores
* topic models, such as LDA
* classifiers, such as Naive Bayes or k-nearest neighbour
* sentiment analysis, using dictionaries

## Additional and planned features

**Additional features** of quanteda include:

*  the ability to explore texts using *key-words-in-context*;

*  fast computation of a variety of readability indexes;

*  fast computation of a variety of lexical diversity measures;

*  quick computation of word or document association measures, for clustering or to compute similarity scores for other purposes; and

*  a comprehensive suite of descriptive statistics on text such as the number of sentences, words, characters, or syllables per document.

**Planned features** coming soon to **quanteda** are:

*  bootstrapping methods for texts that makes it easy to resample texts
   from pre-defined units, to facilitate computation of confidence
   intervals on textual statistics using techniques of non-parametric
   bootstrapping, but applied to the original texts as data. 
   
*  expansion of the document-feature matrix structure through a standard interface called `textmodel()`.  (As of version 0.8.0, textmodel works in a basic fashion only for the "Wordscores" and "wordfish" scaling models.)


## Working with other text analysis packages

`quanteda` is hardly unique in providing facilities for working with
text -- the excellent *tm* package already provides many of the
features we have described.  `quanteda` is designed to complement those
packages, as well to simplify the implementation of the
text-to-analysis workflow.  `quanteda` corpus structures are simpler
objects than in *tm*s, as are the document-feature matrix
objects from `quanteda`, compared to the sparse matrix implementation
found in *tm*.  However, there is no need to choose only one
package, since we provide translator functions from one matrix or
corpus object to the other in `quanteda`.

Once constructed, a **quanteda** "dfm"" can be easily passed to other text-analysis packages for 
additional analysis of topic models or scaling, such as:

*  topic models (including converters for direct use with the **topicmodels**, **LDA**, and **stm** packages)

*  document scaling using **quanteda**'s own functions for the "wordfish" and "Wordscores" models, and a sparse method for correspondence analysis

*  document classification methods, using (for example) Naive Bayes, k-nearest neighbour, or Support Vector Machines

*  more sophisticated machine learning through a variety of other packages that take matrix or matrix-like inputs.

*  graphical analysis, including word clouds and strip plots for selected themes or words.


# How to Install

Through a normal installation of the package from CRAN, or for the GitHub version, see the installation instructions at https://github.com/kbenoit/quanteda.

# Creating and Working with a Corpus


```r
require(quanteda)
```


## Currently available corpus sources

**quanteda** has a simple and powerful companion package for loading texts: [**readtext**](https://github.com/kbenoit/readtext).  The main function in this package, `readtext()`,  takes a file or fileset from disk or a URL, and returns a type of data.frame that can be used directly with the `corpus()` constructor function, to create a **quanteda** corpus object.

`readtext()` works on:

* text (`.txt`) files;
* comma-separated-value (`.csv`) files;
* XML formatted data;
* data from the Facebook API, in JSON format;
* data from the Twitter API, in JSON format; and
* generic JSON data.

The corpus constructor command `corpus()` works directly on:

* a vector of character objects, for instance that you have already loaded into the workspace using other tools;
* a `VCorpus` corpus object from the **tm** package.
* a data.frame containing a text column and any other document-level metadata.

### Example: building a corpus from a character vector

The simplest case is to create a corpus from a vector of texts already in memory in R.  This gives the advanced R user complete flexbility with his or her choice of text inputs, as there are almost endless
ways to get a vector of texts into R.

If we already have the texts in this form, we can call the corpus constructor function directly.  We can demonstrate this on the built-in character object of the texts about immigration policy extracted from the 2010 election manifestos of the UK political parties (called `data_char_ukimmig2010`).


```r
myCorpus <- corpus(data_char_ukimmig2010)  # build a new corpus from the texts
summary(myCorpus)
## Corpus consisting of 9 documents.
## 
##          Text Types Tokens Sentences
##           BNP  1126   3330        88
##     Coalition   144    268         4
##  Conservative   252    503        15
##        Greens   325    687        21
##        Labour   296    703        29
##        LibDem   257    499        14
##            PC    80    118         5
##           SNP    90    136         4
##          UKIP   346    739        27
## 
## Source:  C:/Users/Kohei/Documents/R/quanteda/vignettes/* on x86-64 by Kohei
## Created: Thu May 04 01:35:00 2017
## Notes:
```

If we wanted, we could add some document-level variables -- what quanteda calls `docvars` -- to this corpus.

We can do this using the R's `names()` function to get the names of the character vector `data_char_ukimmig2010`, and assign this to a document variable (`docvar`).

```r
docvars(myCorpus, "Party") <- names(data_char_ukimmig2010)
docvars(myCorpus, "Year") <- 2010
summary(myCorpus)
## Corpus consisting of 9 documents.
## 
##          Text Types Tokens Sentences        Party Year
##           BNP  1126   3330        88          BNP 2010
##     Coalition   144    268         4    Coalition 2010
##  Conservative   252    503        15 Conservative 2010
##        Greens   325    687        21       Greens 2010
##        Labour   296    703        29       Labour 2010
##        LibDem   257    499        14       LibDem 2010
##            PC    80    118         5           PC 2010
##           SNP    90    136         4          SNP 2010
##          UKIP   346    739        27         UKIP 2010
## 
## Source:  C:/Users/Kohei/Documents/R/quanteda/vignettes/* on x86-64 by Kohei
## Created: Thu May 04 01:35:00 2017
## Notes:
```

If we wanted to tag each document with additional meta-data not considered a document variable of interest for analysis, but rather something that we need to know as an attribute of the document, we could also 
add those to our corpus.

```r
metadoc(myCorpus, "language") <- "english"
metadoc(myCorpus, "docsource")  <- paste("data_char_ukimmig2010", 1:ndoc(myCorpus), sep = "_")
summary(myCorpus, showmeta = TRUE)
## Corpus consisting of 9 documents.
## 
##          Text Types Tokens Sentences        Party Year _language
##           BNP  1126   3330        88          BNP 2010   english
##     Coalition   144    268         4    Coalition 2010   english
##  Conservative   252    503        15 Conservative 2010   english
##        Greens   325    687        21       Greens 2010   english
##        Labour   296    703        29       Labour 2010   english
##        LibDem   257    499        14       LibDem 2010   english
##            PC    80    118         5           PC 2010   english
##           SNP    90    136         4          SNP 2010   english
##          UKIP   346    739        27         UKIP 2010   english
##               _docsource
##  data_char_ukimmig2010_1
##  data_char_ukimmig2010_2
##  data_char_ukimmig2010_3
##  data_char_ukimmig2010_4
##  data_char_ukimmig2010_5
##  data_char_ukimmig2010_6
##  data_char_ukimmig2010_7
##  data_char_ukimmig2010_8
##  data_char_ukimmig2010_9
## 
## Source:  C:/Users/Kohei/Documents/R/quanteda/vignettes/* on x86-64 by Kohei
## Created: Thu May 04 01:35:00 2017
## Notes:
```

The last command, `metadoc`, allows you to define your own document meta-data fields.  Note that in assiging just the single value of `"english"`, R has recycled the value until it matches the number of documents in the corpus.  In creating
a simple tag for our custom metadoc field `docsource`, we used the quanteda function `ndoc()` to retrieve
the number of documents in our corpus.  This function is deliberately designed to work in a way similar to 
functions you may already use in R, such as `nrow()` and `ncol()`.

### Example: loading in files using the **readtext** package



```r
require(readtext)

# Twitter json
mytf1 <- readtext("~/Dropbox/QUANTESS/social media/zombies/tweets.json")
myCorpusTwitter <- corpus(mytf1)
summary(myCorpusTwitter, 5)
# generic json - needs a textfield specifier
mytf2 <- readtext("~/Dropbox/QUANTESS/Manuscripts/collocations/Corpora/sotu/sotu.json",
                  textfield = "text")
summary(corpus(mytf2), 5)
# text file
mytf3 <- readtext("~/Dropbox/QUANTESS/corpora/project_gutenberg/pg2701.txt", cache = FALSE)
summary(corpus(mytf3), 5)
# multiple text files
mytf4 <- readtext("~/Dropbox/QUANTESS/corpora/inaugural/*.txt", cache = FALSE)
summary(corpus(mytf4), 5)
# multiple text files with docvars from filenames
mytf5 <- readtext("~/Dropbox/QUANTESS/corpora/inaugural/*.txt", 
                  docvarsfrom = "filenames", sep = "-", docvarnames = c("Year", "President"))
summary(corpus(mytf5), 5)
# XML data
mytf6 <- readtext("~/Dropbox/QUANTESS/quanteda_working_files/xmlData/plant_catalog.xml", 
                  textfield = "COMMON")
summary(corpus(mytf6), 5)
# csv file
write.csv(data.frame(inaugSpeech = texts(data_corpus_inaugural), 
                     docvars(data_corpus_inaugural)),
          file = "/tmp/inaug_texts.csv", row.names = FALSE)b
mytf7 <- readtext("/tmp/inaug_texts.csv", textfield = "inaugSpeech")
summary(corpus(mytf7), 5)
```


## How a quanteda corpus works

### Corpus principles

A corpus is designed to be a "library" of original documents that have been converted to plain, UTF-8 encoded text, and stored along with meta-data at the corpus level and at the document-level.  We have a special name for document-level meta-data: *docvars*.  These are variables or features that describe attributes of each document.

A corpus is designed to be a more or less static container of texts with respect to processing and analysis.  This means that the texts in corpus are not designed to be changed internally through (for example) cleaning or pre-processing steps, such as stemming or removing punctuation.  Rather, texts can be extracted from the corpus as part of processing, and assigned to new objects, but the idea is that the corpus will remain as an original reference copy so that other analyses -- for instance those in which stems and punctuation were required, such as analyzing a reading ease index -- can be performed on the same corpus.

To extract texts from a a corpus, we use an extractor, called `texts()`.  


```r
texts(data_corpus_inaugural)[2]
##                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              1793-Washington 
## "Fellow citizens, I am again called upon by the voice of my country to execute the functions of its Chief Magistrate. When the occasion proper for it shall arrive, I shall endeavor to express the high sense I entertain of this distinguished honor, and of the confidence which has been reposed in me by the people of united America.\n\nPrevious to the execution of any official act of the President the Constitution requires an oath of office. This oath I am now about to take, and in your presence: That if it shall be found during my administration of the Government I have in any instance violated willingly or knowingly the injunctions thereof, I may (besides incurring constitutional punishment) be subject to the upbraidings of all who are now witnesses of the present solemn ceremony.\n\n "
```

To summarize the texts from a corpus, we can call a `summary()` method defined for a corpus.


```r
summary(data_corpus_irishbudget2010)
## Corpus consisting of 14 documents.
## 
##                                   Text Types Tokens Sentences year debate
##        2010_BUDGET_01_Brian_Lenihan_FF  1949   8733       374 2010 BUDGET
##       2010_BUDGET_02_Richard_Bruton_FG  1042   4478       217 2010 BUDGET
##         2010_BUDGET_03_Joan_Burton_LAB  1621   6429       307 2010 BUDGET
##        2010_BUDGET_04_Arthur_Morgan_SF  1589   7185       343 2010 BUDGET
##          2010_BUDGET_05_Brian_Cowen_FF  1618   6697       250 2010 BUDGET
##           2010_BUDGET_06_Enda_Kenny_FG  1151   4254       153 2010 BUDGET
##      2010_BUDGET_07_Kieran_ODonnell_FG   681   2309       133 2010 BUDGET
##       2010_BUDGET_08_Eamon_Gilmore_LAB  1183   4217       201 2010 BUDGET
##     2010_BUDGET_09_Michael_Higgins_LAB   490   1288        44 2010 BUDGET
##        2010_BUDGET_10_Ruairi_Quinn_LAB   442   1290        59 2010 BUDGET
##      2010_BUDGET_11_John_Gormley_Green   404   1036        49 2010 BUDGET
##        2010_BUDGET_12_Eamon_Ryan_Green   512   1651        90 2010 BUDGET
##      2010_BUDGET_13_Ciaran_Cuffe_Green   444   1248        45 2010 BUDGET
##  2010_BUDGET_14_Caoimhghin_OCaolain_SF  1188   4094       176 2010 BUDGET
##  number      foren     name party
##      01      Brian  Lenihan    FF
##      02    Richard   Bruton    FG
##      03       Joan   Burton   LAB
##      04     Arthur   Morgan    SF
##      05      Brian    Cowen    FF
##      06       Enda    Kenny    FG
##      07     Kieran ODonnell    FG
##      08      Eamon  Gilmore   LAB
##      09    Michael  Higgins   LAB
##      10     Ruairi    Quinn   LAB
##      11       John  Gormley Green
##      12      Eamon     Ryan Green
##      13     Ciaran    Cuffe Green
##      14 Caoimhghin OCaolain    SF
## 
## Source:  /home/paul/Dropbox/code/quantedaData/* on x86_64 by paul
## Created: Tue Sep 16 15:58:21 2014
## Notes:
```

We can save the output from the summary command as a data frame, and plot some basic descriptive statistics with this information:


```r
tokenInfo <- summary(data_corpus_inaugural)
## Corpus consisting of 58 documents.
## 
##             Text Types Tokens Sentences Year  President       FirstName
##  1789-Washington   626   1540        23 1789 Washington          George
##  1793-Washington    96    147         4 1793 Washington          George
##       1797-Adams   826   2584        37 1797      Adams            John
##   1801-Jefferson   716   1935        41 1801  Jefferson          Thomas
##   1805-Jefferson   804   2381        45 1805  Jefferson          Thomas
##     1809-Madison   536   1267        21 1809    Madison           James
##     1813-Madison   542   1304        33 1813    Madison           James
##      1817-Monroe  1040   3696       121 1817     Monroe           James
##      1821-Monroe  1262   4898       129 1821     Monroe           James
##       1825-Adams  1004   3154        74 1825      Adams     John Quincy
##     1829-Jackson   517   1210        25 1829    Jackson          Andrew
##     1833-Jackson   499   1271        29 1833    Jackson          Andrew
##    1837-VanBuren  1315   4175        95 1837  Van Buren          Martin
##    1841-Harrison  1893   9178       210 1841   Harrison   William Henry
##        1845-Polk  1330   5211       153 1845       Polk      James Knox
##      1849-Taylor   497   1185        22 1849     Taylor         Zachary
##      1853-Pierce  1166   3657       104 1853     Pierce        Franklin
##    1857-Buchanan   945   3106        89 1857   Buchanan           James
##     1861-Lincoln  1075   4016       135 1861    Lincoln         Abraham
##     1865-Lincoln   362    780        26 1865    Lincoln         Abraham
##       1869-Grant   486   1243        40 1869      Grant      Ulysses S.
##       1873-Grant   552   1479        43 1873      Grant      Ulysses S.
##       1877-Hayes   829   2730        59 1877      Hayes   Rutherford B.
##    1881-Garfield  1018   3240       111 1881   Garfield        James A.
##   1885-Cleveland   674   1828        44 1885  Cleveland          Grover
##    1889-Harrison  1355   4744       157 1889   Harrison        Benjamin
##   1893-Cleveland   823   2135        58 1893  Cleveland          Grover
##    1897-McKinley  1236   4383       130 1897   McKinley         William
##    1901-McKinley   857   2449       100 1901   McKinley         William
##   1905-Roosevelt   404   1089        33 1905  Roosevelt        Theodore
##        1909-Taft  1436   5844       159 1909       Taft  William Howard
##      1913-Wilson   661   1896        68 1913     Wilson         Woodrow
##      1917-Wilson   549   1656        59 1917     Wilson         Woodrow
##     1921-Harding  1172   3743       148 1921    Harding       Warren G.
##    1925-Coolidge  1221   4442       196 1925   Coolidge          Calvin
##      1929-Hoover  1086   3895       158 1929     Hoover         Herbert
##   1933-Roosevelt   744   2064        85 1933  Roosevelt     Franklin D.
##   1937-Roosevelt   729   2027        96 1937  Roosevelt     Franklin D.
##   1941-Roosevelt   527   1552        68 1941  Roosevelt     Franklin D.
##   1945-Roosevelt   276    651        26 1945  Roosevelt     Franklin D.
##      1949-Truman   781   2531       116 1949     Truman        Harry S.
##  1953-Eisenhower   903   2765       119 1953 Eisenhower       Dwight D.
##  1957-Eisenhower   621   1933        92 1957 Eisenhower       Dwight D.
##     1961-Kennedy   566   1568        52 1961    Kennedy         John F.
##     1965-Johnson   569   1725        93 1965    Johnson   Lyndon Baines
##       1969-Nixon   743   2437       103 1969      Nixon Richard Milhous
##       1973-Nixon   545   2018        68 1973      Nixon Richard Milhous
##      1977-Carter   528   1380        52 1977     Carter           Jimmy
##      1981-Reagan   904   2798       128 1981     Reagan          Ronald
##      1985-Reagan   925   2935       123 1985     Reagan          Ronald
##        1989-Bush   795   2683       141 1989       Bush          George
##     1993-Clinton   644   1837        81 1993    Clinton            Bill
##     1997-Clinton   773   2451       111 1997    Clinton            Bill
##        2001-Bush   622   1810        97 2001       Bush       George W.
##        2005-Bush   772   2325       100 2005       Bush       George W.
##       2009-Obama   939   2729       110 2009      Obama          Barack
##       2013-Obama   814   2335        88 2013      Obama          Barack
##       2017-Trump   582   1662        88 2017      Trump       Donald J.
## 
## Source:  /home/paul/Dropbox/code/quanteda/* on x86_64 by paul
## Created: Fri Sep 12 12:41:17 2014
## Notes:
if (require(ggplot2))
    ggplot(data=tokenInfo, aes(x=Year, y=Tokens, group=1)) + geom_line() + geom_point() +
        scale_x_discrete(labels=c(seq(1789,2012,12)), breaks=seq(1789,2012,12) ) 
## Loading required package: ggplot2
```

![plot of chunk unnamed-chunk-9](figure/unnamed-chunk-9-1.png)

```r

# Longest inaugural address: William Henry Harrison
tokenInfo[which.max(tokenInfo$Tokens),] 
##                        Text Types Tokens Sentences Year President
## 1841-Harrison 1841-Harrison  1893   9178       210 1841  Harrison
##                   FirstName
## 1841-Harrison William Henry
```


## Tools for handling corpus objects

### Adding two corpus objects together

The `+` operator provides a simple method for concatenating two corpus objects.  If they contain
different sets of document-level variables, these will be stitched together in a fashion that guarantees
that no information is lost.  Corpus-level medata data is also concatenated.


```r
library(quanteda)
mycorpus1 <- corpus(data_corpus_inaugural[1:5], note = "First five inaug speeches.")
## Warning in corpus.character(data_corpus_inaugural[1:5], note = "First five
## inaug speeches."): Argument note not used.
mycorpus2 <- corpus(data_corpus_inaugural[53:58], note = "Last five inaug speeches.")
## Warning in corpus.character(data_corpus_inaugural[53:58], note = "Last five
## inaug speeches."): Argument note not used.
mycorpus3 <- mycorpus1 + mycorpus2
summary(mycorpus3)
## Corpus consisting of 11 documents.
## 
##             Text Types Tokens Sentences
##  1789-Washington   626   1540        23
##  1793-Washington    96    147         4
##       1797-Adams   826   2584        37
##   1801-Jefferson   716   1935        41
##   1805-Jefferson   804   2381        45
##     1997-Clinton   773   2451       111
##        2001-Bush   622   1810        97
##        2005-Bush   772   2325       100
##       2009-Obama   939   2729       110
##       2013-Obama   814   2335        88
##       2017-Trump   582   1662        88
## 
## Source:  Combination of corpuses mycorpus1 and mycorpus2
## Created: Thu May 04 01:35:01 2017
## Notes:
```

### subsetting corpus objects

There is a method of the `corpus_subset()` function defined for corpus objects, where a new corpus can 
be extracted based on logical conditions applied to docvars:


```r
summary(corpus_subset(data_corpus_inaugural, Year > 1990))
## Corpus consisting of 7 documents.
## 
##          Text Types Tokens Sentences Year President FirstName
##  1993-Clinton   644   1837        81 1993   Clinton      Bill
##  1997-Clinton   773   2451       111 1997   Clinton      Bill
##     2001-Bush   622   1810        97 2001      Bush George W.
##     2005-Bush   772   2325       100 2005      Bush George W.
##    2009-Obama   939   2729       110 2009     Obama    Barack
##    2013-Obama   814   2335        88 2013     Obama    Barack
##    2017-Trump   582   1662        88 2017     Trump Donald J.
## 
## Source:  /home/paul/Dropbox/code/quanteda/* on x86_64 by paul
## Created: Fri Sep 12 12:41:17 2014
## Notes:
summary(corpus_subset(data_corpus_inaugural, President == "Adams"))
## Corpus consisting of 2 documents.
## 
##        Text Types Tokens Sentences Year President   FirstName
##  1797-Adams   826   2584        37 1797     Adams        John
##  1825-Adams  1004   3154        74 1825     Adams John Quincy
## 
## Source:  /home/paul/Dropbox/code/quanteda/* on x86_64 by paul
## Created: Fri Sep 12 12:41:17 2014
## Notes:
```


## Exploring corpus texts

The `kwic` function (KeyWord In Context) performs a search for a word and allows us to view the contexts in which it occurs:


```r
options(width = 200)
kwic(data_corpus_inaugural, "terror")
##                                                                                                       
##     [1797-Adams, 1327]              fraud or violence, by | terror | , intrigue, or venality          
##  [1933-Roosevelt, 112] nameless, unreasoning, unjustified | terror | which paralyzes needed efforts to
##  [1941-Roosevelt, 289]      seemed frozen by a fatalistic | terror | , we proved that this            
##    [1961-Kennedy, 868]    alter that uncertain balance of | terror | that stays the hand of           
##     [1981-Reagan, 821]     freeing all Americans from the | terror | of runaway living costs.         
##   [1997-Clinton, 1055]        They fuel the fanaticism of | terror | . And they torment the           
##   [1997-Clinton, 1655]  maintain a strong defense against | terror | and destruction. Our children    
##     [2009-Obama, 1646]     advance their aims by inducing | terror | and slaughtering innocents, we
kwic(data_corpus_inaugural, "terror", valuetype = "regex")
##                                                                                                               
##     [1797-Adams, 1327]                   fraud or violence, by |  terror   | , intrigue, or venality          
##  [1933-Roosevelt, 112]      nameless, unreasoning, unjustified |  terror   | which paralyzes needed efforts to
##  [1941-Roosevelt, 289]           seemed frozen by a fatalistic |  terror   | , we proved that this            
##    [1961-Kennedy, 868]         alter that uncertain balance of |  terror   | that stays the hand of           
##    [1961-Kennedy, 992]               of science instead of its |  terrors  | . Together let us explore        
##     [1981-Reagan, 821]          freeing all Americans from the |  terror   | of runaway living costs.         
##    [1981-Reagan, 2204]        understood by those who practice | terrorism | and prey upon their neighbors    
##   [1997-Clinton, 1055]             They fuel the fanaticism of |  terror   | . And they torment the           
##   [1997-Clinton, 1655]       maintain a strong defense against |  terror   | and destruction. Our children    
##     [2009-Obama, 1646]          advance their aims by inducing |  terror   | and slaughtering innocents, we   
##     [2017-Trump, 1119] civilized world against radical Islamic | terrorism | , which we will eradicate
kwic(data_corpus_inaugural, "communist*")
##                                                                                              
##   [1949-Truman, 838] the actions resulting from the | Communist  | philosophy are a threat to
##  [1961-Kennedy, 519]             -- not because the | Communists | may be doing it,
```


In the above summary, `Year` and `President` are variables associated with each document. We can access such variables with the `docvars()` function.


```r
# inspect the document-level variables
head(docvars(data_corpus_inaugural))
##                 Year  President FirstName
## 1789-Washington 1789 Washington    George
## 1793-Washington 1793 Washington    George
## 1797-Adams      1797      Adams      John
## 1801-Jefferson  1801  Jefferson    Thomas
## 1805-Jefferson  1805  Jefferson    Thomas
## 1809-Madison    1809    Madison     James

# inspect the corpus-level metadata
metacorpus(data_corpus_inaugural)
## $source
## [1] "/home/paul/Dropbox/code/quanteda/* on x86_64 by paul"
## 
## $created
## [1] "Fri Sep 12 12:41:17 2014"
## 
## $notes
## NULL
## 
## $citation
## NULL
```

More corpora are available from the [quantedaData](http://github.com/kbenoit/quantedaData) package.




# Extracting Features from a Corpus

In order to perform statistical analysis such as document scaling, we
must extract a matrix associating values for certain features with each
document. In quanteda, we use the `dfm` function to produce such a matrix.  "dfm" is short for *document-feature matrix*, and always refers to documents
in rows and "features" as columns.  We fix this dimensional orientation because is is 
standard in data analysis to have a unit of analysis as a row, and features or variables
pertaining to each unit as columns.  We call them "features" rather than terms, because
features are more general than terms: they can be defined as raw terms, stemmed terms, the parts of speech of terms, terms after stopwords have been removed,
or a dictionary class to which a term belongs.  Features can be entirely general, such as ngrams or syntactic dependencies, and we leave this open-ended.

## Tokenizing texts

To simply tokenize a text, quanteda provides a powerful command called `tokens()`.  This produces an 
intermediate object, consisting of a list of tokens in the form of character vectors, where each element
of the list corresponds to an input document.

`tokens()` is deliberately conservative, meaning that it does not remove anything from the text unless
told to do so.


```r
txt <- c(text1 = "This is $10 in 999 different ways,\n up and down; left and right!", 
         text2 = "@kenbenoit working: on #quanteda 2day\t4ever, http://textasdata.com?page=123.")
tokens(txt)
## tokens from 2 documents.
## text1 :
##  [1] "This"      "is"        "$"         "10"        "in"        "999"       "different" "ways"      ","         "up"        "and"       "down"      ";"         "left"      "and"       "right"    
## [17] "!"        
## 
## text2 :
##  [1] "@kenbenoit"     "working"        ":"              "on"             "#quanteda"      "2day"           "4ever"          ","              "http"           ":"              "/"             
## [12] "/"              "textasdata.com" "?"              "page"           "="              "123"            "."
tokens(txt, remove_numbers = TRUE, remove_punct = TRUE)
## tokens from 2 documents.
## text1 :
##  [1] "This"      "is"        "in"        "different" "ways"      "up"        "and"       "down"      "left"      "and"       "right"    
## 
## text2 :
## [1] "@kenbenoit"     "working"        "on"             "#quanteda"      "2day"           "4ever"          "http"           "textasdata.com" "page"
tokens(txt, remove_numbers = FALSE, remove_punct = TRUE)
## tokens from 2 documents.
## text1 :
##  [1] "This"      "is"        "10"        "in"        "999"       "different" "ways"      "up"        "and"       "down"      "left"      "and"       "right"    
## 
## text2 :
##  [1] "@kenbenoit"     "working"        "on"             "#quanteda"      "2day"           "4ever"          "http"           "textasdata.com" "page"           "123"
tokens(txt, remove_numbers = TRUE, remove_punct = FALSE)
## tokens from 2 documents.
## text1 :
##  [1] "This"      "is"        "$"         "in"        "different" "ways"      ","         "up"        "and"       "down"      ";"         "left"      "and"       "right"     "!"        
## 
## text2 :
##  [1] "@kenbenoit"     "working"        ":"              "on"             "#quanteda"      "2day"           "4ever"          ","              "http"           ":"              "/"             
## [12] "/"              "textasdata.com" "?"              "page"           "="              "."
tokens(txt, remove_numbers = FALSE, remove_punct = FALSE)
## tokens from 2 documents.
## text1 :
##  [1] "This"      "is"        "$"         "10"        "in"        "999"       "different" "ways"      ","         "up"        "and"       "down"      ";"         "left"      "and"       "right"    
## [17] "!"        
## 
## text2 :
##  [1] "@kenbenoit"     "working"        ":"              "on"             "#quanteda"      "2day"           "4ever"          ","              "http"           ":"              "/"             
## [12] "/"              "textasdata.com" "?"              "page"           "="              "123"            "."
tokens(txt, remove_numbers = FALSE, remove_punct = FALSE, remove_separators = FALSE)
## tokens from 2 documents.
## text1 :
##  [1] "This"      " "         "is"        " "         "$"         "10"        " "         "in"        " "         "999"       " "         "different" " "         "ways"      ","         "\n"       
## [17] " "         "up"        " "         "and"       " "         "down"      ";"         " "         "left"      " "         "and"       " "         "right"     "!"        
## 
## text2 :
##  [1] "@kenbenoit"     " "              "working"        ":"              " "              "on"             " "              "#quanteda"      " "              "2day"           "\t"             
## [12] "4ever"          ","              " "              "http"           ":"              "/"              "/"              "textasdata.com" "?"              "page"           "="             
## [23] "123"            "."
```

We also have the option to tokenize characters:

```r
tokens("Great website: http://textasdata.com?page=123.", what = "character")
## tokens from 1 document.
## Component 1 :
##  [1] "G" "r" "e" "a" "t" "w" "e" "b" "s" "i" "t" "e" ":" "h" "t" "t" "p" ":" "/" "/" "t" "e" "x" "t" "a" "s" "d" "a" "t" "a" "." "c" "o" "m" "?" "p" "a" "g" "e" "=" "1" "2" "3" "."
tokens("Great website: http://textasdata.com?page=123.", what = "character", 
         remove_separators = FALSE)
## tokens from 1 document.
## Component 1 :
##  [1] "G" "r" "e" "a" "t" " " "w" "e" "b" "s" "i" "t" "e" ":" " " "h" "t" "t" "p" ":" "/" "/" "t" "e" "x" "t" "a" "s" "d" "a" "t" "a" "." "c" "o" "m" "?" "p" "a" "g" "e" "=" "1" "2" "3" "."
```

and sentences:

```r
# sentence level         
tokens(c("Kurt Vongeut said; only assholes use semi-colons.", 
           "Today is Thursday in Canberra:  It is yesterday in London.", 
           "En el caso de que no puedas ir con ellos, ¿quieres ir con nosotros?"), 
          what = "sentence")
## tokens from 3 documents.
## Component 1 :
## [1] "Kurt Vongeut said; only assholes use semi-colons."
## 
## Component 2 :
## [1] "Today is Thursday in Canberra:  It is yesterday in London."
## 
## Component 3 :
## [1] "En el caso de que no puedas ir con ellos, ¿quieres ir con nosotros?"
```

## Constructing a document-frequency matrix

Tokenizing texts is an intermediate option, and most users will want to skip straight to constructing
a document-feature matrix.  For this, we have a Swiss-army knife function, called `dfm()`, which performs
tokenization and tabulates the extracted features into a matrix of documents by features.  Unlike
the conservative approach taken by `tokens()`, the `dfm()` function applies certain options by default,
such as `toLower()` -- a separate function for lower-casing texts -- and removes punctuation.  All of the options to `tokens()` can be passed to `dfm()`, however.


```r
myCorpus <- corpus_subset(data_corpus_inaugural, Year > 1990)

# make a dfm
myDfm <- dfm(myCorpus)
myDfm[, 1:5]
## Document-feature matrix of: 7 documents, 5 features (0% sparse).
## 7 x 5 sparse Matrix of class "dfmSparse"
##               features
## docs           my fellow citizens   , today
##   1993-Clinton  7      5        2 139    10
##   1997-Clinton  6      7        7 131     5
##   2001-Bush     3      1        9 110     2
##   2005-Bush     2      3        6 120     3
##   2009-Obama    2      1        1 130     6
##   2013-Obama    3      3        6  99     4
##   2017-Trump    1      1        4  96     4
```

Other options for a `dfm()` include removing stopwords, and stemming the tokens.

```r
# make a dfm, removing stopwords and applying stemming
myStemMat <- dfm(myCorpus, remove = stopwords("english"), stem = TRUE, remove_punct = TRUE)
myStemMat[, 1:5]
## Document-feature matrix of: 7 documents, 5 features (17.1% sparse).
## 7 x 5 sparse Matrix of class "dfmSparse"
##               features
## docs           fellow citizen today celebr mysteri
##   1993-Clinton      5       2    10      4       1
##   1997-Clinton      7       8     6      1       0
##   2001-Bush         1      10     2      0       0
##   2005-Bush         3       7     3      2       0
##   2009-Obama        1       1     6      2       0
##   2013-Obama        3       8     6      1       0
##   2017-Trump        1       4     5      3       1
```

The option `remove` provides a list of tokens to be ignored.  Most users will 
supply a list of pre-defined "stop words", defined for numerous languages, accessed through 
the `stopwords()` function:

```r
head(stopwords("english"), 20)
##  [1] "i"          "me"         "my"         "myself"     "we"         "our"        "ours"       "ourselves"  "you"        "your"       "yours"      "yourself"   "yourselves" "he"         "him"       
## [16] "his"        "himself"    "she"        "her"        "hers"
head(stopwords("russian"), 10)
##  [1] "<U+0438>" "<U+0432>" "<U+0432><U+043E>" "<U+043D><U+0435>" "<U+0447><U+0442><U+043E>" "<U+043E><U+043D>" "<U+043D><U+0430>" "<U+044F>" "<U+0441>" "<U+0441><U+043E>"
head(stopwords("arabic"), 10)
##  [1] "<U+0641><U+0649>" "<U+0641><U+064A>" "<U+0643><U+0644>" "<U+0644><U+0645>" "<U+0644><U+0646>" "<U+0644><U+0647>" "<U+0645><U+0646>" "<U+0647><U+0648>" "<U+0647><U+064A>" "<U+0642><U+0648><U+0629>"
```



### Viewing the document-frequency matrix

The dfm can be inspected in the Enviroment pane in RStudio, or by calling R's `View` function. Calling `plot` on a dfm will display a wordcloud using the [wordcloud package](link.)

```r
mydfm <- dfm(data_char_ukimmig2010, remove = c("will", stopwords("english")), 
             remove_punct = TRUE)
mydfm
## Document-feature matrix of: 9 documents, 1,547 features (83.8% sparse).
```

To access a list of the most frequently occurring features, we can use `topfeatures()`:

```r
topfeatures(mydfm, 20)  # 20 top words
## immigration     british      people      asylum     britain          uk      system  population     country         new  immigrants      ensure       shall citizenship      social    national 
##          66          37          35          29          28          27          27          21          20          19          17          17          17          16          14          14 
##         bnp     illegal        work     percent 
##          13          13          13          12
```

Plotting a word cloud is done using `textplot_wordcloud()`, for a `dfm` class object.  This function passes arguments through to `wordcloud()` from the **wordcloud** package, and can prettify the plot using the same arguments:

```r
set.seed(100)
textplot_wordcloud(mydfm, min.freq = 6, random.order = FALSE,
                   rot.per = .25, 
                   colors = RColorBrewer::brewer.pal(8,"Dark2"))
```

![plot of chunk unnamed-chunk-22](figure/unnamed-chunk-22-1.png)

### Grouping documents by document variable 

Often, we are interested in analysing how texts differ according to substantive factors which may be encoded in the document variables, rather than simply by the boundaries of the document files. We can group documents which share the same value for a document variable when creating a dfm:


```r
byPartyDfm <- dfm(data_corpus_irishbudget2010, groups = "party", remove = stopwords("english"), remove_punct = TRUE)
```

We can sort this dfm, and inspect it:

```r
sort(byPartyDfm)[, 1:10]
## Warning: 'sort.dfm' is deprecated.
## Use 'dfm_sort' instead.
## See help("Deprecated")
## Document-feature matrix of: 5 documents, 10 features (0% sparse).
## 5 x 10 sparse Matrix of class "dfmSparse"
##        features
## docs    will people budget government public minister tax economy pay jobs
##   FF     212     23     44         47     65       11  60      37  41   41
##   FG      93     78     71         61     47       62  11      20  29   17
##   Green   59     15     26         19      4        4  11      16   4   15
##   LAB     89     69     66         36     32       54  47      37  24   20
##   SF     104     81     53         73     31       39  34      50  24   27
```

Note that the most frequently occurring feature is "will", a word usually on English stop lists, but
one that is not included in quanteda's built-in English stopword list.  


### Grouping words by dictionary or equivalence class

For some applications we have prior knowledge of sets of words that are indicative of traits we would like to measure from the text. For example, a general list of positive words might indicate positive sentiment in a movie review, or we might have a dictionary of political terms which are associated with a particular ideological stance. In these cases, it is sometimes useful to treat these groups of words as equivalent for the purposes of analysis, and sum their counts into classes. 

For example, let's look at how words associated with terrorism and words associated with the economy vary by President in the inaugural speeches corpus. From the original corpus, we select Presidents since Clinton:


```r
recentCorpus <- corpus_subset(data_corpus_inaugural, Year > 1991)
```

Now we define a demonstration dictionary:

```r
myDict <- dictionary(list(terror = c("terrorism", "terrorists", "threat"),
                          economy = c("jobs", "business", "grow", "work")))
```


We can use the dictionary when making the dfm:

```r
byPresMat <- dfm(recentCorpus, dictionary = myDict)
byPresMat
## Document-feature matrix of: 7 documents, 2 features (14.3% sparse).
## 7 x 2 sparse Matrix of class "dfmSparse"
##               features
## docs           terror economy
##   1993-Clinton      0       8
##   1997-Clinton      1       8
##   2001-Bush         0       4
##   2005-Bush         1       6
##   2009-Obama        1      10
##   2013-Obama        1       6
##   2017-Trump        1       5
```

The constructor function `dictionary()` also works with two common "foreign" dictionary formats: the 
LIWC and Provalis Research's Wordstat format.  For instance, we can load the LIWC and apply this to the Presidential inaugural speech corpus:

```r
liwcdict <- dictionary(file = "~/Dropbox/QUANTESS/dictionaries/LIWC/LIWC2001_English.dic",
                       format = "LIWC")
liwcdfm <- dfm(data_corpus_inaugural[52:58], dictionary = liwcdict, verbose = FALSE)
liwcdfm[, 1:10]
```



# Further Examples

## Similarities between texts


```r
presDfm <- dfm(corpus_subset(data_corpus_inaugural, Year>1980), 
               remove = stopwords("english"),
               stem = TRUE, remove_punct = TRUE)
obamaSimil <- textstat_simil(presDfm, c("2009-Obama" , "2013-Obama"), 
                             margin = "documents", method = "cosine")
obamaSimil
##              2009-Obama 2013-Obama
## 2009-Obama    1.0000000  0.7144178
## 2013-Obama    0.7144178  1.0000000
## 1981-Reagan   0.6726373  0.6822342
## 1985-Reagan   0.6669281  0.6844686
## 1989-Bush     0.6687192  0.6257466
## 1993-Clinton  0.6288108  0.6278986
## 1997-Clinton  0.6954614  0.6826705
## 2001-Bush     0.6529447  0.6656213
## 2005-Bush     0.5766800  0.6292757
## 2017-Trump    0.5867729  0.5796322
# dotchart(as.list(obamaSimil)$"2009-Obama", xlab = "Cosine similarity")
```


We can use these distances to plot a dendrogram, clustering presidents:

```r
data(data_corpus_SOTU, package="quantedaData")
presDfm <- dfm(corpus_subset(data_corpus_SOTU, Date > as.Date("1980-01-01")), 
               verbose = FALSE, stem = TRUE, remove_punct = TRUE,
               remove = c("will", stopwords("english")))
presDfm <- dfm_trim(presDfm, min_count = 5, min_docfreq = 3)
# hierarchical clustering - get distances on normalized dfm
presDistMat <- textstat_dist(dfm_weight(presDfm, "relFreq"))
# hiarchical clustering the distance object
presCluster <- hclust(presDistMat)
# label with document names
presCluster$labels <- docnames(presDfm)
# plot as a dendrogram
plot(presCluster, xlab = "", sub = "", main = "Euclidean Distance on Normalized Token Frequency")
```
(try it!)

We can also look at term similarities:

```r
sim <- textstat_simil(presDfm, c("fair", "health", "terror"), method = "cosine", margin = "features")
as.list(sim)
## $fair
##        economi          begin      jefferson         author          faith           call        struggl           best          creat         courag            god          pledg        compass 
##     0.90802522     0.90759507     0.89814624     0.89442719     0.88665863     0.86082846     0.84515425     0.83666003     0.83473001     0.83266640     0.83217147     0.82928843     0.82807867 
##           much         social           alli         believ          order         danger        continu         failur           full          limit           well            tax         govern 
##     0.82353211     0.82158384     0.82158384     0.81946518     0.81649658     0.81649658     0.81508761     0.80829038     0.80829038     0.80829038     0.80647784     0.80075722     0.79758455 
##             us           side       opportun         beyond         travel          stand           vice         suffer          howev           size        chariti           hold         prayer 
##     0.79584025     0.78772636     0.78072006     0.77849894     0.77849894     0.77517023     0.77459667     0.77459667     0.77459667     0.77459667     0.77459667     0.77459667     0.77067464 
##           peac         econom        preserv           meet          bless          among         weapon           take          earth            yet          thoma         almost         republ 
##     0.76650504     0.76405745     0.76405745     0.76405745     0.76035655     0.75373466     0.75324358     0.75277265     0.74600385     0.74586985     0.74535599     0.74535599     0.74535599 
##           cost           sign         troubl         declin           rest         intend           agre           upon           must            now           mani            way           time 
##     0.74535599     0.74535599     0.74535599     0.74535599     0.74535599     0.74535599     0.74535599     0.74479325     0.74318544     0.74043610     0.74043610     0.73952191     0.73802030 
##          think          assur           fall            aim        deficit       threaten         growth           ever         system          carri        digniti         o'neil          occas 
##     0.73786479     0.73786479     0.73786479     0.73786479     0.73484692     0.73333333     0.73333333     0.73246702     0.73192505     0.73156352     0.73131034     0.73029674     0.73029674 
##         inflat       unemploy           pace           bear        concern         ethnic        barrier           core         revers         genius         church       prioriti         unborn 
##     0.73029674     0.73029674     0.73029674     0.73029674     0.73029674     0.73029674     0.73029674     0.73029674     0.73029674     0.73029674     0.73029674     0.73029674     0.73029674 
##        arsenal         utmost           john     accomplish        servant      enterpris       virginia          wrote           abus           lead          place           year        histori 
##     0.73029674     0.73029674     0.73029674     0.73029674     0.73029674     0.73029674     0.73029674     0.73029674     0.73029674     0.72295689     0.71972749     0.71914652     0.71749217 
##          feder        program        poverti        product         resolv           pass           work           fail         defens       interest        societi       individu         purpos 
##     0.71713717     0.71611487     0.71611487     0.70891756     0.70891756     0.70849191     0.70417963     0.70064905     0.70064905     0.69829725     0.69566559     0.69560834     0.69560834 
##          polit           race            pay           ride           slow       independ         worthi          trust       confront           show           fill           play         import 
##     0.69402209     0.69282032     0.69282032     0.69282032     0.69282032     0.69282032     0.69282032     0.69006556     0.69006556     0.68853037     0.68853037     0.68853037     0.68853037 
##          peopl         nation        respons         return           sick           said           find       american          reduc           valu          never         declar        citizen 
##     0.68752137     0.68689567     0.68422569     0.68313005     0.68313005     0.68313005     0.68313005     0.68162120     0.68030913     0.67916218     0.67813269     0.67612340     0.67434031 
##         demand          small           deep            act         number           told        support          storm            can       strength          power       sacrific          today 
##     0.67131711     0.67124862     0.67082039     0.67082039     0.67082039     0.67082039     0.66943868     0.66943868     0.66906672     0.66759195     0.66580030     0.65828059     0.65685666 
##           done       principl        present         restor           kill           away       progress          futur          spend         person         solemn          bound      strongest 
##     0.65556228     0.65465367     0.65319726     0.65319726     0.65319726     0.65158377     0.65079137     0.65060005     0.64597519     0.64549722     0.64549722     0.64549722     0.64549722 
##         histor          georg          spoke          liber           poor         safeti          doubt           step           earn            let           just         commit          simpl 
##     0.64549722     0.64549722     0.64549722     0.64549722     0.64549722     0.64549722     0.64549722     0.64450339     0.64450339     0.64214762     0.63909218     0.63900965     0.63900965 
##           road          world           will          endur            man            two          reach           bush          decis          month           line          short           fate 
##     0.63900965     0.63688854     0.63683037     0.63671454     0.63305416     0.63245553     0.63245553     0.63245553     0.63245553     0.63245553     0.63245553     0.63245553     0.63245553 
##         mutual           west             go          found           hero           live           know            law          chanc       children         remain           last         answer 
##     0.63245553     0.63245553     0.63031524     0.62925320     0.62828086     0.62812062     0.62705445     0.62622429     0.62622429     0.62604751     0.62330229     0.62330229     0.62279916 
##         direct      technolog          blood           life          capac          young         effort         presid           oath          state          group           goal          price 
##     0.62279916     0.62279916     0.62279916     0.62253176     0.62105900     0.61993042     0.61967734     0.61429512     0.61101009     0.60900019     0.60858062     0.60858062     0.60858062 
##          share         famili         togeth           look          decad         energi         strive            tri         common           hear         servic           keep           seek 
##     0.60764362     0.60485838     0.60302269     0.60286056     0.60246408     0.60246408     0.60246408     0.60246408     0.60246408     0.60246408     0.60246408     0.60000000     0.59761430 
##       reverend            goe           cast        collect         whatev          manag         capabl         around          grown      establish        unleash           self          aspir 
##     0.59628479     0.59628479     0.59628479     0.59628479     0.59628479     0.59628479     0.59628479     0.59628479     0.59628479     0.59628479     0.59628479     0.59628479     0.59628479 
##         negoti          yield           knew            cut           bond          begun         domest        conduct        conquer         uphold           rage            one        greater 
##     0.59628479     0.59628479     0.59628479     0.59628479     0.59628479     0.59628479     0.59628479     0.59628479     0.59628479     0.59628479     0.59628479     0.59585816     0.59336610 
##       determin           rais          shall          civil           help         tradit            men           feed           held          allow       destruct          renew            war 
##     0.59234888     0.59234888     0.59234888     0.59016889     0.58848989     0.58554004     0.58554004     0.58554004     0.58554004     0.58554004     0.58554004     0.58468835     0.58430655 
##         father           hope           past          whose          shown         remind         depend        inaugur           hill           kind          local     birthright          drive 
##     0.58430655     0.58068320     0.57975090     0.57975090     0.57735027     0.57735027     0.57735027     0.57735027     0.57735027     0.57735027     0.57735027     0.57735027     0.57735027 
##          music        heritag          spare          senat          great          labor         provid            end        victori         affirm          speak         public          still 
##     0.57735027     0.57735027     0.57735027     0.57535596     0.57319733     0.57289190     0.57289190     0.57207755     0.57154761     0.57154761     0.56803756     0.56803756     0.56779227 
##        forward            put          secur          stori          build           noth           born           gift         though      communiti           make          crisi          human 
##     0.56453449     0.56343617     0.55906539     0.55434789     0.55328334     0.55205245     0.55205245     0.55205245     0.55205245     0.55181927     0.55059776     0.55048188     0.54996441 
##         fellow           alon        increas        process           busi          worst         borrow           week       boundari         patrol         except       recognit         reserv 
##     0.54902140     0.54772256     0.54772256     0.54772256     0.54772256     0.54772256     0.54772256     0.54772256     0.54772256     0.54772256     0.54772256     0.54772256     0.54772256 
##        command         heroic          quiet         balanc          decid     strengthen        forbear       shoulder          humil        dignifi          river      arlington           paid 
##     0.54772256     0.54772256     0.54772256     0.54772256     0.54772256     0.54772256     0.54772256     0.54772256     0.54772256     0.54772256     0.54772256     0.54772256     0.54772256 
##        earlier            lie           fame         repres         bestow      horseback            raw              4         wherev           took          equip         dramat         modern 
##     0.54772256     0.54772256     0.54772256     0.54772256     0.54772256     0.54772256     0.54772256     0.54772256     0.54772256     0.54772256     0.54772256     0.54772256     0.54772256 
##           tide      overwhelm         hungri        treasur           debt            due       unfortun         origin         awesom          waver       research         missil         nowher 
##     0.54772256     0.54772256     0.54772256     0.54772256     0.54772256     0.54772256     0.54772256     0.54772256     0.54772256     0.54772256     0.54772256     0.54772256     0.54772256 
##          youth           snow         valley         affect         expect           lest      conscienc          grace       grandest          delay           wind        serious           soil 
##     0.54772256     0.54772256     0.54772256     0.54772256     0.54772256     0.54772256     0.54772256     0.54772256     0.54772256     0.54772256     0.54772256     0.54772256     0.54772256 
##        respect          cynic        medicar         mistak          favor      substitut       stranger        respond           lend         search          basic        subject          swift 
##     0.54772256     0.54772256     0.54772256     0.54772256     0.54772256     0.54772256     0.54772256     0.54772256     0.54772256     0.54772256     0.54772256     0.54772256     0.54772256 
##           turn         moment         follow        nuclear           need        journey         spirit          happi          thank            arm          birth        problem          everi 
##     0.54718845     0.54494926     0.54494926     0.54451008     0.54221767     0.54175265     0.54029547     0.53838190     0.53708616     0.53333333     0.53333333     0.53136893     0.53085304 
##          chang           long          equal          chief            els         realiz      adversari            met           hall         invest         school          ambit            bad 
##     0.53072278     0.52964090     0.52894368     0.52704628     0.52704628     0.52704628     0.52704628     0.52704628     0.52704628     0.52704628     0.52704628     0.52704628     0.52704628 
##          stake          women        generat        america           face           grow            say          might           care        destroy           back         achiev         requir 
##     0.52704628     0.52704628     0.52587376     0.52586849     0.52223297     0.52174919     0.52174919     0.52174919     0.52174919     0.52174919     0.52092370     0.51961524     0.51961524 
##        freedom         health       hatfield         mondal          baker         moomaw         occurr         routin          uniqu         realli   every-4-year         normal        transit 
##     0.51645531     0.51639778     0.51639778     0.51639778     0.51639778     0.51639778     0.51639778     0.51639778     0.51639778     0.51639778     0.51639778     0.51639778     0.51639778 
##          degre        bulwark        afflict        proport        longest        distort          penal         thrift          crush    fixed-incom           alik        shatter            idl 
##     0.51639778     0.51639778     0.51639778     0.51639778     0.51639778     0.51639778     0.51639778     0.51639778     0.51639778     0.51639778     0.51639778     0.51639778     0.51639778 
##         indign         burden           kept           pile        mortgag      temporari        conveni          trend        tremend         upheav         period  misunderstand          sever 
##     0.51639778     0.51639778     0.51639778     0.51639778     0.51639778     0.51639778     0.51639778     0.51639778     0.51639778     0.51639778     0.51639778     0.51639778     0.51639778 
##        bastion          tempt        complex       self-rul           elit       superior         someon          equit          singl        neglect        section           food           mine 
##     0.51639778     0.51639778     0.51639778     0.51639778     0.51639778     0.51639778     0.51639778     0.51639778     0.51639778     0.51639778     0.51639778     0.51639778     0.51639778 
##          teach     profession  industrialist       shopkeep          clerk          cabbi      truckdriv          breed        healthi          vigor      discrimin        runaway          reviv 
##     0.51639778     0.51639778     0.51639778     0.51639778     0.51639778     0.51639778     0.51639778     0.51639778     0.51639778     0.51639778     0.51639778     0.51639778     0.51639778 
##      inventori          check        consent         intent           curb       distinct        smother         foster          stifl         extent          avail        coincid       parallel 
##     0.51639778     0.51639778     0.51639778     0.51639778     0.51639778     0.51639778     0.51639778     0.51639778     0.51639778     0.51639778     0.51639778     0.51639778     0.51639778 
##     proportion      intervent         intrus         result    unnecessari         excess           loom        creativ           gate        counter   entrepreneur      voluntari            art 
##     0.51639778     0.51639778     0.51639778     0.51639778     0.51639778     0.51639778     0.51639778     0.51639778     0.51639778     0.51639778     0.51639778     0.51639778     0.51639778 
##        address         makeup     countrymen        suffici         theori      unequivoc         emphat      paraphras        winston       churchil       dissolut          ahead         propos 
##     0.51639778     0.51639778     0.51639778     0.51639778     0.51639778     0.51639778     0.51639778     0.51639778     0.51639778     0.51639778     0.51639778     0.51639778     0.51639778 
##          remov      roadblock        various          level         measur           inch           feet           mile       reawaken          giant        lighten          punit            eve 
##     0.51639778     0.51639778     0.51639778     0.51639778     0.51639778     0.51639778     0.51639778     0.51639778     0.51639778     0.51639778     0.51639778     0.51639778     0.51639778 
##             dr         joseph         warren   massachusett        despair       exemplar         beacon          match       benefici    sovereignti           sale        surrend  misunderstood 
##     0.51639778     0.51639778     0.51639778     0.51639778     0.51639778     0.51639778     0.51639778     0.51639778     0.51639778     0.51639778     0.51639778     0.51639778     0.51639778 
##        misjudg        prevail         formid        practic           prey            ten         deepli          vista           mall         shrine       monument  revolutionari         infant 
##     0.51639778     0.51639778     0.51639778     0.51639778     0.51639778     0.51639778     0.51639778     0.51639778     0.51639778     0.51639778     0.51639778     0.51639778     0.51639778 
##     nationhood          eloqu           pool         column        whoever        heroism        potomac          shore          slope       cemeteri            row          white         marker 
##     0.51639778     0.51639778     0.51639778     0.51639778     0.51639778     0.51639778     0.51639778     0.51639778     0.51639778     0.51639778     0.51639778     0.51639778     0.51639778 
##          david           tini       fraction        belleau           wood         argonn          omaha          beach        salerno        halfway      guadalcan         tarawa           pork 
##     0.51639778     0.51639778     0.51639778     0.51639778     0.51639778     0.51639778     0.51639778     0.51639778     0.51639778     0.51639778     0.51639778     0.51639778     0.51639778 
##           chop         chosin      reservoir          hundr           rice          paddi          jungl         barber           shop           1917          franc        rainbow        western 
##     0.51639778     0.51639778     0.51639778     0.51639778     0.51639778     0.51639778     0.51639778     0.51639778     0.51639778     0.51639778     0.51639778     0.51639778     0.51639778 
##      battalion          heavi      artilleri          diari        flyleaf       therefor          cheer        treptow        perform           deed         mathia         burger        presenc 
##     0.51639778     0.51639778     0.51639778     0.51639778     0.51639778     0.51639778     0.51639778     0.51639778     0.51639778     0.51639778     0.51639778     0.51639778     0.51639778 
##         absent         stenni          gilli      louisiana         silent          adequ           50th          stood         wilder             13             60             50           gone 
##     0.51639778     0.51639778     0.51639778     0.51639778     0.51639778     0.51639778     0.51639778     0.51639778     0.51639778     0.51639778     0.51639778     0.51639778     0.51639778 
##            cri           moon         stress          glori    present-day       backward         proper         machin           1980          ultim           rate         employ        vibrant 
##     0.51639778     0.51639778     0.51639778     0.51639778     0.51639778     0.51639778     0.51639778     0.51639778     0.51639778     0.51639778     0.51639778     0.51639778     0.51639778 
##         robust          climb         restat          freed           grip         sincer        meaning         reduct        develop           warm       sunlight           pois         golden 
##     0.51639778     0.51639778     0.51639778     0.51639778     0.51639778     0.51639778     0.51639778     0.51639778     0.51639778     0.51639778     0.51639778     0.51639778     0.51639778 
##           gain      two-parti     republican         boston         lawyer           adam        planter          rival           1800          later         soften          anger         letter 
##     0.51639778     0.51639778     0.51639778     0.51639778     0.51639778     0.51639778     0.51639778     0.51639778     0.51639778     0.51639778     0.51639778     0.51639778     0.51639778 
##    reestablish           1826    anniversari            die         fourth           juli        exchang         sunset          beset        valuabl            oar       harmless           rode 
##     0.51639778     0.51639778     0.51639778     0.51639778     0.51639778     0.51639778     0.51639778     0.51639778     0.51639778     0.51639778     0.51639778     0.51639778     0.51639778 
##    well-intent          error          futil          chase          bloat      prescript        reelect           1984         vindic             25       straight         incent  entrepreneuri 
##     0.51639778     0.51639778     0.51639778     0.51639778     0.51639778     0.51639778     0.51639778     0.51639778     0.51639778     0.51639778     0.51639778     0.51639778     0.51639778 
##       interfer       simplifi          least        emancip           tear       distress      literatur         poetri          dynam       unbroken        brought         reckon          staff 
##     0.51639778     0.51639778     0.51639778     0.51639778     0.51639778     0.51639778     0.51639778     0.51639778     0.51639778     0.51639778     0.51639778     0.51639778     0.51639778 
##         submit          freez          desir    unconstitut        alreadi          handl      fundament         upgrad         infirm    disadvantag         instal        hearten    brotherhood 
##     0.51639778     0.51639778     0.51639778     0.51639778     0.51639778     0.51639778     0.51639778     0.51639778     0.51639778     0.51639778     0.51639778     0.51639778     0.51639778 
##          hesit          abund          utter        fervent          scorn        buildup         offens        legitim        discuss         elimin         either         resort         retali 
##     0.51639778     0.51639778     0.51639778     0.51639778     0.51639778     0.51639778     0.51639778     0.51639778     0.51639778     0.51639778     0.51639778     0.51639778     0.51639778 
##          logic        recours         approv         shield        militar      demilitar         render        obsolet            rid       fourfold      hemispher       worldwid  self-determin 
##     0.51639778     0.51639778     0.51639778     0.51639778     0.51639778     0.51639778     0.51639778     0.51639778     0.51639778     0.51639778     0.51639778     0.51639778     0.51639778 
##        inalien     staunchest        inflict         lightn      transcend         ribbon         unfurl         symbol          insid        general           knee           lone         darken 
##     0.51639778     0.51639778     0.51639778     0.51639778     0.51639778     0.51639778     0.51639778     0.51639778     0.51639778     0.51639778     0.51639778     0.51639778     0.51639778 
##         ponder          alamo       encourag        settler           sing           song         unknow      big-heart         tender       knowledg           rare           gore        contest 
##     0.51639778     0.51639778     0.51639778     0.51639778     0.51639778     0.51639778     0.51639778     0.51639778     0.51639778     0.51639778     0.51639778     0.51639778     0.51639778 
##     slave-hold           went        fallibl          grand     insignific          enact           halt           rock            sea           seed           root         inborn            225 
##     0.51639778     0.51639778     0.51639778     0.51639778     0.51639778     0.51639778     0.51639778     0.51639778     0.51639778     0.51639778     0.51639778     0.51639778     0.51639778 
##         hidden         onward           deal         forgiv         appear       undermin         permit         tactic           chao         inspir        condemn         apathi        prevent 
##     0.51639778     0.51639778     0.51639778     0.51639778     0.51639778     0.51639778     0.51639778     0.51639778     0.51639778     0.51639778     0.51639778     0.51639778     0.51639778 
##          recov       momentum          invit           mass         horror          arrog        aggress     compassion       unworthi           view          fault       prolifer       diminish 
##     0.51639778     0.51639778     0.51639778     0.51639778     0.51639778     0.51639778     0.51639778     0.51639778     0.51639778     0.51639778     0.51639778     0.51639778     0.51639778 
##         mentor         pastor       synagogu          mosqu          wound        jericho      scapegoat         option          civic        uncount        unhonor        comfort        spectat 
##     0.51639778     0.51639778     0.51639778     0.51639778     0.51639778     0.51639778     0.51639778     0.51639778     0.51639778     0.51639778     0.51639778     0.51639778     0.51639778 
##           miss      statesman          angel      whirlwind        accumul          theme           tire         finish            day            see       maintain           task            ago 
##     0.51639778     0.51639778     0.51639778     0.51639778     0.51639778     0.51639778     0.51639778     0.51639778     0.51136738     0.51011279     0.50709255     0.50709255     0.50636968 
##        million           love           made         member         deserv           safe           voic        revolut           rich        special         soviet          union       greatest 
##     0.50617107     0.50421948     0.50410083     0.50128041     0.50128041     0.50128041     0.50128041     0.50128041     0.50128041     0.50097943     0.50097943     0.50037023     0.49690399 
##            use          ideal           less       ceremoni            ill           heal           save          child          front         memori        countri         cultur     understood 
##     0.49690399     0.49497475     0.49028113     0.48989795     0.48989795     0.48989795     0.48989795     0.48989795     0.48989795     0.48989795     0.48854276     0.48795004     0.48795004 
##           dark         reward           sinc         afford         embrac           hard           duti          enemi           open         threat          creed           come           caus 
##     0.48795004     0.48795004     0.48795004     0.48795004     0.48795004     0.48795004     0.48388670     0.48304589     0.48304589     0.48304589     0.47946330     0.47609523     0.47469288 
##         toward           also          right          bring       challeng       neighbor        capitol          light          shape           unit          other           left          start 
##     0.47387910     0.47387910     0.47373778     0.47249954     0.46790960     0.46709937     0.46709937     0.46709937     0.46709937     0.46709937     0.46692400     0.46666667     0.46666667 
##          sound         cooper       guarante      administr          grant         reluct       conflict            led        lincoln        willing           serv           dare          elect 
##     0.46666667     0.46188022     0.46188022     0.46188022     0.46188022     0.46188022     0.46188022     0.46188022     0.46188022     0.46188022     0.46188022     0.46188022     0.46188022 
##           area        allianc        possibl           echo        possess        depress          wrong       generous           risk           idea         matter          readi          final 
##     0.46188022     0.46188022     0.46188022     0.46188022     0.46188022     0.46188022     0.46188022     0.46188022     0.46188022     0.46106249     0.45643546     0.45643546     0.45643546 
##           name       prejudic         decent            era       question           join         belong        sometim        prosper           mean      democraci      constitut            eye 
##     0.45643546     0.45643546     0.45643546     0.45643546     0.45643546     0.45643546     0.45643546     0.45573272     0.45542003     0.45117883     0.45074894     0.44721360     0.44721360 
##         rather         inevit          ensur           firm       magnific          flame           star         welcom            aid          enjoy         reborn     presidenti           send 
##     0.44721360     0.44721360     0.44721360     0.44721360     0.44721360     0.44721360     0.44721360     0.44721360     0.44721360     0.44721360     0.44721360     0.44721360     0.44721360 
##           bold           next         corner         spoken        instead           cold           forg          dedic           wait          battl           easi         unfold        fascism 
##     0.44721360     0.44721360     0.44721360     0.44721360     0.44721360     0.44721360     0.44721360     0.44721360     0.44721360     0.44721360     0.44721360     0.44721360     0.44721360 
##           wage          remak        persist         contin         temper           gave         replac           plan          claim           lose          petti    distinguish          guest 
##     0.44721360     0.44721360     0.44721360     0.44721360     0.44721360     0.44721360     0.44721360     0.44721360     0.44721360     0.44721360     0.44721360     0.44721360     0.44721360 
##         vulner         exampl           weak          pursu          dream            ask         confid            may        charact       tomorrow        loyalti          forev           hour 
##     0.44721360     0.44721360     0.44721360     0.44721360     0.44597990     0.44280744     0.43835700     0.43699022     0.43643578     0.43033148     0.43033148     0.43033148     0.42966892 
##     throughout         better            old          taken          cross        reflect       democrat         lesson       stronger        ancient           seiz          brave          becam 
##     0.42966892     0.42899599     0.42769946     0.42600643     0.42163702     0.42163702     0.42163702     0.42163702     0.42163702     0.42163702     0.42163702     0.42163702     0.42163702 
##          humbl        clinton       industri     understand           even         budget            age         justic            job            new          heart           hatr         terror 
##     0.42163702     0.42163702     0.41464421     0.41464421     0.41464421     0.41403934     0.41079192     0.41039134     0.41039134     0.41010365     0.40931190     0.40824829     0.40824829 
##         worker          parti         spring          choos          drift        without       congress         reform         friend          alway           word         fulfil           guid 
##     0.40824829     0.40824829     0.40824829     0.40824829     0.40824829     0.40824829     0.40509575     0.40414519     0.40407529     0.40323892     0.40160966     0.40000000     0.40000000 
##          truth          watch           free            yes         within          moral           four           anew      communism         planet         immigr        patriot           deni 
##     0.40000000     0.39440532     0.39260996     0.39036003     0.39036003     0.39036003     0.39036003     0.39036003     0.39036003     0.39036003     0.39036003     0.38924947     0.38729833 
##         beauti           bodi         wonder         invent          given            rob          retir          march        commerc          vital          along          anoth         celebr 
##     0.38729833     0.38729833     0.38729833     0.38729833     0.38729833     0.38729833     0.38729833     0.38729833     0.38729833     0.38729833     0.38729833     0.38357064     0.38202872 
##         defend         across          debat         vision         differ          uniti          defin     commonplac         miracl       gracious          elder         miseri          becom 
##     0.38202872     0.37619206     0.37267800     0.37267800     0.36885556     0.36514837     0.36514837     0.36514837     0.36514837     0.36514837     0.36514837     0.36514837     0.36514837 
##         racial         object        bigotri         bounti          unwil      compromis            fit        abraham            far            add           town         messag        written 
##     0.36514837     0.36514837     0.36514837     0.36514837     0.36514837     0.36514837     0.36514837     0.36514837     0.36514837     0.36514837     0.36514837     0.36514837     0.36514837 
##         martin           dole         clergi       brighter           amen          untam        mankind         master        consist           aliv    self-govern   totalitarian         remark 
##     0.36514837     0.36514837     0.36514837     0.36514837     0.36514837     0.36514837     0.36514837     0.36514837     0.36514837     0.36514837     0.36514837     0.36514837     0.36514837 
##     difficulti         repeat         spiral         disast           tool        cabinet          money       servitud           bill          older         custom          black        primari 
##     0.36514837     0.36514837     0.36514837     0.36514837     0.36514837     0.36514837     0.36514837     0.36514837     0.36514837     0.36514837     0.36514837     0.36514837     0.36514837 
##         target          space        noblest         hunger         mighti         expans            lit         gotten           push            air       idealist          saint         deeper 
##     0.36514837     0.36514837     0.36514837     0.36514837     0.36514837     0.36514837     0.36514837     0.36514837     0.36514837     0.36514837     0.36514837     0.36514837     0.36514837 
##      sentiment       circumst           hurt           flaw          depth       reinvent         weaken        univers       competit       profound         harder         devast          crise 
##     0.36514837     0.36514837     0.36514837     0.36514837     0.36514837     0.36514837     0.36514837     0.36514837     0.36514837     0.36514837     0.36514837     0.36514837     0.36514837 
##        foundat          engin            saw           sacr           toil       privileg           asid         abroad         shrink         recogn        convict       scriptur            joy 
##     0.36514837     0.36514837     0.36514837     0.36514837     0.36514837     0.36514837     0.36514837     0.36514837     0.36514837     0.36514837     0.36514837     0.36514837     0.36514837 
##          touch         taught        reclaim           imag     background         prison       hopeless         listen         privat          anyth          arriv          etern       document 
##     0.36514837     0.36514837     0.36514837     0.36514837     0.36514837     0.36514837     0.36514837     0.36514837     0.36514837     0.36514837     0.36514837     0.36514837     0.36514837 
##        network            sap           lash        faction        unmatch         surest         reject         precis      uncertain           give        sustain         leader           hand 
##     0.36514837     0.36514837     0.36514837     0.36514837     0.36514837     0.36514837     0.36514837     0.36514837     0.36514837     0.35856858     0.35805744     0.35805744     0.35781322 
##           forc          divis          grate       timeless         winter        pursuit         imagin           vote          skill          settl         scienc        qualiti          drawn 
##     0.35437465     0.34641016     0.34641016     0.34641016     0.34641016     0.34641016     0.34641016     0.34641016     0.34641016     0.34641016     0.34641016     0.34641016     0.34641016 
##          cours          solut       thousand           path          forth          first        protect        factori           fear           sake         poster           true          honor 
##     0.34503278     0.34426519     0.34426519     0.34426519     0.34426519     0.34262414     0.33946737     0.33333333     0.32659863     0.32659863     0.32659863     0.32659863     0.32461723 
##         rememb           home         advanc     washington         promis           high          relat            win       mountain        rebuild        deepest          often          sworn 
##     0.32461723     0.32302914     0.32302914     0.32274861     0.32064193     0.31622777     0.31622777     0.31622777     0.31622777     0.31622777     0.31622777     0.31622777     0.31622777 
##           pain    citizenship            run        abandon         belief          sourc        speaker       transfer         action          began           lift          clear            set 
##     0.31622777     0.31622777     0.31622777     0.31622777     0.31622777     0.31622777     0.31139958     0.31139958     0.31139958     0.31139958     0.31139958     0.31139958     0.31139958 
##           land          thing        everyon            tie          impos        highest           citi           issu          night   neighborhood         behalf        resourc           cure 
##     0.31108551     0.30389098     0.30382181     0.29814240     0.29814240     0.29814240     0.29814240     0.29814240     0.29814240     0.29814240     0.29814240     0.29814240     0.29814240 
##          break          globe           role           near         colleg          ignor          recal         surviv           evid       hardship           good         higher         fortun 
##     0.29814240     0.29814240     0.29814240     0.29814240     0.29814240     0.29814240     0.29814240     0.29814240     0.29814240     0.29814240     0.29439203     0.29277002     0.29277002 
##           wave        control         mother      necessari           mind        liberti         accept           came        success         longer           soul          choic           part 
##     0.29277002     0.29277002     0.29277002     0.29277002     0.28867513     0.28232038     0.28171808     0.27824334     0.27602622     0.27602622     0.27524094     0.27247463     0.26943013 
##             mr          close         market         parent           lost      difficult           sure           bind         oldest    predecessor   half-centuri      steadfast         shadow 
##     0.26363525     0.25819889     0.25819889     0.25819889     0.25819889     0.25819889     0.25819889     0.25819889     0.25819889     0.25819889     0.25819889     0.25819889     0.25819889 
##        sunshin        unrival        inherit       stagnant          inequ           news         slowli           boat          sight      broadcast      instantan      tobillion       communic 
##     0.25819889     0.25819889     0.25819889     0.25819889     0.25819889     0.25819889     0.25819889     0.25819889     0.25819889     0.25819889     0.25819889     0.25819889     0.25819889 
##          mobil          magic     livelihood          shake            abl         compet       bankrupt           abid           erod         shaken        fearsom       restless         muster 
##     0.25819889     0.25819889     0.25819889     0.25819889     0.25819889     0.25819889     0.25819889     0.25819889     0.25819889     0.25819889     0.25819889     0.25819889     0.25819889 
##      construct         pillar           envi       deadlock         season         massiv         wander          revit        intrigu         calcul         maneuv          posit          worri 
##     0.25819889     0.25819889     0.25819889     0.25819889     0.25819889     0.25819889     0.25819889     0.25819889     0.25819889     0.25819889     0.25819889     0.25819889     0.25819889 
##          sweat           pave          shout       advantag       franklin      roosevelt     experiment          stabl        collaps         animos         engulf         intern           defi 
##     0.25819889     0.25819889     0.25819889     0.25819889     0.25819889     0.25819889     0.25819889     0.25819889     0.25819889     0.25819889     0.25819889     0.25819889     0.25819889 
##        persian           gulf        somalia      testament         rejoic       unmistak       undertak      reconnect           torn           inde          reded         myriad         upward 
##     0.25819889     0.25819889     0.25819889     0.25819889     0.25819889     0.25819889     0.25819889     0.25819889     0.25819889     0.25819889     0.25819889     0.25819889     0.25819889 
##      disciplin        well-do          faint    mountaintop          guard       ancestor     forty-four         amidst          cloud         simpli          midst      far-reach        consequ 
##     0.25819889     0.25819889     0.25819889     0.25819889     0.25819889     0.25819889     0.25819889     0.25819889     0.25819889     0.25819889     0.25819889     0.25819889     0.25819889 
##          greed      irrespons           shed          indic           data        statist            nag          lower         easili           span       grievanc           fals       recrimin 
##     0.25819889     0.25819889     0.25819889     0.25819889     0.25819889     0.25819889     0.25819889     0.25819889     0.25819889     0.25819889     0.25819889     0.25819889     0.25819889 
##       worn-out          dogma        strangl       childish           nobl      god-given       shortcut    faint-heart         leisur        pleasur       risk-tak           doer     things'som 
##     0.25819889     0.25819889     0.25819889     0.25819889     0.25819889     0.25819889     0.25819889     0.25819889     0.25819889     0.25819889     0.25819889     0.25819889     0.25819889 
##         obscur            rug           pack      sweatshop           whip           plow         fought        concord     gettysburg       normandi            khe           sahn         sacrif 
##     0.25819889     0.25819889     0.25819889     0.25819889     0.25819889     0.25819889     0.25819889     0.25819889     0.25819889     0.25819889     0.25819889     0.25819889     0.25819889 
##           till     undiminish            pat        unpleas           pick           dust            lay         electr           grid          digit          wield            sun      transform 
##     0.25819889     0.25819889     0.25819889     0.25819889     0.25819889     0.25819889     0.25819889     0.25819889     0.25819889     0.25819889     0.25819889     0.25819889     0.25819889 
##          scale        suggest         necess          shift        beneath          stale       argument         consum          appli           wise         expand           spin          gross 
##     0.25819889     0.25819889     0.25819889     0.25819889     0.25819889     0.25819889     0.25819889     0.25819889     0.25819889     0.25819889     0.25819889     0.25819889     0.25819889 
##           abil           rout          peril          scarc          draft        charter         expedi         villag           tank         sturdi         entitl          pleas           eman 
##     0.25819889     0.25819889     0.25819889     0.25819889     0.25819889     0.25819889     0.25819889     0.25819889     0.25819889     0.25819889     0.25819889     0.25819889     0.25819889 
##      restraint         keeper           iraq      hard-earn    afghanistan         former            foe       tireless         lessen        specter         apolog          induc      slaughter 
##     0.25819889     0.25819889     0.25819889     0.25819889     0.25819889     0.25819889     0.25819889     0.25819889     0.25819889     0.25819889     0.25819889     0.25819889     0.25819889 
##          innoc        outlast      patchwork      christian         muslim            jew         hindus     non-believ        languag           tast         bitter          swill         segreg 
##     0.25819889     0.25819889     0.25819889     0.25819889     0.25819889     0.25819889     0.25819889     0.25819889     0.25819889     0.25819889     0.25819889     0.25819889     0.25819889 
##        someday          tribe        dissolv          usher            sow          blame          cling        corrupt         deceit         silenc       unclench       alongsid          clean 
##     0.25819889     0.25819889     0.25819889     0.25819889     0.25819889     0.25819889     0.25819889     0.25819889     0.25819889     0.25819889     0.25819889     0.25819889     0.25819889 
##        nourish          starv         plenti       indiffer         outsid         regard       gratitud        far-off         desert        whisper       guardian         embodi           leve 
##     0.25819889     0.25819889     0.25819889     0.25819889     0.25819889     0.25819889     0.25819889     0.25819889     0.25819889     0.25819889     0.25819889     0.25819889     0.25819889 
##       selfless      firefight       stairway          smoke         nurtur     instrument        honesti         curios           glad        satisfi          sixti        restaur        remembr 
##     0.25819889     0.25819889     0.25819889     0.25819889     0.25819889     0.25819889     0.25819889     0.25819889     0.25819889     0.25819889     0.25819889     0.25819889     0.25819889 
##        coldest           band          huddl        campfir            ici         outcom          virtu          alarm        current         falter            fix          deliv          biden 
##     0.25819889     0.25819889     0.25819889     0.25819889     0.25819889     0.25819889     0.25819889     0.25819889     0.25819889     0.25819889     0.25819889     0.25819889     0.25819889 
##          color           skin          tenet        articul      self-evid          endow        unalien      never-end    self-execut           1776            mob        entrust          sword 
##     0.25819889     0.25819889     0.25819889     0.25819889     0.25819889     0.25819889     0.25819889     0.25819889     0.25819889     0.25819889     0.25819889     0.25819889     0.25819889 
##      half-slav       half-fre       railroad          speed          train         discov         hazard      misfortun     relinquish        skeptic        central        fiction          initi 
##     0.25819889     0.25819889     0.25819889     0.25819889     0.25819889     0.25819889     0.25819889     0.25819889     0.25819889     0.25819889     0.25819889     0.25819889     0.25819889 
##         insist          fidel         musket        militia           math        teacher            lab          steel          prove         resili       recoveri           bare          brink 
##     0.25819889     0.25819889     0.25819889     0.25819889     0.25819889     0.25819889     0.25819889     0.25819889     0.25819889     0.25819889     0.25819889     0.25819889     0.25819889 
##       bleakest        anybodi        outworn        inadequ         revamp           code          empow       twilight         disabl          lucki           loss         sudden          swept 
##     0.25819889     0.25819889     0.25819889     0.25819889     0.25819889     0.25819889     0.25819889     0.25819889     0.25819889     0.25819889     0.25819889     0.25819889     0.25819889 
##        terribl       medicaid          taker         climat         betray       judgment           none         impact        drought         resist           cede         forest       waterway 
##     0.25819889     0.25819889     0.25819889     0.25819889     0.25819889     0.25819889     0.25819889     0.25819889     0.25819889     0.25819889     0.25819889     0.25819889     0.25819889 
##           crop       snow-cap           peak        perpetu        uniform           sear           harm           heir            won           naiv      suspicion         anchor           asia 
##     0.25819889     0.25819889     0.25819889     0.25819889     0.25819889     0.25819889     0.25819889     0.25819889     0.25819889     0.25819889     0.25819889     0.25819889     0.25819889 
##         africa           east         compel         margin        describ       forebear         seneca          selma       stonewal           sung         unsung      footprint       preacher 
##     0.25819889     0.25819889     0.25819889     0.25819889     0.25819889     0.25819889     0.25819889     0.25819889     0.25819889     0.25819889     0.25819889     0.25819889     0.25819889 
##       inextric        pioneer           wive       daughter            gay        brother         sister         enlist       workforc          expel     appalachia           lane        newtown 
##     0.25819889     0.25819889     0.25819889     0.25819889     0.25819889     0.25819889     0.25819889     0.25819889     0.25819889     0.25819889     0.25819889     0.25819889     0.25819889 
##        cherish        contour          exact centuries-long        absolut       spectacl       name-cal      imperfect        partial             40            400           henc         confer 
##     0.25819889     0.25819889     0.25819889     0.25819889     0.25819889     0.25819889     0.25819889     0.25819889     0.25819889     0.25819889     0.25819889     0.25819889     0.25819889 
##   philadelphia          recit          durat        passion        complet         strong        whether          fight         someth          toler         extend           real           want 
##     0.25819889     0.25819889     0.25819889     0.25819889     0.25318484     0.24866795     0.24845200     0.24494897     0.24494897     0.24494897     0.24494897     0.24494897     0.23354968 
##          offer       influenc           solv        vietnam           head          point       militari          total           blow         summon            har         decenc          treat 
##     0.23354968     0.23094011     0.23094011     0.23094011     0.23094011     0.23094011     0.23094011     0.23094011     0.23094011     0.23094011     0.23094011     0.23094011     0.23094011 
##         attack       reaffirm           rule         street           leav           seem         wealth         produc            get        potenti           bibl           king          water 
##     0.23094011     0.23094011     0.22537447     0.22360680     0.22360680     0.22019275     0.21535276     0.21081851     0.21081851     0.21081851     0.21081851     0.21081851     0.21081851 
##        endless         larger          ocean          habit       constant          oblig       precious           test          pride           move         gather     friendship          whole 
##     0.21081851     0.21081851     0.21081851     0.21081851     0.21081851     0.21081851     0.21081851     0.21081851     0.21081851     0.21081851     0.20701967     0.19518001     0.19518001 
##           sens         divers          anyon           rise           tell          offic          bridg           fire          engag         ground         broken         reveal            sum 
##     0.19518001     0.19518001     0.19518001     0.19518001     0.19518001     0.19518001     0.19518001     0.18257419     0.18257419     0.18257419     0.18257419     0.18257419     0.18257419 
##            car        account        prudent         chorus        discord           soon           fist         effect          vigil        trumpet        mysteri       almighti          plagu 
##     0.18257419     0.18257419     0.18257419     0.18257419     0.18257419     0.18257419     0.18257419     0.18257419     0.18257419     0.18257419     0.18257419     0.18257419     0.18257419 
##         urgent         enrich        fractur          sleep           dawn      yesterday        environ      diplomaci         whenev        compani         ennobl          infus          weari 
##     0.18257419     0.18257419     0.18257419     0.18257419     0.18257419     0.18257419     0.18257419     0.18257419     0.18257419     0.18257419     0.18257419     0.18257419     0.18257419 
##           reap      limitless        smaller           roll           fuel         crippl        succumb      everywher          broad          stain         legaci           girl           farm 
##     0.18257419     0.18257419     0.18257419     0.18257419     0.18257419     0.18257419     0.18257419     0.18257419     0.18257419     0.18257419     0.18257419     0.18257419     0.18257419 
##           mark         durabl        distant        violenc          woman          maker          emerg         prefer          grudg        dissent         defeat        darkest         prepar 
##     0.18257419     0.18257419     0.18257419     0.18257419     0.18257419     0.18257419     0.18257419     0.18257419     0.18257419     0.18257419     0.18257419     0.18257419     0.18257419 
##           judg         victim         consid           flow        inhabit         dollar       flourish         fallen          refus        horizon        creator        highway          littl 
##     0.18257419     0.18257419     0.18257419     0.18257419     0.18257419     0.18257419     0.18257419     0.18257419     0.18257419     0.18257419     0.18257419     0.18257419     0.18257419 
##          spent        student        detroit         reason        centuri           stop        succeed        founder        mission          middl         thrive        soldier           like 
##     0.18257419     0.18257419     0.18257419     0.18257419     0.17807996     0.17213259     0.17213259     0.17213259     0.17213259     0.17213259     0.17213259     0.17213259     0.16807316 
##        oppress          learn            big           page         perman       proclaim           educ          assum           walk        exercis           base         honest          heard 
##     0.16329932     0.16329932     0.16329932     0.16329932     0.15569979     0.15569979     0.14907120     0.14907120     0.14907120     0.14907120     0.14907120     0.14907120     0.14907120 
##           mere          salut           flag          truli        chapter         global         forget        shutter          avoid         narrow        realiti          capit         enough 
##     0.14907120     0.14907120     0.14907120     0.14907120     0.14907120     0.14907120     0.14907120     0.14907120     0.14907120     0.14907120     0.14907120     0.14322297     0.12565617 
##        express            200        generos         execut          quest         bright      forgotten         bigger        certain            vow       movement          built           read 
##     0.12524486     0.11547005     0.11547005     0.11547005     0.11547005     0.11547005     0.11547005     0.10540926     0.10540926     0.10540926     0.10540926     0.10540926     0.10540926 
##         allegi           fact          crime            edg           feel        destini           21st            wit       institut           reli           seen         chosen        foreign 
##     0.10540926     0.10127394     0.09759001     0.09759001     0.09759001     0.09759001     0.08164966     0.08164966     0.08164966     0.08164966     0.07968191     0.07784989     0.07784989 
##        triumph          class         border        tyranni          quayl        mitchel         wright    congressman        michael         reagan     bicentenni        gladden        concret 
##     0.07784989     0.07784989     0.06900656     0.04969040     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000 
##           stun          porch           talk        suspend            bow         heaven           heed          write           lord          breez        refresh         dictat          blown 
##     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000 
##       lifeless           tree          thick            fog            sit           mist           door           room           agit        thought     intellectu      satisfact         speech 
##     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000 
##         unhamp         perhap           late           form          wrest        crucial          proud           loud        enthral         materi        appreci          nobil           bank 
##     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000 
##          loyal         driven           stay          trade        quieter           gold           silk          finer         wholli         unless         kinder        gentler       homeless 
##     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000 
##           roam       normalci         enslav         addict           drug         welfar          demor           slum          rough        guidanc           case           fund            low 
##     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000 
##         wallet          alloc         wisest          activ       hands-on         involv           unus         talent        unfocus     leadership    stewardship         second          organ 
##     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000 
##         spread           hous          agenc          pitch         thrash         fiscal        dissens        harmoni      statement          motiv          apart        untrust          cleav 
##     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000 
##        earnest        quarter         statut         sunder bipartisanship        opposit          major          clock           wish          await         bicker       partisan      unaccount 
##     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000 
##         assist          beget         treati      agreement          marbl         candor     compliment       subtleti   relationship         experi          throw            son           hymn 
##     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000 
##      continuum        inescap        connect           kite        neither          princ           pope         window          yearn          going        attitud        intoler        obvious 
##     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000 
##         cocain         smuggl           ship           dead       bacteria         scourg       mistrust           larg      boundless          drama           book           20th     millennium 
##     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000 
##       prospect         affair           18th           19th        abolish             aw        slaveri        turmoil         explod           onto          stage      mightiest          unriv 
##     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000 
##         center          split           atom         explor         comput      microchip         deepen        wellspr        african          minor          circl          third          coast 
##     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000 
##        conserv         inform        perfect        tragedi        exhilar      indispens        cleaner         destin           bend          safer         record        flexibl       everyday 
##     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000 
##        preemin           hire         behind           lock           gang          divid           curs       contempt          cloak        pretens         religi        fanatic        torment 
##     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000 
##         obsess           hate         impuls           lurk         region        overcom         textur        godsend       approach         outlin       internet         mystic        provinc 
##     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000 
##      physicist   encyclopedia schoolchildren      scientist          decod      blueprint         hostil           camp   dictatorship        surpass        bloodsh        resound         sought 
##     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000 
##          prize       standard          ignit          spark            boy      classroom        librari        kitchen           tabl       laughter          shoot           sell         anymor 
##     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000 
##        medicin       hardwork         chemic         biolog           port        airport          innov       grandpar  grandchildren        benefit        fortifi          natur         majest 
##     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000 
##         louder            din         regain       particip           armi    thirty-four        prophet         luther      ceaseless         redeem         extrem   partisanship          plain 
##     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000 
##         deplor         repair         breach         wisdom         cardin      bernardin           wast       acrimoni        patienc           wide          belov         height         summit 
##     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000 
##         cheney         carter            non       prescrib    consequenti           half      shipwreck          repos         sabbat         simmer         resent          prone        ideolog 
##     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000 
##          excus         murder       multipli         mortal          reign          expos         tyrant          event        conclus      matchless          imper          slave         polici 
##     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000 
##      primarili           aris          style         attain       concentr        unlimit       consider          unwis        clarifi          ruler        pretend           jail         dissid 
##     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000 
##          chain         humili          merci          bulli      treatment        concess         appeal       swiftest            odd        surpris         eventu          exist      oppressor 
##     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000 
##        repress           exil         outlaw          regim         retain        counsel        concert         promot         prelud       dishonor          kindl           burn        hardest 
##     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000 
##       intellig          devot          death       youngest         fragil           evil        essenti       unfinish        subsist        broader        definit      homestead             gi 
##     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000 
##      ownership          widen          insur          agent         integr         edific          sinai         sermon          mount          koran           vari       surround         unwant 
##     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000 
##          worth         racism         baggag       perspect         includ      viewpoint         credit          known           felt     fellowship         unjust        encount         captiv 
##     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000 
##          wheel         outrag         banner          meant            ebb         visibl           bell           rang        thereof         robert          obama           ladi        michell 
##     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000 
##             dc     politician        januari             20           2017      righteous           trap          inner     rusted-out        scatter       tombston       landscap          flush 
##     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000 
##           cash         depriv         stolen         unreal         carnag       glorious         expens         subsid            sad         deplet       trillion        oversea  infrastructur 
##     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000 
##      disrepair          decay         dissip            rip    redistribut        assembl          decre          ravag          steal         breath         tunnel        railway            buy 
##     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000 
##        goodwil          shine       reinforc          radic          islam           erad        bedrock       rediscov       pleasant         disagr        solidar        unstopp         enforc 
##     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000 
##       complain          empti         unlock         diseas           stir          brown          bleed            red          urban         sprawl      windswept       nebraska            sky 
##     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000 
##        wealthi 
##     0.00000000 
## 
## $health
##          shape        generat          wrong         common       knowledg         planet           task         demand            eye          defin           forc         danger          child 
##     0.90453403     0.89711799     0.89442719     0.88888889     0.88888889     0.88191710     0.87287156     0.86666667     0.86602540     0.86424162     0.86415857     0.84327404     0.84327404 
##          choos           fear         extend           true        without           long         advanc         servic        commerc          vital          power         deserv           less 
##     0.84327404     0.84327404     0.84327404     0.84327404     0.84327404     0.83571089     0.83405766     0.83333333     0.83333333     0.83333333     0.83268373     0.83205029     0.83074716 
##          everi           busi          endur         spirit         reform         school          ambit            bad          brave          humbl            can           face            law 
##     0.82550912     0.82495791     0.82199494     0.82060994     0.81989159     0.81649658     0.81649658     0.81649658     0.81649658     0.81649658     0.80977633     0.80903983     0.80845208 
##         travel            set           gift       interest           just          storm            end            see           play         measur          forth        respons           give 
##     0.80403025     0.80403025     0.80178373     0.78881064     0.78756153     0.78567420     0.77957944     0.77828964     0.77777778     0.77777778     0.77777778     0.77733178     0.77151675 
##           care        instead        fascism           wage          remak          break         temper          globe           role           plan         colleg           lose         narrow 
##     0.76980036     0.76980036     0.76980036     0.76980036     0.76980036     0.76980036     0.76980036     0.76980036     0.76980036     0.76980036     0.76980036     0.76980036     0.76980036 
##          petti           rage           weak          recal         surviv           evid           life           also           even          build           know           find      communism 
##     0.76980036     0.76980036     0.76980036     0.76980036     0.76980036     0.76980036     0.76874221     0.76471911     0.76471911     0.76190476     0.76190476     0.75592895     0.75592895 
##     understood         afford          choic         failur           serv           race        possess       timeless         winter         imagin       reaffirm          settl        qualiti 
##     0.75592895     0.75592895     0.75377836     0.74535599     0.74535599     0.74535599     0.74535599     0.74535599     0.74535599     0.74535599     0.74535599     0.74535599     0.74535599 
##         courag           last        america         nation           hour            ill           hatr         someth          toler           real            way            may          still 
##     0.74420841     0.74278135     0.74204617     0.74041700     0.73960026     0.73786479     0.73786479     0.73786479     0.73786479     0.73786479     0.73773716     0.73773716     0.73301667 
##          faith           oath          ideal           duti         confid          carri         purpos          futur        prosper           born         threat         gather          enemi 
##     0.73258967     0.73246702     0.73029674     0.72881089     0.72760688     0.72222222     0.71842121     0.71393289     0.71393289     0.71269665     0.71269665     0.71269665     0.71269665 
##          trust           meet           valu          crisi           work           must            era          quiet       question           join           road          depth         weaken 
##     0.71269665     0.71239561     0.71239561     0.71066905     0.70878358     0.70747196     0.70710678     0.70710678     0.70710678     0.70710678     0.70710678     0.70710678     0.70710678 
##        univers       profound        foundat            saw           sacr           toil           asid      conscienc        convict       scriptur        smaller           roll           fuel 
##     0.70710678     0.70710678     0.70710678     0.70710678     0.70710678     0.70710678     0.70710678     0.70710678     0.70710678     0.70710678     0.70710678     0.70710678     0.70710678 
##      everywher          stain         legaci           farm          grace       grandest           wind        serious           soil        respect          cynic          favor       stranger 
##     0.70710678     0.70710678     0.70710678     0.70710678     0.70710678     0.70710678     0.70710678     0.70710678     0.70710678     0.70710678     0.70710678     0.70710678     0.70710678 
##         search        subject          swift           mark        distant        violenc          woman          maker          emerg         prefer          grudg        dissent         defeat 
##     0.70710678     0.70710678     0.70710678     0.70710678     0.70710678     0.70710678     0.70710678     0.70710678     0.70710678     0.70710678     0.70710678     0.70710678     0.70710678 
##        darkest         prepar           judg         consid           flow        inhabit       document        network            sap           lash        faction        unmatch         surest 
##     0.70710678     0.70710678     0.70710678     0.70710678     0.70710678     0.70710678     0.70710678     0.70710678     0.70710678     0.70710678     0.70710678     0.70710678     0.70710678 
##         reject         precis      uncertain         answer         defens          blood        journey         promis            war          today           guid          truth          birth 
##     0.70710678     0.70710678     0.70710678     0.70352647     0.70352647     0.70352647     0.69939967     0.69853547     0.69631062     0.68899862     0.68853037     0.68853037     0.68853037 
##          pledg       determin          shall           come          small         remain            met       stronger          often          water          habit           seiz    citizenship 
##     0.68824720     0.68824720     0.68824720     0.68694645     0.68088291     0.68088291     0.68041382     0.68041382     0.68041382     0.68041382     0.68041382     0.68041382     0.68041382 
##           read       precious            run        abandon          sourc           test             us           well        charact       challeng           time         rather         vision 
##     0.68041382     0.68041382     0.68041382     0.68041382     0.68041382     0.68041382     0.67933588     0.67675297     0.67612340     0.67513560     0.67391764     0.67357531     0.67357531 
##           grow           mind         depend         effort        chariti           mall          ultim           warm          retir        alreadi           poor          doubt           path 
##     0.67357531     0.67082039     0.67082039     0.66666667     0.66666667     0.66666667     0.66666667     0.66666667     0.66666667     0.66666667     0.66666667     0.66666667     0.66666667 
##         parent           lost      difficult           sure           bind          sight       ancestor     forty-four         amidst          cloud         simpli          midst      far-reach 
##     0.66666667     0.66666667     0.66666667     0.66666667     0.66666667     0.66666667     0.66666667     0.66666667     0.66666667     0.66666667     0.66666667     0.66666667     0.66666667 
##        consequ          greed      irrespons           shed          indic           data        statist            nag          lower         easili           span       grievanc           fals 
##     0.66666667     0.66666667     0.66666667     0.66666667     0.66666667     0.66666667     0.66666667     0.66666667     0.66666667     0.66666667     0.66666667     0.66666667     0.66666667 
##       recrimin       worn-out          dogma        strangl       childish           nobl      god-given       shortcut    faint-heart         leisur        pleasur       risk-tak           doer 
##     0.66666667     0.66666667     0.66666667     0.66666667     0.66666667     0.66666667     0.66666667     0.66666667     0.66666667     0.66666667     0.66666667     0.66666667     0.66666667 
##     things'som         obscur            rug           pack      sweatshop           whip           plow         fought        concord     gettysburg       normandi            khe           sahn 
##     0.66666667     0.66666667     0.66666667     0.66666667     0.66666667     0.66666667     0.66666667     0.66666667     0.66666667     0.66666667     0.66666667     0.66666667     0.66666667 
##         sacrif           till     undiminish            pat        unpleas           pick           dust            lay         electr           grid          digit          wield            sun 
##     0.66666667     0.66666667     0.66666667     0.66666667     0.66666667     0.66666667     0.66666667     0.66666667     0.66666667     0.66666667     0.66666667     0.66666667     0.66666667 
##      transform          scale        suggest         necess          shift        beneath          stale       argument         consum          appli           wise         expand           spin 
##     0.66666667     0.66666667     0.66666667     0.66666667     0.66666667     0.66666667     0.66666667     0.66666667     0.66666667     0.66666667     0.66666667     0.66666667     0.66666667 
##          gross           abil           rout          peril          scarc          draft        charter         expedi         villag           tank         sturdi         entitl          pleas 
##     0.66666667     0.66666667     0.66666667     0.66666667     0.66666667     0.66666667     0.66666667     0.66666667     0.66666667     0.66666667     0.66666667     0.66666667     0.66666667 
##           eman      restraint         keeper           iraq      hard-earn    afghanistan         former            foe       tireless         lessen        specter         apolog          induc 
##     0.66666667     0.66666667     0.66666667     0.66666667     0.66666667     0.66666667     0.66666667     0.66666667     0.66666667     0.66666667     0.66666667     0.66666667     0.66666667 
##      slaughter          innoc        outlast      patchwork      christian         muslim            jew         hindus     non-believ        languag           tast         bitter          swill 
##     0.66666667     0.66666667     0.66666667     0.66666667     0.66666667     0.66666667     0.66666667     0.66666667     0.66666667     0.66666667     0.66666667     0.66666667     0.66666667 
##         segreg        someday          tribe        dissolv          usher            sow          blame          cling        corrupt         deceit         silenc       unclench       alongsid 
##     0.66666667     0.66666667     0.66666667     0.66666667     0.66666667     0.66666667     0.66666667     0.66666667     0.66666667     0.66666667     0.66666667     0.66666667     0.66666667 
##          clean        nourish          starv         plenti       indiffer         outsid         regard       gratitud        far-off         desert        whisper       guardian         embodi 
##     0.66666667     0.66666667     0.66666667     0.66666667     0.66666667     0.66666667     0.66666667     0.66666667     0.66666667     0.66666667     0.66666667     0.66666667     0.66666667 
##           leve       selfless      firefight       stairway          smoke         nurtur     instrument        honesti         curios           glad        satisfi          sixti        restaur 
##     0.66666667     0.66666667     0.66666667     0.66666667     0.66666667     0.66666667     0.66666667     0.66666667     0.66666667     0.66666667     0.66666667     0.66666667     0.66666667 
##        remembr        coldest           band          huddl        campfir            ici         outcom          virtu          alarm        current         falter            fix          deliv 
##     0.66666667     0.66666667     0.66666667     0.66666667     0.66666667     0.66666667     0.66666667     0.66666667     0.66666667     0.66666667     0.66666667     0.66666667     0.66666667 
##       children         celebr        citizen          stand            now        greater         fellow            let       american         father            new          women          polit 
##     0.65855277     0.65759595     0.65736888     0.65433031     0.65001125     0.64818122     0.64715023     0.64059787     0.63913552     0.63828474     0.63532984     0.63505290     0.63245553 
##            big           said      necessari          offic           feed           hard         embrac          bridg           pass           alon           home           noth         longer 
##     0.63245553     0.62994079     0.62994079     0.62994079     0.62994079     0.62994079     0.62994079     0.62994079     0.62882815     0.62853936     0.62554324     0.62360956     0.62360956 
##           call         across           seek          world            yet     understand         toward          peopl          light           fail       proclaim      communiti         public 
##     0.62103443     0.61811510     0.61721340     0.61430198     0.61276335     0.61177529     0.61177529     0.60564711     0.60302269     0.60302269     0.60302269     0.60279629     0.60000000 
##           lead          bless           live         cooper         achiev           full       conflict          grate        willing        possibl            har          skill         scienc 
##     0.60000000     0.59750544     0.59750544     0.59628479     0.59628479     0.59628479     0.59628479     0.59628479     0.59628479     0.59628479     0.59628479     0.59628479     0.59628479 
##          drawn           will           upon           make           move     strengthen            far            age         decent         resolv            men         person         declar 
##     0.59628479     0.59563528     0.59522911     0.59234888     0.58969198     0.58925565     0.58925565     0.58925565     0.58925565     0.58834841     0.58794474     0.58333333     0.58191437 
##           rule          found           cost        collect        societi          manag         inevit         declin          might           firm           knew            cut     presidenti 
##     0.58191437     0.58025885     0.57735027     0.57735027     0.57735027     0.57735027     0.57735027     0.57735027     0.57735027     0.57735027     0.57735027     0.57735027     0.57735027 
##           bold           next         domest           forg           base        resourc        whether         unfold        chapter         uphold         global           cure        persist 
##     0.57735027     0.57735027     0.57735027     0.57735027     0.57735027     0.57735027     0.57735027     0.57735027     0.57735027     0.57735027     0.57735027     0.57735027     0.57735027 
##         contin           gave        shutter         replac          avoid          claim    distinguish          guest         vulner         exampl          pursu       hardship            job 
##     0.57735027     0.57735027     0.57735027     0.57735027     0.57735027     0.57735027     0.57735027     0.57735027     0.57735027     0.57735027     0.57735027     0.57735027     0.57396402 
##        forward         famili          alway          civil        liberti        economi          chanc           caus           peac       principl          anoth          creed           hope 
##     0.57263713     0.57263713     0.57263713     0.57142857     0.56695962     0.56591646     0.56591646     0.56568542     0.56545929     0.56343617     0.55708601     0.55708601     0.55619846 
##          happi           show            die          forev         market        founder        mission          equal          labor     throughout        revolut         leader          chang 
##     0.55603844     0.55555556     0.55555556     0.55555556     0.55555556     0.55555556     0.55555556     0.55482648     0.55470020     0.55470020     0.55470020     0.55470020     0.55339859 
##           word         moment           year            god            act           bush          month           line          short          whose          reach         mutual          relat 
##     0.55304090     0.55277080     0.55124591     0.54965657     0.54527525     0.54433105     0.54433105     0.54433105     0.54433105     0.54433105     0.54433105     0.54433105     0.54433105 
##           west        ancient          sworn          built          oblig          stake         belief           came          honor        histori           unit           mani           rais 
##     0.54433105     0.54433105     0.54433105     0.54433105     0.54433105     0.54433105     0.54433105     0.53881591     0.53881591     0.53626644     0.53602017     0.53530338     0.53530338 
##        success         though           need          secur         justic          order         worker          learn           sake           reli         differ       sacrific         requir 
##     0.53452248     0.53452248     0.53333333     0.53181602     0.52981294     0.52704628     0.52704628     0.52704628     0.52704628     0.52704628     0.52380952     0.52297636     0.52174919 
##           fair            old           take            ask         accept          watch           ever      democraci           land        countri          never         cultur       opportun 
##     0.51639778     0.51534712     0.51449576     0.51449576     0.50917508     0.50917508     0.50917508     0.50917508     0.50870557     0.50806872     0.50507627     0.50395263     0.50395263 
##         fortun           dark           wave         reward           anew            edg           rise           feel        destini         immigr           idea        patriot      technolog 
##     0.50395263     0.50395263     0.50395263     0.50395263     0.50395263     0.50395263     0.50395263     0.50395263     0.50395263     0.50395263     0.50365540     0.50251891     0.50251891 
##          began           lift         chosen          clear         solemn        transit           deni          bound          singl           size          shore         invent          given 
##     0.50251891     0.50251891     0.50251891     0.50251891     0.50000000     0.50000000     0.50000000     0.50000000     0.50000000     0.50000000     0.50000000     0.50000000     0.50000000 
##          liber         safeti          march           hold          along        freedom        digniti          right         defend         togeth          earth           free         author 
##     0.50000000     0.50000000     0.50000000     0.50000000     0.50000000     0.49756786     0.49690399     0.49509897     0.49319696     0.49304933     0.48154341     0.48151442     0.48112522 
##            say           deep          debat          creat         better          uniti           week           bear         patrol       recognit          decid         commit        forbear 
##     0.48112522     0.48112522     0.48112522     0.47894747     0.47471266     0.47140452     0.47140452     0.47140452     0.47140452     0.47140452     0.47140452     0.47140452     0.47140452 
##          humil        dignifi          river      arlington        earlier            lie           fame         bestow            raw      enterpris           tide         hungri       prejudic 
##     0.47140452     0.47140452     0.47140452     0.47140452     0.47140452     0.47140452     0.47140452     0.47140452     0.47140452     0.47140452     0.47140452     0.47140452     0.47140452 
##          waver         missil           snow         ground         broken         reveal            sum            car        account        prudent        discord           soon           fist 
##     0.47140452     0.47140452     0.47140452     0.47140452     0.47140452     0.47140452     0.47140452     0.47140452     0.47140452     0.47140452     0.47140452     0.47140452     0.47140452 
##         effect       reinvent          plagu       competit         urgent         harder         devast        fractur          crise          engin          sleep         expect           dawn 
##     0.47140452     0.47140452     0.47140452     0.47140452     0.47140452     0.47140452     0.47140452     0.47140452     0.47140452     0.47140452     0.47140452     0.47140452     0.47140452 
##       privileg      yesterday         abroad        environ         shrink           lest      diplomaci         whenev         recogn         ennobl          weari            joy        slaveri 
##     0.47140452     0.47140452     0.47140452     0.47140452     0.47140452     0.47140452     0.47140452     0.47140452     0.47140452     0.47140452     0.47140452     0.47140452     0.47140452 
##          minor      limitless          touch         taught        reclaim        pretens         crippl        succumb         region          broad           girl          natur       particip 
##     0.47140452     0.47140452     0.47140452     0.47140452     0.47140452     0.47140452     0.47140452     0.47140452     0.47140452     0.47140452     0.47140452     0.47140452     0.47140452 
##        patienc          delay           imag     background        medicar         mistak         prison      substitut       hopeless        respond           lend         privat          basic 
##     0.47140452     0.47140452     0.47140452     0.47140452     0.47140452     0.47140452     0.47140452     0.47140452     0.47140452     0.47140452     0.47140452     0.47140452     0.47140452 
##          etern         durabl         victim         dollar       flourish         fallen          refus        horizon            day          great          speak        sustain           voic 
##     0.47140452     0.47140452     0.47140452     0.47140452     0.47140452     0.47140452     0.47140452     0.47140452     0.46850948     0.46736499     0.46666667     0.46225016     0.46225016 
##       congress          bring          young       strength       greatest        tyranni          shown          divis       independ        inaugur            led        allianc        pursuit 
##     0.45760432     0.45749571     0.45732956     0.45360921     0.44905021     0.44905021     0.44721360     0.44721360     0.44721360     0.44721360     0.44721360     0.44721360     0.44721360 
##        depress          quest           vote         bright         decenc       generous       standard          treat          spare           risk         attack           done          cours 
##     0.44721360     0.44721360     0.44721360     0.44721360     0.44721360     0.44721360     0.44721360     0.44721360     0.44721360     0.44721360     0.44721360     0.44543540     0.44543540 
##        centuri         suffer         energi            tri           fill        succeed          middl         thrive        soldier            yes           good          other          human 
##     0.44447381     0.44444444     0.44444444     0.44444444     0.44444444     0.44444444     0.44444444     0.44444444     0.44444444     0.44095855     0.43852901     0.43839730     0.43692369 
##           mean           like            one          renew           keep         fulfil           soul         terror         restor          drift         poster           21st         affirm 
##     0.43685203     0.43396304     0.43357704     0.43133109     0.43033148     0.43033148     0.42640143     0.42163702     0.42163702     0.42163702     0.42163702     0.42163702     0.42163702 
##            wit       institut        million         rememb         wealth          thank          decis            els           fate      adversari       democrat         invest        deepest 
##     0.42163702     0.42163702     0.42008403     0.41907904     0.41702883     0.41602515     0.40824829     0.40824829     0.40824829     0.40824829     0.40824829     0.40824829     0.40824829 
##           king        certain         larger          ocean          becam       constant        clinton          pride        capitol         direct         follow         perman        triumph 
##     0.40824829     0.40824829     0.40824829     0.40824829     0.40824829     0.40824829     0.40824829     0.40824829     0.40201513     0.40201513     0.40201513     0.40201513     0.40201513 
##          class         presid         govern          place          state          taken           fire        complet      constitut           cast         street         around           sign 
##     0.40201513     0.39652579     0.39434447     0.39310793     0.39310793     0.39283710     0.39283710     0.39223227     0.38490018     0.38490018     0.38490018     0.38490018     0.38490018 
##        highest         intend       magnific          thoma          flame         welcom          enjoy           bond          begun         corner         spoken        conduct           agre 
##     0.38490018     0.38490018     0.38490018     0.38490018     0.38490018     0.38490018     0.38490018     0.38490018     0.38490018     0.38490018     0.38490018     0.38490018     0.38490018 
##           cold          dedic         behalf         heaven          assum           walk        exercis          battl           easi         forget           near         wisdom        realiti 
##     0.38490018     0.38490018     0.38490018     0.38490018     0.38490018     0.38490018     0.38490018     0.38490018     0.38490018     0.38490018     0.38490018     0.38490018     0.38490018 
##          ignor            use           help         enough         tradit         return           held          whole           sens        control       destruct         divers           tell 
##     0.38490018     0.38490018     0.37986859     0.37851665     0.37796447     0.37796447     0.37796447     0.37796447     0.37796447     0.37796447     0.37796447     0.37796447     0.37796447 
##        continu           side           safe           earn        program        poverti          capit         friend         strong           best        protect          capac         border 
##     0.37139068     0.36980013     0.36980013     0.36980013     0.36980013     0.36980013     0.36980013     0.36822985     0.36115756     0.36004115     0.35856858     0.35634832     0.35634832 
##           seem         social         balanc          readi           alli          simpl          final           name          union          begin       threaten            arm        problem 
##     0.35533453     0.35355339     0.35355339     0.35355339     0.35355339     0.35355339     0.35355339     0.35355339     0.34783280     0.34717043     0.34426519     0.34426519     0.34299717 
##          heart           vice          decad           hear          teach      strongest         import          match         strive          spoke           bodi         wonder            rob 
##     0.34191843     0.33333333     0.33333333     0.33333333     0.33333333     0.33333333     0.33333333     0.33333333     0.33333333     0.33333333     0.33333333     0.33333333     0.33333333 
##       encourag         oldest    predecessor   half-centuri      steadfast         shadow        sunshin        unrival        inherit       stagnant          inequ           news         slowli 
##     0.33333333     0.33333333     0.33333333     0.33333333     0.33333333     0.33333333     0.33333333     0.33333333     0.33333333     0.33333333     0.33333333     0.33333333     0.33333333 
##           boat      broadcast      instantan      tobillion       communic          mobil          magic     livelihood          shake            abl         compet       bankrupt           abid 
##     0.33333333     0.33333333     0.33333333     0.33333333     0.33333333     0.33333333     0.33333333     0.33333333     0.33333333     0.33333333     0.33333333     0.33333333     0.33333333 
##           erod         shaken        fearsom       restless         muster      construct         pillar           envi       deadlock         season         massiv         wander          revit 
##     0.33333333     0.33333333     0.33333333     0.33333333     0.33333333     0.33333333     0.33333333     0.33333333     0.33333333     0.33333333     0.33333333     0.33333333     0.33333333 
##        intrigu         calcul         maneuv          posit          worri          sweat           pave          shout       advantag       franklin      roosevelt     experiment          stabl 
##     0.33333333     0.33333333     0.33333333     0.33333333     0.33333333     0.33333333     0.33333333     0.33333333     0.33333333     0.33333333     0.33333333     0.33333333     0.33333333 
##        collaps         animos         engulf         intern           defi        persian           gulf        somalia      testament         rejoic       unmistak       undertak      reconnect 
##     0.33333333     0.33333333     0.33333333     0.33333333     0.33333333     0.33333333     0.33333333     0.33333333     0.33333333     0.33333333     0.33333333     0.33333333     0.33333333 
##           torn           inde          reded         myriad         upward      disciplin        well-do          faint    mountaintop          guard           20th       prospect           18th 
##     0.33333333     0.33333333     0.33333333     0.33333333     0.33333333     0.33333333     0.33333333     0.33333333     0.33333333     0.33333333     0.33333333     0.33333333     0.33333333 
##           19th        abolish             aw        turmoil         explod           onto          stage      mightiest          unriv          split           atom         explor         comput 
##     0.33333333     0.33333333     0.33333333     0.33333333     0.33333333     0.33333333     0.33333333     0.33333333     0.33333333     0.33333333     0.33333333     0.33333333     0.33333333 
##      microchip         deepen        wellspr        african          circl          third          coast        conserv         inform        perfect        tragedi        exhilar      indispens 
##     0.33333333     0.33333333     0.33333333     0.33333333     0.33333333     0.33333333     0.33333333     0.33333333     0.33333333     0.33333333     0.33333333     0.33333333     0.33333333 
##        cleaner         destin           bend          safer         record        flexibl       everyday        preemin           lock          divid           curs       contempt          cloak 
##     0.33333333     0.33333333     0.33333333     0.33333333     0.33333333     0.33333333     0.33333333     0.33333333     0.33333333     0.33333333     0.33333333     0.33333333     0.33333333 
##         religi        fanatic        torment         obsess           hate         impuls           lurk        overcom         textur        godsend       approach         outlin       internet 
##     0.33333333     0.33333333     0.33333333     0.33333333     0.33333333     0.33333333     0.33333333     0.33333333     0.33333333     0.33333333     0.33333333     0.33333333     0.33333333 
##         mystic        provinc      physicist   encyclopedia schoolchildren      scientist          decod      blueprint         hostil           camp   dictatorship        surpass        bloodsh 
##     0.33333333     0.33333333     0.33333333     0.33333333     0.33333333     0.33333333     0.33333333     0.33333333     0.33333333     0.33333333     0.33333333     0.33333333     0.33333333 
##        resound         sought          prize          ignit          spark            boy      classroom        librari        kitchen           tabl       laughter          shoot           sell 
##     0.33333333     0.33333333     0.33333333     0.33333333     0.33333333     0.33333333     0.33333333     0.33333333     0.33333333     0.33333333     0.33333333     0.33333333     0.33333333 
##         anymor        medicin       hardwork         chemic         biolog           port          innov       grandpar  grandchildren        fortifi         majest         louder            din 
##     0.33333333     0.33333333     0.33333333     0.33333333     0.33333333     0.33333333     0.33333333     0.33333333     0.33333333     0.33333333     0.33333333     0.33333333     0.33333333 
##         regain    thirty-four        prophet         luther      ceaseless         redeem         extrem   partisanship         deplor         repair         breach         cardin      bernardin 
##     0.33333333     0.33333333     0.33333333     0.33333333     0.33333333     0.33333333     0.33333333     0.33333333     0.33333333     0.33333333     0.33333333     0.33333333     0.33333333 
##           wast       acrimoni           wide          belov         height         summit           rare           gore        contest     slave-hold           went        fallibl          grand 
##     0.33333333     0.33333333     0.33333333     0.33333333     0.33333333     0.33333333     0.33333333     0.33333333     0.33333333     0.33333333     0.33333333     0.33333333     0.33333333 
##     insignific          enact           halt           rock            sea           seed           root         inborn            225         hidden         onward           deal         forgiv 
##     0.33333333     0.33333333     0.33333333     0.33333333     0.33333333     0.33333333     0.33333333     0.33333333     0.33333333     0.33333333     0.33333333     0.33333333     0.33333333 
##         appear       undermin         permit         tactic           chao         inspir        condemn         apathi        prevent          recov       momentum          invit           mass 
##     0.33333333     0.33333333     0.33333333     0.33333333     0.33333333     0.33333333     0.33333333     0.33333333     0.33333333     0.33333333     0.33333333     0.33333333     0.33333333 
##         horror          arrog        aggress     compassion       unworthi           view          fault       prolifer       diminish         mentor         pastor       synagogu          mosqu 
##     0.33333333     0.33333333     0.33333333     0.33333333     0.33333333     0.33333333     0.33333333     0.33333333     0.33333333     0.33333333     0.33333333     0.33333333     0.33333333 
##          wound        jericho      scapegoat         option          civic        uncount        unhonor        comfort        spectat           miss      statesman          angel      whirlwind 
##     0.33333333     0.33333333     0.33333333     0.33333333     0.33333333     0.33333333     0.33333333     0.33333333     0.33333333     0.33333333     0.33333333     0.33333333     0.33333333 
##        accumul          theme           tire         finish         cheney            non       prescrib    consequenti           half      shipwreck          repos         sabbat         simmer 
##     0.33333333     0.33333333     0.33333333     0.33333333     0.33333333     0.33333333     0.33333333     0.33333333     0.33333333     0.33333333     0.33333333     0.33333333     0.33333333 
##         resent          prone        ideolog          excus         murder       multipli         mortal          reign          expos         tyrant          event        conclus      matchless 
##     0.33333333     0.33333333     0.33333333     0.33333333     0.33333333     0.33333333     0.33333333     0.33333333     0.33333333     0.33333333     0.33333333     0.33333333     0.33333333 
##          imper          slave         polici      primarili           aris          style         attain       concentr        unlimit       consider          unwis        clarifi        pretend 
##     0.33333333     0.33333333     0.33333333     0.33333333     0.33333333     0.33333333     0.33333333     0.33333333     0.33333333     0.33333333     0.33333333     0.33333333     0.33333333 
##           jail         dissid          chain         humili          merci          bulli      treatment        concess         appeal       swiftest            odd        surpris         eventu 
##     0.33333333     0.33333333     0.33333333     0.33333333     0.33333333     0.33333333     0.33333333     0.33333333     0.33333333     0.33333333     0.33333333     0.33333333     0.33333333 
##      oppressor        repress           exil         outlaw          regim         retain        counsel        concert         promot         prelud       dishonor          kindl           burn 
##     0.33333333     0.33333333     0.33333333     0.33333333     0.33333333     0.33333333     0.33333333     0.33333333     0.33333333     0.33333333     0.33333333     0.33333333     0.33333333 
##        hardest       intellig          devot          death       youngest         fragil           evil        essenti       unfinish        subsist        broader        definit      homestead 
##     0.33333333     0.33333333     0.33333333     0.33333333     0.33333333     0.33333333     0.33333333     0.33333333     0.33333333     0.33333333     0.33333333     0.33333333     0.33333333 
##             gi      ownership          widen          insur          agent         integr         edific          sinai         sermon          mount          koran           vari       surround 
##     0.33333333     0.33333333     0.33333333     0.33333333     0.33333333     0.33333333     0.33333333     0.33333333     0.33333333     0.33333333     0.33333333     0.33333333     0.33333333 
##         unwant          worth         racism         baggag       perspect         includ      viewpoint         credit          known           felt     fellowship         unjust        encount 
##     0.33333333     0.33333333     0.33333333     0.33333333     0.33333333     0.33333333     0.33333333     0.33333333     0.33333333     0.33333333     0.33333333     0.33333333     0.33333333 
##         captiv          wheel         outrag         banner          meant            ebb         visibl           bell           rang        thereof          biden          color           skin 
##     0.33333333     0.33333333     0.33333333     0.33333333     0.33333333     0.33333333     0.33333333     0.33333333     0.33333333     0.33333333     0.33333333     0.33333333     0.33333333 
##          tenet        articul      self-evid          endow        unalien      never-end    self-execut           1776            mob        entrust          sword      half-slav       half-fre 
##     0.33333333     0.33333333     0.33333333     0.33333333     0.33333333     0.33333333     0.33333333     0.33333333     0.33333333     0.33333333     0.33333333     0.33333333     0.33333333 
##       railroad          speed          train         discov         hazard      misfortun     relinquish        skeptic        central        fiction          initi         insist          fidel 
##     0.33333333     0.33333333     0.33333333     0.33333333     0.33333333     0.33333333     0.33333333     0.33333333     0.33333333     0.33333333     0.33333333     0.33333333     0.33333333 
##         musket        militia           math        teacher            lab          steel          prove         resili       recoveri           bare          brink       bleakest        anybodi 
##     0.33333333     0.33333333     0.33333333     0.33333333     0.33333333     0.33333333     0.33333333     0.33333333     0.33333333     0.33333333     0.33333333     0.33333333     0.33333333 
##        outworn        inadequ         revamp           code          empow       twilight         disabl          lucki           loss         sudden          swept        terribl       medicaid 
##     0.33333333     0.33333333     0.33333333     0.33333333     0.33333333     0.33333333     0.33333333     0.33333333     0.33333333     0.33333333     0.33333333     0.33333333     0.33333333 
##          taker         climat         betray       judgment           none         impact        drought         resist           cede         forest       waterway           crop       snow-cap 
##     0.33333333     0.33333333     0.33333333     0.33333333     0.33333333     0.33333333     0.33333333     0.33333333     0.33333333     0.33333333     0.33333333     0.33333333     0.33333333 
##           peak        perpetu        uniform           sear           harm           heir            won           naiv      suspicion         anchor           asia         africa           east 
##     0.33333333     0.33333333     0.33333333     0.33333333     0.33333333     0.33333333     0.33333333     0.33333333     0.33333333     0.33333333     0.33333333     0.33333333     0.33333333 
##         compel         margin        describ       forebear         seneca          selma       stonewal           sung         unsung      footprint       preacher       inextric        pioneer 
##     0.33333333     0.33333333     0.33333333     0.33333333     0.33333333     0.33333333     0.33333333     0.33333333     0.33333333     0.33333333     0.33333333     0.33333333     0.33333333 
##           wive       daughter            gay        brother         sister         enlist       workforc          expel     appalachia           lane        newtown        cherish        contour 
##     0.33333333     0.33333333     0.33333333     0.33333333     0.33333333     0.33333333     0.33333333     0.33333333     0.33333333     0.33333333     0.33333333     0.33333333     0.33333333 
##          exact centuries-long        absolut       spectacl       name-cal      imperfect        partial             40            400           henc         confer   philadelphia          recit 
##     0.33333333     0.33333333     0.33333333     0.33333333     0.33333333     0.33333333     0.33333333     0.33333333     0.33333333     0.33333333     0.33333333     0.33333333     0.33333333 
##          durat        passion          thing        preserv        product            ago        sometim       progress         believ          stori           much       ceremoni          parti 
##     0.33333333     0.33333333     0.33196440     0.32879797     0.32686023     0.32686023     0.32686023     0.32673202     0.32551538     0.32530002     0.32357511     0.31622777     0.31622777 
##         memori      jefferson           save          fight         spring        oppress         spread           goal           seen           want       neighbor         action           part 
##     0.31622777     0.31622777     0.31622777     0.31622777     0.31622777     0.31622777     0.31622777     0.31426968     0.30860670     0.30151134     0.30151134     0.30151134     0.29814240 
##       influenc         remind           ride           solv           kind          local        heritag         summon        generos          ruler      forgotten            put           educ 
##     0.29814240     0.29814240     0.29814240     0.29814240     0.29814240     0.29814240     0.29814240     0.29814240     0.29814240     0.29814240     0.29814240     0.29095719     0.28867513 
##           told           citi        destroy            man           hand         member           rich          dream          chief            two          cross          assur           high 
##     0.28867513     0.28867513     0.28867513     0.28426762     0.28426762     0.27735010     0.27735010     0.27272727     0.27216553     0.27216553     0.27216553     0.27216553     0.27216553 
##         realiz            aim        potenti       mountain         lesson         bigger         welfar            vow        endless       movement           pain         allegi        compass 
##     0.27216553     0.27216553     0.27216553     0.27216553     0.27216553     0.27216553     0.27216553     0.27216553     0.27216553     0.27216553     0.27216553     0.27216553     0.26726124 
##           open           turn        everyon        factori         growth          start           made         higher         within           four           sinc          anyon          crime 
##     0.26726124     0.26490647     0.26148818     0.25819889     0.25819889     0.25819889     0.25308553     0.25197632     0.25197632     0.25197632     0.25197632     0.25197632     0.25197632 
##         mother         beyond       individu           look           back          price        support     commonplac         miracl        process          worst         borrow        concern 
##     0.25197632     0.25125945     0.23947374     0.23947374     0.23735633     0.23570226     0.23570226     0.23570226     0.23570226     0.23570226     0.23570226     0.23570226     0.23570226 
##       boundari         racial         object        bigotri         bounti         except         reserv          unwil        command         heroic         church       prioriti            fit 
##     0.23570226     0.23570226     0.23570226     0.23570226     0.23570226     0.23570226     0.23570226     0.23570226     0.23570226     0.23570226     0.23570226     0.23570226     0.23570226 
##       shoulder        abraham            add           paid         messag         martin         clergi           john         repres      horseback          untam              4        mankind 
##     0.23570226     0.23570226     0.23570226     0.23570226     0.23570226     0.23570226     0.23570226     0.23570226     0.23570226     0.23570226     0.23570226     0.23570226     0.23570226 
##         wherev           took     accomplish         master        servant          equip         belong         dramat           aliv         modern    self-govern       virginia         remark 
##     0.23570226     0.23570226     0.23570226     0.23570226     0.23570226     0.23570226     0.23570226     0.23570226     0.23570226     0.23570226     0.23570226     0.23570226     0.23570226 
##          wrote     difficulti      overwhelm           abus         disast        treasur           tool           debt       servitud           bill            due       unfortun         origin 
##     0.23570226     0.23570226     0.23570226     0.23570226     0.23570226     0.23570226     0.23570226     0.23570226     0.23570226     0.23570226     0.23570226     0.23570226     0.23570226 
##         custom        primari         awesom       research         target         nowher        noblest         hunger         mighti          youth         expans            lit         valley 
##     0.23570226     0.23570226     0.23570226     0.23570226     0.23570226     0.23570226     0.23570226     0.23570226     0.23570226     0.23570226     0.23570226     0.23570226     0.23570226 
##            air       idealist         affect         dictat          saint         deeper         second         chorus          motiv          apart          await         bicker          vigil 
##     0.23570226     0.23570226     0.23570226     0.23570226     0.23570226     0.23570226     0.23570226     0.23570226     0.23570226     0.23570226     0.23570226     0.23570226     0.23570226 
##      sentiment        connect       circumst         window           hurt         scourg           flaw        trumpet        mysteri       almighti         enrich        compani          infus 
##     0.23570226     0.23570226     0.23570226     0.23570226     0.23570226     0.23570226     0.23570226     0.23570226     0.23570226     0.23570226     0.23570226     0.23570226     0.23570226 
##           reap     millennium         affair         center           hire         behind           gang        airport        benefit           armi          plain         listen          anyth 
##     0.23570226     0.23570226     0.23570226     0.23570226     0.23570226     0.23570226     0.23570226     0.23570226     0.23570226     0.23570226     0.23570226     0.23570226     0.23570226 
##          arriv         carter        creator        highway          littl          spent        student        detroit         reason       industri       tomorrow          solut        loyalti 
##     0.23570226     0.23570226     0.23570226     0.23570226     0.23570226     0.23570226     0.23570226     0.23570226     0.23570226     0.22941573     0.22222222     0.22222222     0.22222222 
##            ten           drug       maintain        struggl           love           heal          among           past          first          share             go       reverend         almost 
##     0.22222222     0.22222222     0.21821789     0.21821789     0.21698152     0.21081851     0.20851441     0.20412415     0.19658927     0.19611614     0.19374606     0.19245009     0.19245009 
##         republ            goe         whatev        unleash         troubl         number           self           rest          ensur            tie          impos          aspir           star 
##     0.19245009     0.19245009     0.19245009     0.19245009     0.19245009     0.19245009     0.19245009     0.19245009     0.19245009     0.19245009     0.19245009     0.19245009     0.19245009 
##           issu            aid          yield         reborn           send           leav        conquer           wait           form          trade         honest          heard           mere 
##     0.19245009     0.19245009     0.19245009     0.19245009     0.19245009     0.19245009     0.19245009     0.19245009     0.19245009     0.19245009     0.19245009     0.19245009     0.19245009 
##          salut           flag          truli         provid       confront          sound          ahead         histor         beauti          georg          howev          close         econom 
##     0.19245009     0.19245009     0.19245009     0.18490007     0.17817416     0.17213259     0.16666667     0.16666667     0.16666667     0.16666667     0.16666667     0.16666667     0.16439899 
##        nuclear        increas          engag       guarante          grant        lincoln           hill     birthright           dare          drive          music           echo            200 
##     0.16222142     0.15713484     0.15713484     0.14907120     0.14907120     0.14907120     0.14907120     0.14907120     0.14907120     0.14907120     0.14907120     0.14907120     0.14907120 
##           door         execut          exist           fall         produc        reflect            win        rebuild           hall          spend         system           sick     friendship 
##     0.14907120     0.14907120     0.14907120     0.13608276     0.13608276     0.13608276     0.13608276     0.13608276     0.13608276     0.12830006     0.12598816     0.12598816     0.12598816 
##          moral          allow          becom         matter       thousand           hero        deficit        present        victori           page       transfer          offer        foreign 
##     0.12598816     0.12598816     0.11785113     0.11785113     0.11111111     0.10814761     0.10540926     0.10540926     0.10540926     0.10540926     0.10050378     0.10050378     0.10050378 
##             mr         weapon          reduc           step            tax         budget           left     washington           away          limit         prayer          think          senat 
##     0.09724333     0.09724333     0.09245003     0.09245003     0.08989331     0.08908708     0.08606630     0.08333333     0.07647191     0.07453560     0.07106691     0.06804138     0.00000000 
##       hatfield         mondal          baker        speaker         o'neil         moomaw          occas         occurr         routin           stop          uniqu         realli   every-4-year 
##     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000 
##         normal       gracious          degre        bulwark        afflict        proport        longest         inflat        distort          penal         thrift          crush    fixed-incom 
##     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000 
##          elder           alik        shatter            idl       unemploy         miseri         indign         burden           kept           pace           pile        mortgag      temporari 
##     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000 
##        conveni          trend        tremend         upheav         period  misunderstand          sever        bastion          tempt        complex       self-rul           elit          group 
##     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000 
##       superior         capabl         someon          equit            pay        special        neglect        section         ethnic           food           mine     profession  industrialist 
##     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000 
##       shopkeep          clerk          cabbi      truckdriv          breed      administr        healthi          vigor        barrier      discrimin        runaway          reviv           core 
##     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000 
##      inventori          check         revers          grown        consent         intent           curb          feder      establish       distinct        smother         foster          stifl 
##     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000 
##         genius         extent          avail        coincid       parallel     proportion      intervent         intrus         result    unnecessari         excess           loom        creativ 
##     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000 
##           gate        counter   entrepreneur      voluntari            art        address         makeup     countrymen        suffici           fact         theori      unequivoc         emphat 
##     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000 
##      paraphras        winston       churchil       dissolut         propos          remov      roadblock           slow        various          level           inch           feet           mile 
##     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000 
##       reawaken          giant            get        lighten          punit      compromis            eve             dr         joseph         warren   massachusett        despair         unborn 
##     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000 
##         worthi       exemplar         beacon       benefici    sovereignti           sale         negoti        surrend  misunderstood         reluct        misjudg        prevail        arsenal 
##     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000 
##         formid        practic           prey         deepli          front          vista         shrine       monument  revolutionari         infant     nationhood          eloqu           pool 
##     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000 
##         column        whoever        heroism        potomac          slope       cemeteri            row          white         marker          david           tini       fraction        belleau 
##     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000 
##           wood         argonn          omaha          beach        salerno        halfway      guadalcan         tarawa           pork           chop         chosin      reservoir          hundr 
##     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000 
##           rice          paddi          jungl        vietnam           town         barber           shop           1917          franc        rainbow        western           kill      battalion 
##     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000 
##          heavi      artilleri          diari        flyleaf           head        written       therefor          cheer         utmost        treptow        perform           deed         mathia 
##     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000 
##         burger           dole       brighter        presenc         absent         stenni          gilli      louisiana          night         silent           amen          adequ        express 
##     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000 
##           50th           bibl          stood         wilder             13             60             50           gone            cri           moon         stress          glori    present-day 
##     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000 
##       backward         proper         machin           1980        consist           rate         employ        vibrant         robust          climb   neighborhood         restat          freed 
##     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000 
##           grip         sincer        meaning         reduct        develop   totalitarian       sunlight           pois         golden           gain      two-parti     republican         boston 
##     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000 
##         lawyer           adam        planter          rival          elect           1800          later         soften          anger         letter    reestablish           1826    anniversari 
##     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000 
##         fourth           juli        exchang         sunset          beset        valuabl            oar       harmless           rode         repeat    well-intent          error          futil 
##     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000 
##          chase         spiral          bloat      prescript        reelect           1984         vindic             25       straight         incent  entrepreneuri       interfer       simplifi 
##     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000 
##          least        emancip           tear       distress           area      literatur         poetri          dynam       unbroken        brought         reckon          point        cabinet 
##     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000 
##          staff         submit          freez          desir          money    unconstitut          handl      fundament         upgrad         infirm    disadvantag          older         instal 
##     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000 
##        hearten    brotherhood          hesit          abund          black          utter        fervent          scorn         soviet       militari        buildup         offens        legitim 
##     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000 
##        discuss          total         elimin         either         resort         retali          logic        recours         approv         shield        militar          space      demilitar 
##     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000 
##         render        obsolet            rid       fourfold      hemispher       worldwid  self-determin        inalien     staunchest           blow        inflict         lightn      transcend 
##     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000 
##         ribbon         unfurl         symbol         gotten          insid        general           knee           lone         darken         ponder          alamo        settler           push 
##     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000 
##           sing           song         unknow      big-heart         tender          quayl        mitchel         wright    congressman        michael         reagan     bicentenni        gladden 
##     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000 
##        concret           stun          porch           talk        suspend            bow           heed          write           lord          breez        refresh          blown       lifeless 
##     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000 
##           tree          thick            fog            sit           mist           room           agit        thought     intellectu      satisfact         speech         unhamp         perhap 
##     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000 
##           late          wrest        crucial          proud           loud        enthral         materi        appreci          nobil           bank          loyal         driven           stay 
##     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000 
##        quieter           gold           silk          finer         wholli         unless         kinder        gentler       homeless           roam       normalci         enslav         addict 
##     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000 
##          demor           slum          rough        guidanc           case           fund            low         wallet          alloc         wisest          activ       hands-on         involv 
##     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000 
##           unus         talent        unfocus     leadership    stewardship          organ           hous          agenc          pitch         thrash         fiscal        dissens        harmoni 
##     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000 
##      statement        untrust          cleav        earnest        quarter         statut         sunder bipartisanship        opposit          major          clock           wish       partisan 
##     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000 
##      unaccount         assist          beget         treati      agreement          marbl         candor     compliment       subtleti   relationship         experi          throw            son 
##     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000 
##           hymn      continuum        inescap           kite        neither          princ           pope          yearn          going        attitud        intoler        obvious         cocain 
##     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000 
##         smuggl           ship           dead       bacteria       mistrust           larg      boundless          drama           book         robert          obama           ladi        michell 
##     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000 
##             dc     politician        januari             20           2017      righteous           trap          inner     rusted-out        scatter       tombston       landscap          flush 
##     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000 
##           cash         depriv         stolen         unreal         carnag       glorious         expens         subsid            sad         deplet       trillion        oversea  infrastructur 
##     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000 
##      disrepair          decay         dissip            rip    redistribut        assembl          decre          ravag          steal         breath         tunnel        railway            buy 
##     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000 
##        goodwil          shine       reinforc          radic          islam           erad        bedrock       rediscov       pleasant         disagr        solidar        unstopp         enforc 
##     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000 
##       complain          empti         unlock         diseas           stir          brown          bleed            red          urban         sprawl      windswept       nebraska            sky 
##     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000 
##        wealthi 
##     0.00000000 
## 
## $terror
##        potenti      adversari     commonplac         miracl         racial         bounti         martin          dream          polit       guarante           solv          grate           open 
##     0.90369611     0.90369611     0.89442719     0.89442719     0.89442719     0.89442719     0.89442719     0.86243936     0.85000000     0.84852814     0.84852814     0.84852814     0.84515425 
##          solut          whose         cultur       maintain           upon           educ           told        factori        product       industri          match           mall           land 
##     0.84327404     0.83914639     0.83666003     0.82807867     0.82530726     0.82158384     0.82158384     0.81649658     0.80622577     0.79802388     0.79056942     0.79056942     0.78740079 
##     strengthen            far          cross         realiz             go       children         answer        problem          decad        loyalti       strength            eye         street 
##     0.78262379     0.78262379     0.77459667     0.77459667     0.77197535     0.76674848     0.76277007     0.75925660     0.73786479     0.73786479     0.73156352     0.73029674     0.73029674 
##           grow        unleash            tie        highest       magnific          flame        shutter       greatest           will         enough         within         fortun     understood 
##     0.73029674     0.73029674     0.73029674     0.73029674     0.73029674     0.73029674     0.73029674     0.73029674     0.72026431     0.71818485     0.71713717     0.71713717     0.71713717 
##            yes          offic          bridg            let           full      administr         remind           kind        sustain          labor            end            big        special 
##     0.71713717     0.71713717     0.71713717     0.71497001     0.70710678     0.70710678     0.70710678     0.70710678     0.70164642     0.70164642     0.70064905     0.70000000     0.69026849 
##          chanc             us         govern           face            new       opportun           noth           born           gift           back          group          price       gracious 
##     0.69026849     0.68708450     0.68586469     0.68224229     0.67806766     0.67729621     0.67612340     0.67612340     0.67612340     0.67552796     0.67082039     0.67082039     0.67082039 
##         miseri           week         patrol       recognit            era         balanc          decid          readi        forbear          humil        dignifi          river      arlington 
##     0.67082039     0.67082039     0.67082039     0.67082039     0.67082039     0.67082039     0.67082039     0.67082039     0.67082039     0.67082039     0.67082039     0.67082039     0.67082039 
##        earlier            lie           fame        convict     millennium         affair         center        smaller           hire           roll         behind           gang           fuel 
##     0.67082039     0.67082039     0.67082039     0.67082039     0.67082039     0.67082039     0.67082039     0.67082039     0.67082039     0.67082039     0.67082039     0.67082039     0.67082039 
##      everywher          stain         legaci        airport           farm        benefit           armi          plain         nation          class           hero         wealth          pledg 
##     0.67082039     0.67082039     0.67082039     0.67082039     0.67082039     0.67082039     0.67082039     0.67082039     0.66975048     0.66742381     0.66688593     0.65938047     0.65292863 
##          shall           come           live           last          small          assur           fate         welfar    citizenship           read       precious         fellow        centuri 
##     0.65292863     0.65169462     0.64782106     0.64594224     0.64594224     0.64549722     0.64549722     0.64549722     0.64549722     0.64549722     0.64549722     0.64317588     0.63976740 
##         around          never         promis          futur         achiev           give       american       hatfield         mondal          baker         moomaw         occurr         routin 
##     0.63900965     0.63887656     0.63814490     0.63745526     0.63639610     0.63433505     0.63269968     0.63245553     0.63245553     0.63245553     0.63245553     0.63245553     0.63245553 
##          uniqu         realli   every-4-year         normal        transit          degre        bulwark        afflict        proport        longest        distort          penal         thrift 
##     0.63245553     0.63245553     0.63245553     0.63245553     0.63245553     0.63245553     0.63245553     0.63245553     0.63245553     0.63245553     0.63245553     0.63245553     0.63245553 
##          crush    fixed-incom           alik        shatter            idl         indign           deni         burden           kept           pile        mortgag      temporari        conveni 
##     0.63245553     0.63245553     0.63245553     0.63245553     0.63245553     0.63245553     0.63245553     0.63245553     0.63245553     0.63245553     0.63245553     0.63245553     0.63245553 
##          trend        tremend         upheav         period  misunderstand          sever        bastion          tempt        complex       self-rul           elit       superior         someon 
##     0.63245553     0.63245553     0.63245553     0.63245553     0.63245553     0.63245553     0.63245553     0.63245553     0.63245553     0.63245553     0.63245553     0.63245553     0.63245553 
##          equit        neglect        section           food           mine          teach     profession  industrialist       shopkeep          clerk          cabbi      truckdriv          breed 
##     0.63245553     0.63245553     0.63245553     0.63245553     0.63245553     0.63245553     0.63245553     0.63245553     0.63245553     0.63245553     0.63245553     0.63245553     0.63245553 
##        healthi          vigor      discrimin        runaway          reviv           play      inventori          check        consent         intent           curb         demand       distinct 
##     0.63245553     0.63245553     0.63245553     0.63245553     0.63245553     0.63245553     0.63245553     0.63245553     0.63245553     0.63245553     0.63245553     0.63245553     0.63245553 
##        smother         foster          stifl         energi         extent          avail        coincid       parallel     proportion      intervent         intrus         result    unnecessari 
##     0.63245553     0.63245553     0.63245553     0.63245553     0.63245553     0.63245553     0.63245553     0.63245553     0.63245553     0.63245553     0.63245553     0.63245553     0.63245553 
##         excess           loom        creativ           gate        counter   entrepreneur      voluntari            art        address         makeup     countrymen        suffici         theori 
##     0.63245553     0.63245553     0.63245553     0.63245553     0.63245553     0.63245553     0.63245553     0.63245553     0.63245553     0.63245553     0.63245553     0.63245553     0.63245553 
##      unequivoc         emphat      paraphras        winston       churchil       dissolut      strongest          ahead         propos          remov      roadblock        various          level 
##     0.63245553     0.63245553     0.63245553     0.63245553     0.63245553     0.63245553     0.63245553     0.63245553     0.63245553     0.63245553     0.63245553     0.63245553     0.63245553 
##           inch           feet           mile       reawaken          giant        lighten          punit            eve             dr         joseph         warren   massachusett        despair 
##     0.63245553     0.63245553     0.63245553     0.63245553     0.63245553     0.63245553     0.63245553     0.63245553     0.63245553     0.63245553     0.63245553     0.63245553     0.63245553 
##       exemplar         beacon       benefici    sovereignti           sale        surrend  misunderstood        misjudg        prevail         formid        practic           prey       thousand 
##     0.63245553     0.63245553     0.63245553     0.63245553     0.63245553     0.63245553     0.63245553     0.63245553     0.63245553     0.63245553     0.63245553     0.63245553     0.63245553 
##         deepli          vista         shrine       monument  revolutionari         infant     nationhood          eloqu           pool         column        whoever        heroism        potomac 
##     0.63245553     0.63245553     0.63245553     0.63245553     0.63245553     0.63245553     0.63245553     0.63245553     0.63245553     0.63245553     0.63245553     0.63245553     0.63245553 
##          shore          slope       cemeteri            row         marker          david           tini       fraction          spoke        belleau           wood         argonn          omaha 
##     0.63245553     0.63245553     0.63245553     0.63245553     0.63245553     0.63245553     0.63245553     0.63245553     0.63245553     0.63245553     0.63245553     0.63245553     0.63245553 
##          beach        salerno        halfway      guadalcan         tarawa           pork           chop         chosin      reservoir          hundr           rice          paddi          jungl 
##     0.63245553     0.63245553     0.63245553     0.63245553     0.63245553     0.63245553     0.63245553     0.63245553     0.63245553     0.63245553     0.63245553     0.63245553     0.63245553 
##         barber           shop           1917          franc        rainbow        western            tri      battalion          heavi      artilleri           bodi          diari        flyleaf 
##     0.63245553     0.63245553     0.63245553     0.63245553     0.63245553     0.63245553     0.63245553     0.63245553     0.63245553     0.63245553     0.63245553     0.63245553     0.63245553 
##       therefor          cheer        treptow        perform           deed          sight           20th       prospect           18th           19th        abolish             aw        turmoil 
##     0.63245553     0.63245553     0.63245553     0.63245553     0.63245553     0.63245553     0.63245553     0.63245553     0.63245553     0.63245553     0.63245553     0.63245553     0.63245553 
##         explod           onto          stage      mightiest          unriv          split           atom         explor         comput      microchip         deepen        wellspr        african 
##     0.63245553     0.63245553     0.63245553     0.63245553     0.63245553     0.63245553     0.63245553     0.63245553     0.63245553     0.63245553     0.63245553     0.63245553     0.63245553 
##          circl          third          coast        conserv         inform        perfect        tragedi        exhilar      indispens        cleaner         destin           bend          safer 
##     0.63245553     0.63245553     0.63245553     0.63245553     0.63245553     0.63245553     0.63245553     0.63245553     0.63245553     0.63245553     0.63245553     0.63245553     0.63245553 
##         record        flexibl       everyday        preemin           lock          divid           curs       contempt          cloak         religi        fanatic        torment         obsess 
##     0.63245553     0.63245553     0.63245553     0.63245553     0.63245553     0.63245553     0.63245553     0.63245553     0.63245553     0.63245553     0.63245553     0.63245553     0.63245553 
##           hate         impuls           lurk        overcom         textur        godsend       approach         outlin       internet         mystic        provinc      physicist   encyclopedia 
##     0.63245553     0.63245553     0.63245553     0.63245553     0.63245553     0.63245553     0.63245553     0.63245553     0.63245553     0.63245553     0.63245553     0.63245553     0.63245553 
## schoolchildren      scientist          decod      blueprint         hostil           camp   dictatorship        surpass        bloodsh        resound         sought          prize          ignit 
##     0.63245553     0.63245553     0.63245553     0.63245553     0.63245553     0.63245553     0.63245553     0.63245553     0.63245553     0.63245553     0.63245553     0.63245553     0.63245553 
##          spark            boy      classroom        librari        kitchen           tabl       laughter          shoot           sell         anymor        medicin       hardwork         chemic 
##     0.63245553     0.63245553     0.63245553     0.63245553     0.63245553     0.63245553     0.63245553     0.63245553     0.63245553     0.63245553     0.63245553     0.63245553     0.63245553 
##         biolog           port          innov       grandpar  grandchildren        fortifi         majest         louder            din         regain    thirty-four        prophet         luther 
##     0.63245553     0.63245553     0.63245553     0.63245553     0.63245553     0.63245553     0.63245553     0.63245553     0.63245553     0.63245553     0.63245553     0.63245553     0.63245553 
##      ceaseless         redeem         extrem   partisanship         deplor         repair         breach         cardin      bernardin           wast       acrimoni           wide          belov 
##     0.63245553     0.63245553     0.63245553     0.63245553     0.63245553     0.63245553     0.63245553     0.63245553     0.63245553     0.63245553     0.63245553     0.63245553     0.63245553 
##         height         summit            job       individu           meet            one         spirit           ever            now           side     throughout          everi         worker 
##     0.63245553     0.63245553     0.62828086     0.62475802     0.62385029     0.62362525     0.62279916     0.62105900     0.61665481     0.61394061     0.61394061     0.60583048     0.60000000 
##            ill          parti           save         extend          order        present          front         spread           21st        destini         return        prosper           said 
##     0.60000000     0.60000000     0.60000000     0.60000000     0.60000000     0.60000000     0.60000000     0.60000000     0.60000000     0.59761430     0.59761430     0.59761430     0.59761430 
##           held           take           home         famili       confront          capac          cours       interest          build          today         across          power          stand 
##     0.59761430     0.59655876     0.59344243     0.59263776     0.59160798     0.59160798     0.59160798     0.58797473     0.58728014     0.58660133     0.58639547     0.58609427     0.58423739 
##          reach           away           even           long        million        economi          peopl           time            man          great         beyond        patriot       neighbor 
##     0.58094750     0.58038100     0.58038100     0.57659998     0.57564968     0.57522374     0.57456729     0.57319636     0.57306988     0.57270092     0.57207755     0.57207755     0.57207755 
##          shape           work          bless         cooper            pay          grant           slow         depend         worthi         reluct       conflict        inaugur            led 
##     0.57207755     0.57008771     0.56684343     0.56568542     0.56568542     0.56568542     0.56568542     0.56568542     0.56568542     0.56568542     0.56568542     0.56568542     0.56568542 
##        lincoln           hill        willing            har          quest         bright         decenc            can           less        greater          becom           join       prejudic 
##     0.56568542     0.56568542     0.56568542     0.56568542     0.56568542     0.56568542     0.56568542     0.56336227     0.56293997     0.55901699     0.55901699     0.55901699     0.55901699 
##          world        struggl            put        collect          manag      establish         inevit         declin          might           firm          impos         intend           citi 
##     0.55588058     0.55205245     0.55205245     0.54772256     0.54772256     0.54772256     0.54772256     0.54772256     0.54772256     0.54772256     0.54772256     0.54772256     0.54772256 
##           issu     presidenti           next           forg           form          trade        chapter         forget          globe           role           near           plan         colleg 
##     0.54772256     0.54772256     0.54772256     0.54772256     0.54772256     0.54772256     0.54772256     0.54772256     0.54772256     0.54772256     0.54772256     0.54772256     0.54772256 
##           lose         narrow          petti         wisdom        realiti         rather         vision            day         believ          crisi            may           just        america 
##     0.54772256     0.54772256     0.54772256     0.54772256     0.54772256     0.54772256     0.54772256     0.54548237     0.54041926     0.53935989     0.53520153     0.53367605     0.53171961 
##          among         suffer       tomorrow         import         strive            ten           drug       knowledg          middl         thrive           much         provid           alon 
##     0.52750438     0.52704628     0.52704628     0.52704628     0.52704628     0.52704628     0.52704628     0.52704628     0.52704628     0.52704628     0.52623481     0.52623481     0.52174919 
##        preserv         togeth           bush          decis          month           line          short           fall         produc        reflect            get         mutual           west 
##     0.51987524     0.51698426     0.51639778     0.51639778     0.51639778     0.51639778     0.51639778     0.51639778     0.51639778     0.51639778     0.51639778     0.51639778     0.51639778 
##            win       stronger        certain          water          built          humbl          women          creat           look           mani     understand        success         longer 
##     0.51639778     0.51639778     0.51639778     0.51639778     0.51639778     0.51639778     0.51639778     0.51116565     0.51116565     0.50783338     0.50783338     0.50709255     0.50709255 
##         gather         effort            age         presid       ceremoni           heal         restor         memori          child          choos           fear           real            yet 
##     0.50709255     0.50596443     0.50311529     0.50156986     0.50000000     0.50000000     0.50000000     0.50000000     0.50000000     0.50000000     0.50000000     0.50000000     0.49827288 
##           life          share          divis         strong          earth          begin           well           keep           make            use         higher           sick           feed 
##     0.49724516     0.49613894     0.49497475     0.49490154     0.49490154     0.49403218     0.49386480     0.48989795     0.48702462     0.48686450     0.47809144     0.47809144     0.47809144 
##          whole           sens            edg         planet         system          faith         moment       transfer         action          began           lift            set          state 
##     0.47809144     0.47809144     0.47809144     0.47809144     0.47809144     0.47780947     0.47673129     0.47673129     0.47673129     0.47673129     0.47673129     0.47673129     0.47464445 
##          carri           size        chariti         histor         beauti          white          howev         invent            rob          retir        alreadi          close         parent 
##     0.47434165     0.47434165     0.47434165     0.47434165     0.47434165     0.47434165     0.47434165     0.47434165     0.47434165     0.47434165     0.47434165     0.47434165     0.47434165 
##           sure           bind        commerc          along            god       challeng          anoth           must           valu          first          happi           cost        instead 
##     0.47434165     0.47434165     0.47434165     0.47434165     0.47404546     0.47193990     0.46977618     0.46889738     0.46788772     0.46625240     0.46156633     0.45643546     0.45643546 
##        citizen            see            old            way          think            two           past         school         differ          taken        support         o'neil          occas 
##     0.45508491     0.45436947     0.45397969     0.45286283     0.45184806     0.45184806     0.45184806     0.45184806     0.45175395     0.44721360     0.44721360     0.44721360     0.44721360 
##        process           busi          worst         inflat          elder       unemploy           pace         borrow           bear        concern       boundari         ethnic         object 
##     0.44721360     0.44721360     0.44721360     0.44721360     0.44721360     0.44721360     0.44721360     0.44721360     0.44721360     0.44721360     0.44721360     0.44721360     0.44721360 
##        barrier        bigotri           core         except         revers         reserv         genius          unwil        command         heroic         church       prioriti      compromis 
##     0.44721360     0.44721360     0.44721360     0.44721360     0.44721360     0.44721360     0.44721360     0.44721360     0.44721360     0.44721360     0.44721360     0.44721360     0.44721360 
##         unborn        arsenal            fit       shoulder        abraham            add           paid           town         messag        written         utmost           aliv         remark 
##     0.44721360     0.44721360     0.44721360     0.44721360     0.44721360     0.44721360     0.44721360     0.44721360     0.44721360     0.44721360     0.44721360     0.44721360     0.44721360 
##           tool         target        noblest         mighti            air          apart          await         bicker        connect         window         scourg          plagu        fractur 
##     0.44721360     0.44721360     0.44721360     0.44721360     0.44721360     0.44721360     0.44721360     0.44721360     0.44721360     0.44721360     0.44721360     0.44721360     0.44721360 
##          sleep           dawn        environ        slaveri          minor      limitless          touch         taught        reclaim        pretens         crippl        succumb         region 
##     0.44721360     0.44721360     0.44721360     0.44721360     0.44721360     0.44721360     0.44721360     0.44721360     0.44721360     0.44721360     0.44721360     0.44721360     0.44721360 
##          broad           girl          natur       particip        patienc         dollar       flourish         fallen          refus        horizon        forward          alway          speak 
##     0.44721360     0.44721360     0.44721360     0.44721360     0.44721360     0.44721360     0.44721360     0.44721360     0.44721360     0.44721360     0.44447832     0.44447832     0.44271887 
##          heart         father            war           seek         deserv        revolut          capit           call       sacrific          young        whether          shown          limit 
##     0.44232587     0.44038551     0.44038551     0.43915503     0.43852901     0.43852901     0.43852901     0.43412157     0.43412157     0.43386092     0.42600643     0.42426407     0.42426407 
##         failur           race        possess       timeless          wrong      forgotten           done         health           vice           stop         measur         common        succeed 
##     0.42426407     0.42426407     0.42426407     0.42426407     0.42426407     0.42426407     0.42257713     0.42163702     0.42163702     0.42163702     0.42163702     0.42163702     0.42163702 
##          forth        founder        mission         econom          endur        protect           mean         declar           year            say           left           fair         growth 
##     0.42163702     0.42163702     0.42163702     0.41590020     0.41590020     0.41576092     0.41443422     0.41403934     0.41286141     0.41079192     0.40824829     0.40824829     0.40824829 
##          place           seem        respons        deficit         danger        victori          fight          learn           true       progress            men         advanc          thank 
##     0.40683810     0.40451992     0.40224091     0.40000000     0.40000000     0.40000000     0.40000000     0.40000000     0.40000000     0.39852670     0.39840954     0.39562828     0.39467611 
##        histori         courag       question          feder          chief           high            aim          relat       mountain           move          ocean          becam       constant 
##     0.39312270     0.39223227     0.39131190     0.39036003     0.38729833     0.38729833     0.38729833     0.38729833     0.38729833     0.38729833     0.38729833     0.38729833     0.38729833 
##        journey           know            tax         follow         defens          choic        triumph         public          union         better           oath          defin       congress 
##     0.38413666     0.38399086     0.38376129     0.38138504     0.38138504     0.38138504     0.38138504     0.37947332     0.37712362     0.37529331     0.37416574     0.37267800     0.37210420 
##        everyon           hand           like         weapon           hope       reverend      constitut         almost         republ            goe           cast         whatev         capabl 
##     0.37210420     0.37080992     0.37052414     0.36901248     0.36706517     0.36514837     0.36514837     0.36514837     0.36514837     0.36514837     0.36514837     0.36514837     0.36514837 
##           sign          grown         troubl         number           self           rest          ensur          aspir         negoti          thoma           star          enjoy           bond 
##     0.36514837     0.36514837     0.36514837     0.36514837     0.36514837     0.36514837     0.36514837     0.36514837     0.36514837     0.36514837     0.36514837     0.36514837     0.36514837 
##        destroy           cold         heaven          assum         global           cure         contin           gave         replac          avoid          claim         exampl          pursu 
##     0.36514837     0.36514837     0.36514837     0.36514837     0.36514837     0.36514837     0.36514837     0.36514837     0.36514837     0.36514837     0.36514837     0.36514837     0.36514837 
##       hardship          equal         celebr         toward           word           free           help     friendship           find           four           dark         reward        control 
##     0.36514837     0.36439935     0.36391267     0.36273813     0.36070421     0.36063528     0.36037499     0.35856858     0.35856858     0.35856858     0.35856858     0.35856858     0.35856858 
##          crime           rise           tell         immigr         reform         remain           hour           rich           need      democraci          watch           made          renew 
##     0.35856858     0.35856858     0.35856858     0.35856858     0.35355339     0.35233213     0.35082321     0.35082321     0.34785054     0.34503278     0.34503278     0.34299717     0.34099717 
##           came         rememb          trust         threat         border           forc          human         matter          quiet          simpl           road            act           good 
##     0.34077710     0.34077710     0.33806170     0.33806170     0.33806170     0.33756998     0.33678304     0.33541020     0.33541020     0.33541020     0.33541020     0.33471934     0.33282012 
##        countri          found           part           guid           care           unit         solemn         person          bound           hear          georg     washington         wonder 
##     0.33241125     0.33028913     0.32998316     0.32659863     0.31950483     0.31782086     0.31622777     0.31622777     0.31622777     0.31622777     0.31622777     0.31622777     0.31622777 
##          march          forev       ancestor     forty-four         amidst          cloud         simpli          midst      far-reach        consequ          greed      irrespons           shed 
##     0.31622777     0.31622777     0.31622777     0.31622777     0.31622777     0.31622777     0.31622777     0.31622777     0.31622777     0.31622777     0.31622777     0.31622777     0.31622777 
##          indic           data        statist            nag          lower         easili           span       grievanc           fals       recrimin       worn-out          dogma        strangl 
##     0.31622777     0.31622777     0.31622777     0.31622777     0.31622777     0.31622777     0.31622777     0.31622777     0.31622777     0.31622777     0.31622777     0.31622777     0.31622777 
##       childish           nobl      god-given       shortcut    faint-heart         leisur        pleasur       risk-tak           doer     things'som         obscur            rug           pack 
##     0.31622777     0.31622777     0.31622777     0.31622777     0.31622777     0.31622777     0.31622777     0.31622777     0.31622777     0.31622777     0.31622777     0.31622777     0.31622777 
##      sweatshop           whip           plow         fought        concord     gettysburg       normandi            khe           sahn         sacrif           till     undiminish            pat 
##     0.31622777     0.31622777     0.31622777     0.31622777     0.31622777     0.31622777     0.31622777     0.31622777     0.31622777     0.31622777     0.31622777     0.31622777     0.31622777 
##        unpleas           pick           dust            lay         electr           grid          digit          wield            sun      transform          scale        suggest         necess 
##     0.31622777     0.31622777     0.31622777     0.31622777     0.31622777     0.31622777     0.31622777     0.31622777     0.31622777     0.31622777     0.31622777     0.31622777     0.31622777 
##          shift        beneath          stale       argument         consum          appli           wise         expand           spin          gross           abil           rout          peril 
##     0.31622777     0.31622777     0.31622777     0.31622777     0.31622777     0.31622777     0.31622777     0.31622777     0.31622777     0.31622777     0.31622777     0.31622777     0.31622777 
##          scarc          draft        charter         expedi         villag           tank         sturdi         entitl          pleas           eman      restraint         keeper           iraq 
##     0.31622777     0.31622777     0.31622777     0.31622777     0.31622777     0.31622777     0.31622777     0.31622777     0.31622777     0.31622777     0.31622777     0.31622777     0.31622777 
##      hard-earn    afghanistan         former            foe       tireless         lessen        specter         apolog          induc      slaughter          innoc        outlast      patchwork 
##     0.31622777     0.31622777     0.31622777     0.31622777     0.31622777     0.31622777     0.31622777     0.31622777     0.31622777     0.31622777     0.31622777     0.31622777     0.31622777 
##      christian         muslim            jew         hindus     non-believ        languag           tast         bitter          swill         segreg        someday          tribe        dissolv 
##     0.31622777     0.31622777     0.31622777     0.31622777     0.31622777     0.31622777     0.31622777     0.31622777     0.31622777     0.31622777     0.31622777     0.31622777     0.31622777 
##          usher            sow          blame          cling        corrupt         deceit         silenc       unclench       alongsid          clean        nourish          starv         plenti 
##     0.31622777     0.31622777     0.31622777     0.31622777     0.31622777     0.31622777     0.31622777     0.31622777     0.31622777     0.31622777     0.31622777     0.31622777     0.31622777 
##       indiffer         outsid         regard       gratitud        far-off         desert        whisper       guardian         embodi           leve       selfless      firefight       stairway 
##     0.31622777     0.31622777     0.31622777     0.31622777     0.31622777     0.31622777     0.31622777     0.31622777     0.31622777     0.31622777     0.31622777     0.31622777     0.31622777 
##          smoke         nurtur     instrument        honesti         curios           glad        satisfi          sixti        restaur        remembr        coldest           band          huddl 
##     0.31622777     0.31622777     0.31622777     0.31622777     0.31622777     0.31622777     0.31622777     0.31622777     0.31622777     0.31622777     0.31622777     0.31622777     0.31622777 
##        campfir            ici         outcom          virtu          alarm        current         falter            fix          deliv         robert          obama           ladi        michell 
##     0.31622777     0.31622777     0.31622777     0.31622777     0.31622777     0.31622777     0.31622777     0.31622777     0.31622777     0.31622777     0.31622777     0.31622777     0.31622777 
##             dc     politician        januari             20           2017      righteous           trap          inner     rusted-out        scatter       tombston       landscap          flush 
##     0.31622777     0.31622777     0.31622777     0.31622777     0.31622777     0.31622777     0.31622777     0.31622777     0.31622777     0.31622777     0.31622777     0.31622777     0.31622777 
##           cash         depriv         stolen         unreal         carnag       glorious         expens         subsid            sad         deplet       trillion        oversea  infrastructur 
##     0.31622777     0.31622777     0.31622777     0.31622777     0.31622777     0.31622777     0.31622777     0.31622777     0.31622777     0.31622777     0.31622777     0.31622777     0.31622777 
##      disrepair          decay         dissip            rip    redistribut        assembl          decre          ravag          steal         breath         tunnel        railway            buy 
##     0.31622777     0.31622777     0.31622777     0.31622777     0.31622777     0.31622777     0.31622777     0.31622777     0.31622777     0.31622777     0.31622777     0.31622777     0.31622777 
##        goodwil          shine       reinforc          radic          islam           erad        bedrock       rediscov       pleasant         disagr        solidar        unstopp         enforc 
##     0.31622777     0.31622777     0.31622777     0.31622777     0.31622777     0.31622777     0.31622777     0.31622777     0.31622777     0.31622777     0.31622777     0.31622777     0.31622777 
##       complain          empti         unlock         diseas           stir          brown          bleed            red          urban         sprawl      windswept       nebraska            sky 
##     0.31622777     0.31622777     0.31622777     0.31622777     0.31622777     0.31622777     0.31622777     0.31622777     0.31622777     0.31622777     0.31622777     0.31622777     0.31622777 
##        wealthi          other      communiti            ago            law        societi          right           goal           peac        continu       determin           love           want 
##     0.31622777     0.31192515     0.31192515     0.31008684     0.30678600     0.30429031     0.30391843     0.29814240     0.29504298     0.29361011     0.29019050     0.28818544     0.28603878 
##           fail          blood        foreign       influenc           ride       independ        vietnam           head           serv           dare           mind       militari          total 
##     0.28603878     0.28603878     0.28603878     0.28284271     0.28284271     0.28284271     0.28284271     0.28284271     0.28284271     0.28284271     0.28284271     0.28284271     0.28284271 
##        allianc        possibl           echo           door         winter         imagin       generous       standard          treat          exist       reaffirm          settl        qualiti 
##     0.28284271     0.28284271     0.28284271     0.28284271     0.28284271     0.28284271     0.28284271     0.28284271     0.28284271     0.28284271     0.28284271     0.28284271     0.28284271 
##             mr         accept           task           bold          debat            ask         prayer           step           safe           voic            els        rebuild            met 
##     0.27675936     0.27602622     0.27602622     0.27386128     0.27386128     0.27116307     0.26967994     0.26311741     0.26311741     0.26311741     0.25819889     0.25819889     0.25819889 
##        ancient           king         bigger          often            vow          ambit       movement            bad           pain          brave          oblig         allegi          enemi 
##     0.25819889     0.25819889     0.25819889     0.25819889     0.25819889     0.25819889     0.25819889     0.25819889     0.25819889     0.25819889     0.25819889     0.25819889     0.25354628 
##        generat           lead          secur         resolv         requir       threaten          start          birth          bring         tradit          moral           wave       destruct 
##     0.25302404     0.25298221     0.25226249     0.24806947     0.24748737     0.24494897     0.24494897     0.24494897     0.24112141     0.23904572     0.23904572     0.23904572     0.23904572 
##         divers          anyon         afford           feel         embrac      necessari        digniti          senat        liberti         confid        freedom          storm         social 
##     0.23904572     0.23904572     0.23904572     0.23904572     0.23904572     0.23904572     0.23570226     0.23488809     0.23051363     0.23008950     0.22657647     0.22360680     0.22360680 
##           alli         bestow            raw         belong          final      enterpris           tide           name         hungri          black          waver         missil          space 
##     0.22360680     0.22360680     0.22360680     0.22360680     0.22360680     0.22360680     0.22360680     0.22360680     0.22360680     0.22360680     0.22360680     0.22360680     0.22360680 
##           snow           talk         ground         broken         reveal           room            sum            car        account        prudent        discord           soon           fist 
##     0.22360680     0.22360680     0.22360680     0.22360680     0.22360680     0.22360680     0.22360680     0.22360680     0.22360680     0.22360680     0.22360680     0.22360680     0.22360680 
##         effect           larg        mysteri          depth       almighti         weaken        univers       profound         enrich        foundat            saw           sacr           toil 
##     0.22360680     0.22360680     0.22360680     0.22360680     0.22360680     0.22360680     0.22360680     0.22360680     0.22360680     0.22360680     0.22360680     0.22360680     0.22360680 
##           asid      conscienc        compani          infus       scriptur           reap          grace       grandest           wind        serious           soil        respect          cynic 
##     0.22360680     0.22360680     0.22360680     0.22360680     0.22360680     0.22360680     0.22360680     0.22360680     0.22360680     0.22360680     0.22360680     0.22360680     0.22360680 
##          favor       stranger         listen         search          anyth        subject          swift          arriv         carter           mark        distant        violenc          woman 
##     0.22360680     0.22360680     0.22360680     0.22360680     0.22360680     0.22360680     0.22360680     0.22360680     0.22360680     0.22360680     0.22360680     0.22360680     0.22360680 
##          maker          emerg         prefer          grudg        dissent         defeat        darkest         prepar           judg         consid           flow        inhabit       document 
##     0.22360680     0.22360680     0.22360680     0.22360680     0.22360680     0.22360680     0.22360680     0.22360680     0.22360680     0.22360680     0.22360680     0.22360680     0.22360680 
##        network            sap           lash        faction        unmatch         surest         reject         precis      uncertain        creator        highway          littl          spent 
##     0.22360680     0.22360680     0.22360680     0.22360680     0.22360680     0.22360680     0.22360680     0.22360680     0.22360680     0.22360680     0.22360680     0.22360680     0.22360680 
##        student        detroit         reason           rais           idea           show            die           fill           path         market         justic      jefferson           kill 
##     0.22360680     0.22360680     0.22360680     0.21764288     0.21718612     0.21081851     0.21081851     0.21081851     0.21081851     0.21081851     0.20942695     0.20000000     0.20000000 
##           hatr         someth          toler        without           best        speaker        capitol         direct          light      technolog         perman         travel          clear 
##     0.20000000     0.20000000     0.20000000     0.20000000     0.19518001     0.19069252     0.19069252     0.19069252     0.19069252     0.19069252     0.19069252     0.19069252     0.19069252 
##         author           deep          night            aid           knew            cut   neighborhood           leav         spoken         domest           base         honest        resourc 
##     0.18257419     0.18257419     0.18257419     0.18257419     0.18257419     0.18257419     0.18257419     0.18257419     0.18257419     0.18257419     0.18257419     0.18257419     0.18257419 
##          heard           mere          salut           flag          truli         unfold        fascism           wage          remak          break         temper           rage          ignor 
##     0.18257419     0.18257419     0.18257419     0.18257419     0.18257419     0.18257419     0.18257419     0.18257419     0.18257419     0.18257419     0.18257419     0.18257419     0.18257419 
##           weak          recal         surviv           evid          spend          civil           hard          creed         member          chang        compass         budget           pass 
##     0.18257419     0.18257419     0.18257419     0.18257419     0.18257419     0.18070158     0.17928429     0.17616607     0.17541160     0.17500000     0.16903085     0.16903085     0.16269784 
##       principl        charact          singl          given          ultim           warm           poor         safeti          doubt           lost      difficult         servic          vital 
##     0.16035675     0.16035675     0.15811388     0.15811388     0.15811388     0.15811388     0.15811388     0.15811388     0.15811388     0.15811388     0.15811388     0.15811388     0.15811388 
##          still        nuclear           fire          uniti           seen           also          ideal          thing          local        heritag        thought        generos          proud 
##     0.15453348     0.15389675     0.14907120     0.14907120     0.14638501     0.14509525     0.14433757     0.14314958     0.14142136     0.14142136     0.14142136     0.14142136     0.14142136 
##          ruler          skill         scienc          drawn           rule           soul           caus           bibl           hall          habit           seiz        clinton            run 
##     0.14142136     0.14142136     0.14142136     0.14142136     0.13801311     0.13483997     0.13416408     0.12909944     0.12909944     0.12909944     0.12909944     0.12909944     0.12909944 
##        abandon          sourc           test          pride           fact        tyranni          allow         mother      communism         purpos         commit         decent        soldier 
##     0.12909944     0.12909944     0.12909944     0.12909944     0.12403473     0.12171612     0.11952286     0.11952286     0.11952286     0.11359237     0.11180340     0.11180340     0.10540926 
##         defend        crucial           sake           reli           duti         chosen       proclaim          reduc           earn        program        poverti         leader         friend 
##     0.10397505     0.10000000     0.10000000     0.10000000     0.09877296     0.09534626     0.09534626     0.08770580     0.08770580     0.08770580     0.08770580     0.08770580     0.08733338 
##          truth          honor           turn         mathia         burger           dole         clergi       brighter        presenc         absent           john         stenni         welcom 
##     0.08164966     0.05679618     0.04188539     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000 
##         repres          gilli      louisiana         silent           amen          adequ        express           50th          stood      horseback          untam         wilder              4 
##     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000 
##             13             60             50           gone        mankind         wherev            cri           moon           took         stress          glori    present-day       backward 
##     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000 
##     accomplish         master        servant          equip          yield         proper         machin        increas           1980        consist           rate         dramat         employ 
##     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000 
##        vibrant         robust          climb     birthright         restat         modern          freed           grip         sincer        meaning            arm         reduct        develop 
##     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000 
##    self-govern   totalitarian       sunlight           pois         golden         reborn           gain      two-parti       democrat     republican         boston         lawyer           adam 
##     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000 
##       virginia        planter         lesson          rival          elect           1800          later         soften          anger         letter    reestablish           1826    anniversari 
##     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000 
##         fourth           juli        exchang         sunset          wrote          beset     difficulti        valuabl            oar      overwhelm       harmless           rode         repeat 
##     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000 
##    well-intent          error           abus           send          futil          chase         spiral          bloat      prescript         disast        reelect           1984         vindic 
##     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000 
##             25       straight         incent          drive  entrepreneuri          begun         invest       interfer       simplifi           anew          least        emancip           tear 
##     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000 
##          liber       distress           area         spring         fulfil        treasur      literatur          music         poetri          dynam       unbroken        brought         reckon 
##     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000 
##          point        cabinet          staff           debt         submit          freez          desir          money       servitud           bill            due    unconstitut          handl 
##     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000 
##      fundament         upgrad         infirm    disadvantag          offer       unfortun          older         origin         instal         custom          stori        hearten    brotherhood 
##     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000 
##          hesit          abund         though         corner        primari          utter        fervent          scorn         soviet        conduct        buildup         awesom         offens 
##     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000 
##        legitim        discuss         elimin         either         resort         retali          logic        recours         approv       research         shield        militar      demilitar 
##     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000 
##         render        obsolet           agre            rid           sinc       fourfold         nowher      hemispher        deepest       worldwid         hunger  self-determin        inalien 
##     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000 
##     staunchest        conquer           blow        inflict        oppress          youth         expans            lit         lightn      transcend         ribbon         unfurl         symbol 
##     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000 
##         gotten          insid        general           knee         valley           lone         darken         ponder          alamo       encourag        settler           push           sing 
##     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000 
##           song         unknow          sound      big-heart       idealist         tender           hold         affect          dedic           wait          quayl        mitchel         wright 
##     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000 
##    congressman        michael         reagan         behalf            200     bicentenni        gladden        concret           stun          porch        suspend            bow           heed 
##     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000 
##          write           lord          breez        refresh         dictat          blown       lifeless           tree          thick            fog            sit           mist           walk 
##     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000 
##           agit     intellectu      satisfact         speech        exercis         unhamp         perhap           late          wrest         summon          saint           loud        enthral 
##     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000 
##         materi        appreci          nobil           bank          loyal         driven           stay        quieter         deeper           gold           silk          finer         wholli 
##     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000 
##         unless          engag         kinder        gentler       homeless           roam       normalci         enslav         addict          demor           slum          rough        guidanc 
##     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000 
##           case           fund            low         wallet          alloc         wisest          activ       hands-on         involv           unus         talent        unfocus     leadership 
##     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000 
##    stewardship         second          organ        sometim           hous          agenc          pitch         execut         thrash         fiscal        dissens        harmoni         chorus 
##     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000 
##      statement          motiv        untrust          cleav        earnest        quarter         statut         sunder bipartisanship        opposit          major          clock           wish 
##     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000 
##       partisan      unaccount         assist          beget        endless         treati      agreement          marbl         candor     compliment       subtleti   relationship         experi 
##     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000 
##          vigil          throw            son          battl           hymn      sentiment      continuum        inescap           kite       circumst        neither          princ           pope 
##     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000 
##          yearn           easi          going        attitud        intoler        obvious         cocain         smuggl           ship           dead       bacteria           hurt       mistrust 
##     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000 
##         larger           flaw      boundless          drama        trumpet           book           page         oldest       reinvent        pursuit    predecessor   half-centuri      steadfast 
##     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000 
##        depress         shadow        sunshin        unrival        inherit       stagnant          inequ          sworn         uphold           news         slowli           boat      broadcast 
##     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000 
##      instantan      tobillion       communic          mobil          magic     livelihood       competit          shake         urgent            abl         compet         harder         devast 
##     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000 
##       bankrupt           abid          drift           erod         shaken        fearsom       restless         muster      construct          crise         pillar           envi          engin 
##     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000 
##       deadlock         season         massiv         poster         wander         expect          revit        intrigu         calcul         maneuv          posit          worri          sweat 
##     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000 
##           pave       privileg          shout       advantag       franklin      roosevelt        persist     experiment      yesterday         abroad          stabl        collaps         animos 
##     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000 
##         shrink           lest         engulf         intern           defi      diplomaci         whenev        persian           gulf        somalia      testament         rejoic       unmistak 
##     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000 
##           vote       undertak      reconnect           torn           inde         recogn          reded         ennobl         myriad         upward      disciplin          weari        well-do 
##     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000 
##          faint            joy    mountaintop          guard    distinguish          guest           rare         affirm           gore        contest     slave-hold           went        fallibl 
##     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000 
##          grand     insignific          enact           halt          delay           rock            sea           seed           root         inborn            225         hidden           imag 
##     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000 
##         onward     background           deal         forgiv          stake         appear       undermin         permit         vulner         tactic           chao         inspir        condemn 
##     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000 
##         apathi        medicar          spare        prevent          recov       momentum          invit           mass         horror         mistak          arrog        aggress     compassion 
##     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000 
##       unworthi           view           risk          fault       prolifer         prison      substitut       diminish       hopeless        respond         mentor         pastor       synagogu 
##     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000 
##          mosqu           lend          wound        jericho      scapegoat         option         privat          civic          basic        uncount        unhonor        comfort         attack 
##     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000 
##        spectat         belief           miss      statesman          angel      whirlwind        accumul          theme          etern           tire         finish         cheney            non 
##     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000 
##       prescrib         durabl    consequenti            wit           half      shipwreck          repos         sabbat         simmer         resent          prone        ideolog          excus 
##     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000 
##         murder       multipli         mortal          reign          expos         tyrant          event        conclus      matchless          imper          slave         polici       institut 
##     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000 
##      primarili           aris          style         attain       concentr        unlimit       consider          unwis        clarifi        pretend           jail         dissid          chain 
##     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000 
##         humili          merci          bulli      treatment        concess         appeal       swiftest            odd        surpris         eventu      oppressor        repress           exil 
##     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000 
##         outlaw          regim         retain        counsel        concert         promot         prelud       dishonor          kindl           burn        hardest       intellig          devot 
##     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000 
##          death       youngest         fragil           evil        essenti       unfinish        subsist        broader        definit      homestead             gi      ownership          widen 
##     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000 
##          insur          agent         integr         edific          sinai         sermon          mount          koran           vari       surround         unwant          worth         racism 
##     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000 
##         baggag       perspect         includ      viewpoint         credit          known           felt     fellowship         victim         unjust        encount         captiv        complet 
##     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000 
##          wheel         outrag         banner          meant            ebb         visibl           bell           rang        thereof          biden          color           skin          tenet 
##     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000 
##        articul      self-evid          endow        unalien      never-end    self-execut           1776            mob        entrust          sword      half-slav       half-fre       railroad 
##     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000 
##          speed          train         discov         hazard      misfortun     relinquish        skeptic        central        fiction          initi         insist          fidel         musket 
##     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000 
##        militia           math        teacher            lab          steel          prove         resili       recoveri           bare          brink       bleakest        anybodi        outworn 
##     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000 
##        inadequ         revamp           code          empow       twilight         disabl          lucki           loss         sudden          swept        terribl       medicaid          taker 
##     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000 
##         climat         betray       judgment           none         impact        drought         resist           cede         forest       waterway           crop       snow-cap           peak 
##     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000 
##        perpetu        uniform           sear           harm           heir            won           naiv      suspicion         anchor           asia         africa           east         compel 
##     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000 
##         margin        describ       forebear         seneca          selma       stonewal           sung         unsung      footprint       preacher       inextric        pioneer           wive 
##     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000 
##       daughter            gay        brother         sister         enlist       workforc          expel     appalachia           lane        newtown        cherish        contour          exact 
##     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000 
## centuries-long        absolut       spectacl       name-cal      imperfect        partial             40            400           henc         confer   philadelphia          recit          durat 
##     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000     0.00000000 
##        passion 
##     0.00000000
```

## Scaling document positions

We have a lot of development work to do on the `textmodel()` function, but here is a demonstration of unsupervised document scaling comparing the "wordfish" model: 

```r
# make prettier document names
ieDfm <- dfm(data_corpus_irishbudget2010)
textmodel(ieDfm, model = "wordfish", dir=c(2,1))
## Fitted wordfish model:
## Call:
## 	textmodel_wordfish(x = x, dir = ..1)
## 
## Estimated document positions:
## 
##                                Documents      theta         SE       lower       upper
## 1        2010_BUDGET_01_Brian_Lenihan_FF  1.8268036 0.02020113  1.78720939  1.86639781
## 2       2010_BUDGET_02_Richard_Bruton_FG -0.5855453 0.02767752 -0.63979326 -0.53129738
## 3         2010_BUDGET_03_Joan_Burton_LAB -1.0696093 0.01556654 -1.10011977 -1.03909892
## 4        2010_BUDGET_04_Arthur_Morgan_SF -0.1141058 0.02791284 -0.16881502 -0.05939668
## 5          2010_BUDGET_05_Brian_Cowen_FF  1.7742535 0.02354795  1.72809951  1.82040749
## 6           2010_BUDGET_06_Enda_Kenny_FG -0.7016675 0.02613599 -0.75289405 -0.65044099
## 7      2010_BUDGET_07_Kieran_ODonnell_FG -0.4967371 0.04081043 -0.57672554 -0.41674864
## 8       2010_BUDGET_08_Eamon_Gilmore_LAB -0.5438955 0.02931964 -0.60136202 -0.48642903
## 9     2010_BUDGET_09_Michael_Higgins_LAB -1.0104557 0.03669246 -1.08237288 -0.93853843
## 10       2010_BUDGET_10_Ruairi_Quinn_LAB -0.9879423 0.03741468 -1.06127505 -0.91460950
## 11     2010_BUDGET_11_John_Gormley_Green  1.1913710 0.07324285  1.04781505  1.33492703
## 12       2010_BUDGET_12_Eamon_Ryan_Green  0.1500193 0.06254512  0.02743087  0.27260773
## 13     2010_BUDGET_13_Ciaran_Cuffe_Green  0.7196468 0.07340590  0.57577127  0.86352238
## 14 2010_BUDGET_14_Caoimhghin_OCaolain_SF -0.1521357 0.03644625 -0.22357033 -0.08070102
## 
## Estimated feature scores: showing first 30 beta-hats for features
## 
##            when               i       presented             the   supplementary          budget              to            this           house            last           april               , 
##     -0.15058982      0.34726053      0.36922895      0.21628040      1.09099461      0.05829019      0.33579912      0.26839752      0.14935164      0.25353066     -0.13851438      0.30630139 
##            said              we           could            work             our             way         through          period              of          severe        economic        distress 
##     -0.77779200      0.43921107     -0.59703182      0.54219872      0.70824736      0.29607747      0.62274642      0.51835687      0.30095010      1.25529502      0.44375418      1.83470247 
##               .           today             can          report            that notwithstanding 
##      0.23398491      0.14174074      0.32480755      0.65459489      0.04184343      1.83470247
```

## Topic models

**quanteda** makes it very easy to fit topic models as well, e.g.:


```r
quantdfm <- dfm(data_corpus_irishbudget2010,
                remove = c("will", stopwords("english")))

if (require(topicmodels)) {
    myLDAfit20 <- LDA(convert(quantdfm, to = "topicmodels"), k = 20)
    get_terms(myLDAfit20, 5)
    topics(myLDAfit20, 3)
}
## Loading required package: topicmodels
## Warning in library(package, lib.loc = lib.loc, character.only = TRUE, logical.return = TRUE, : there is no package called 'topicmodels'
```
