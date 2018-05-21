---
title: 'quanteda: An R package for the quantitative analysis of textual data'
authors:
- affiliation: 1
  name: Kenneth Benoit
  orcid: 0000-0002-0797-564X
- affiliation: 1
  name: Kohei Watanabe
  orcid: 0000-0001-6519-5265
- affiliation: 2
  name: Haiyan Wang
  orcid: 0000-0000-0000-0000
- affiliation: 3
  name: Paul Nulty
  orcid: 0000-0000-0000-0000
- affiliation: 4
  name: Adam Obeng
  orcid: 0000-0000-0000-0000
- affiliation: 5
  name: Stefan Müller
  orcid: 0000-0002-6315-4125
- affiliation: 1
  name: Akitaka Matsuo
  orcid: 0000-0002-3323-6330
date: "4 May 2018"
bibliography: paper.bib
tags:
- text mining
- natural language processing
affiliations:
- index: 1
  name: Department of Methodology, London School of Economics and Political Science
- index: 2
  name: De Beers Inc.
- index: 3
  name: Centre for Research in Arts, Social Science and Humanities, University of Cambridge
- index: 4
  name: Facebook Inc.
- index: 5
  name: Department of Political Science, Trinity College Dublin
---

# Summary

quanteda is an R package providing a comprehensive workflow and toolkit for managing and analyzing textual data, including corpus management, natural language processing, tokenization, analysis, and plotting. It has extensive functions for applying dictionary analysis, for exploring texts using keywords-in-context, discovering keywords, computing document and feature similarities, and discovering multi-word expressions through collocation scoring. It provides highly efficient methods for compiling sparse document-feature matrices and for manipulating these or using them in further quantitative analysis, using entirely sparse operations. In addition, the package provides similar functions for compiling sparse feature co-occurrence matrices, where the context window and the distance weights are entirely flexible. Making extensive use of C++ and multi-threading, quanteda is also considerably faster and more efficient than other R and Python packages for quantitative text analysis.

# Corpus management and NLP

The package makes it easy to manage texts in the form of a "corpus:, defined as a collection of texts that includes document-level variables specific to each text, as well as meta-data for documents and for the collection as a whole. **quanteda** includes tools to make it easy and fast to manipulate the texts in a corpus, by performing the most common natural language processing tasks simply and quickly, such as tokenizing, stemming, or forming ngrams. quanteda's functions for tokenizing texts and forming multiple tokenized documents into a sparse "document-feature matrix"" are both extremely fast and extremely simple to use.  **quanteda** can segment texts easily by words, paragraphs, sentences, or even user-supplied delimiters and tags.

**quanteda** is principally designed to allow users a fast and convenient method to go from a corpus of texts to a selected matrix of documents by features, after defining what the documents and features. The package makes it easy to redefine documents, for instance by splitting them into sentences or paragraphs, or by tags, as well as to group them into larger documents by document variables, or to subset them based on logical conditions or combinations of document variables. The package also implements common NLP feature selection functions, such as removing stopwords and stemming in numerous languages, selecting words found in dictionaries, treating words as equivalent based on a user-defined "thesaurus", and trimming and weighting features based on document frequency, feature frequency, and related measures such as _tf-idf_.

Built on the text processing functions in the **stringi** package [@stringi], which is in turn built on C++ implementation of the ICU libraries for Unicode text handling, quanteda correctly and efficiently implements Unicode rules and regular expression character categories for handling of text in any character set.

# Available models and textual statistics

Because quanteda was designed from the outset for the social scientific analysis of textual data, it is especially suited to research.  Its "textmodel" functions provide native, highly efficient implementations of several text analytic scaling methods, such as Wordscores [@lbg:2003], Wordfish [@SlapinProksch2008], class affinity scaling [@PerryBenoit2017], and correspondence analysis [@greenacre1984].  More general textmodel functions include efficient implementations of a multinomial Naive Bayes classifier designed specifically for textual data [@Manningetal2008] and latent semantic analysis [@deerwester1990].  **quanteda** also works flexibly and efficiently with dictionaries, and is distributed with 2015 version of the Lexicoder Sentiment Dictionary [@youngsoroka2012].

In addition to models, the package provides a variety of text-based statistics, such as frequency, ``keyness'', lexical diversity, readability, and similarity and distance of documents or features.  These make use of the sparseness document-feature matrices -- often up to 90% sparse -- and parallelism for efficient, fast computation.  It also provides methods for statistically scoring collocations, useful in identifying multi-word expressions.


# Visualizations

The package provides extensive methods for visualizing textual analyses, via its family of "textplot" functions.  These are typically designed to take another package object as an input, to produce a specific form of plot.  For instance, from a "feature co-occurrence matrix" or "fcm", we can directly plot a network using `textplot_network()`:

```r
library("quanteda")

# construct the feature co-occurrence matrix
examplefcm <- 
    tokens(data_corpus_irishbudget2010, remove_punct = TRUE) %>%
    tokens_tolower() %>%
    tokens_remove(stopwords("english"), padding = FALSE) %>%
    fcm(context = "window", window = 5, tri = FALSE)

# choose 30 most frequency features
topfeats <- names(topfeatures(myfcm, 30))

# select the top 30 features only, plot the network 
set.seed(100)
textplot_network(fcm_select(examplefcm, topfeats), min_freq = 0.8)
```

![Feature co-occurrence network plot example.](networkplot.png)



# Package design

**quanteda** has been carefully designed with several key aims in mind.

_Consistency_.  Behind the design of quanteda lies an obsession with consistency. Its function and object names follow a careful naming scheme, such that functions such as `corpus()` and `tokens()` construct those object classes, and subsequent functions starting with the object class name and an underscore, such as `corpus_reshape()` (for reshaping a corpus into sentences or paragraphs) or `tokens_lookup()` (for converting pattern matches in a dictionary into the dictionary’s key entries) take as their first input those object classes, and output a modified object of the same object class. This means also that it is easy for new users (beginner or expert) to get a clear overview of the package functions, as they group nicely in the documentation index. This even applies to the extensive built-in data objects in the package, such as `data_corpus_inaugural`, signalling that the object is data and has the class of corpus, with its descriptor indicating that this is the corpus of US presidents’ inaugural addresses.

_Accessibility_.  In addition to the accessibility that the consistent API provides, **quanteda** also comes with extensive manual pages for every function and data object, with examples. On the website at http://docs.quanteda.io, furthermore, there are extensive on-line documentation, package vignettes, examples, and tutorials. The objective is to make **quanteda** accessible to beginner users but also to be  powerful and flexible enough also to satisfy expert users.

_Performance_.  **quanteda** is designed to squeeze every ounce of performance possible from the R environment, which it does using a combination of efficient object design, extensive use of C++ for core processing and mathematical computing tasks, parallelization, and generally efficient R programming structures.

_Transparency and reproducibility_.  **quanteda** was designed from the outset for rigorous, transparent, and reproducible scientific analysis of text.  Because it is entirely open-source, it can be trusted because it is open to extensive scrutiny (and correction) by experts. The structure of its text processing functions, moreover, is designed to encourage a reproducible workflow, and permits the chaining of operations using the "pipe" operator ``%>%`` from the **magrittr** package [@magrittr]. 

_Compatibility with other packages_.  For analysis not offered built-in, the package is designed to make it wasy to hand off its core objects seamlessly to other packages such as the **stm** package for structural topic models [@STM].  It also provides functions for easy conversion of its document-feature matrices into objects usable by other topic model packages or word embedding packages like **text2vec** [@text2vec].  The package also works very nicely companion packages such as **spacyr**, an R wrapper to the spaCy NLP pipeline [@spacy2], and **readtext**, a package for reading and converting text files.  Finally, it has import and export function either that make it very easy to go between a "tidy" approach using the **tidytext** package [@tidytextJOSS] or other text analysis frameworks such as **tm** [@tmJSS].

# Funding and Support

Created at the London School of Economics in 2013 with funding from the European Research Council funding (ERC-2011-StG 283794-QUANTESS), **quanteda** is now supported by the Quanteda Initiative, a non-profit organization founded in 2018 to provide ongoing support for the “quanteda ecosystem” of open-source text analysis software.

# References
