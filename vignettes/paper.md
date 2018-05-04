---
title: 'quanteda: R package for quantitative analysis of textual data'
tags:
  - text analysis
  - NLP
  - R
authors:
 - name: Kenneth Benoit
   orcid: 0000-0000-0000-0000
   affiliation: 1
 - name: Kohei Watanabe
   orcid: 0000-0001-6519-5265
   affiliation: 1
 - name: Haiyan Wang
   orcid: 0000-0000-0000-0000
   affiliation: 1
 - name: Paul Nulty
   orcid: 0000-0000-0000-0000
   affiliation: 1
 - name: Adam Obeng
   orcid: 0000-0000-0000-0000
   affiliation: 2
 - name: Stefan Müller
   orcid: 0000-0000-0000-0000
   affiliation: 5
 - name: Akitaka Matsuo
   orcid: 0000-0000-0000-0000
   affiliation: 1

 
affiliations:
 - name: London School of Economics and Political Science
   index: 1
 - name: De Beers Inc.
   index: 2
 - name: Cambridge University
   index: 3
 - name: Facebook inc.
   index: 4
 - name: Trinity Colledge Dublin
   index: 5

date: 14 February 2016
# bibliography: paper.bib
---

# Summary

This is a proof of concept integration between a GitHub [@GitHub] repo and figshare [@figshare] in an effort to get a DOI for a GitHub repository. When a repository is tagged for release on GitHub, Fidgit [@Fidgit] will import the release into figshare thus giving the code bundle a DOI. In a somewhat meta fashion, Fidgit is publishing itself to figshare with DOI 'https://doi.org/10.6084/m9.figshare.828487' [@figshare_archive].


## Design

quanteda has been carefully designed with four key aims:

Consistency. Behind the design of quanteda lies an obsession with consistency. Its “API” (in the form of the design of its interrelated functions, their names, and the type and names of the function arguments) uses a carefully chosen naming scheme, such that functions such as corpus() and tokens() construct those object classes, and subsequent functions starting with the object class name and an underscore, such as corpus_reshape() (for reshaping a corpus into sentences or paragraphs) or tokens_lookup() (for converting pattern matches in a dictionary into the dictionary’s key entries) take as their first input those object classes, and output a modified object of the same object class. This means also that it is easy for new users (beginner or expert) to get a clear overview of the package functions, as they group nicely in the documentation index. This even applies to the extensive built-in data objects in the package, such as data_corpus_inaugural, signalling that the object is data and has the class of corpus, with its descriptor indicating that this is the corpus of US presidents’ inaugural addresses.


Accessibility. In addition to the accessibility that the consistent API provides, quanteda also provides extensive, clearly written manual pages for every function and data object, with examples. On the website at http://docs.quanteda.io, furthermore, there are extensive on-line documentation, package vignettes, examples, and tutorials. To make this accessible in other languages, we have translated these into Chinese, Japanese, and Spanish. The objective is to make quanteda accessible to beginners, especially students in master’s and PhD level courses on quantitative text analysis, but also powerful and flexible enough also to satisfy expert users.


Performance. quanteda is designed to squeeze every ounce of performance possible from the R environment, which it does using a combination of efficient object design, extensive use of C++ for core processing and mathematical computing tasks, parallelization, and generally efficient R programming structures. Benchmarked against other packages in R, Python, and Java for performance, quanteda has been shown to be generally faster to alternatives. 

Transparency and encouragement of a reproducible workflow. While quanteda can be used by anyone for any purpose (in accord with its GPL license), it was designed from the outset for rigorous, transparent, and reproducible scientific analysis of text. Because all of its source code is published on GitHub, it can be trusted because it is open to extensive scrutiny (and correction) by experts. As of the submission of this nomination, the package has been downloaded over 125,000 times from CRAN, has opened 755 issues (and closed 94% of these), and has an active, dedicated channel on StackOverflow. The structure of its text processing functions, moreover, is designed to encourage a reproducible workflow, and permits the chaining of operations using the “pipe” operator %>% from the magrittr package.


-![Fidgit deposited in figshare.](https://cdn.rawgit.com/quanteda/quanteda/master/images/quanteda_logo.svg)

# References
