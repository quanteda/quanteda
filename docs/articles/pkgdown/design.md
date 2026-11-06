# The Structure of quanteda

See <https://quanteda.io> for additional tutorials, examples, and
general documentation.

## Introduction

## The logic of Quanteda’s design

### Grammatical rules

The “grammar” of the package is split between three basic types of
functions and data objects:

- *object*: a constructor function named `object()` that returns an
  object of class *object*. Example:
  [`corpus()`](https://quanteda.io/reference/corpus.md) constructs a
  `corpus` class object.

- *object*`_`*verb*: a function that inputs an object of class *object*,
  and returns a a modified *object* class object. There are no
  exceptions to this naming rule, so that even functions that operate on
  character objects following this convention, such as
  [`char_tolower()`](https://quanteda.io/reference/char_tolower.md).
  (Ok, so there is a slight exception: we abbreviated `character` to
  `char`!)

- `data_`*class*`_`*descriptor*: data objects are named this way to
  clearly distinguish them and to make them easy to identify in the
  index. The first part identifies them as data, the second names their
  object class, and the third component is a descriptor. Example:
  `data_corpus_inaugural` is the **quanteda**
  [`corpus()`](https://quanteda.io/reference/corpus.md) class object
  consisting of the US presidents’ inaugural addresses.

- `text`*general*`_`*specific*: functions that input a **quanteda**
  object and return the result of an analysis, as a new type of object.
  Only the underscored functions that begin with `text` break the
  previous rule about the first part of the name identifying the object
  class that is input and output. Examples:
  [`textstat_readability()`](https://quanteda.io/reference/textstat_readability.html)
  takes a character or corpus as input, and returns a data.frame;
  [`textplot_xray()`](https://rdrr.io/pkg/quanteda.textplots/man/textplot_xray.html)
  takes a `kwic` object as input, and generates a dispersion plot (named
  “x-ray” because of its similarity to the plot produced by Kindle).

- Extensions of R functions: These are commonly used R functions, such
  as [`head()`](https://rdrr.io/r/utils/head.html), that are also
  defined for **quanteda** objects. Examples:
  [`head.dfm()`](https://quanteda.io/reference/head.dfm.md), coercion
  functions such as `as.list.tokens`, and Boolean class type checking
  functions such as
  [`is.dfm()`](https://quanteda.io/reference/as.dfm.md). Many
  post-estimation methods defined for `lm` objects, for instance
  [`predict()`](https://rdrr.io/r/stats/predict.html), are also defined
  for most `textmodel` objects

- R-like functions. These are functions for **quanteda** objects that
  follow naming conventions and functionality that should be very
  familiar to users of R. Example:
  [`ndoc()`](https://quanteda.io/reference/ndoc.md) returns the number
  of documents in a corpus, tokens, or dfm object, similar to
  [`base::nrow()`](https://rdrr.io/r/base/nrow.html). Note that like
  [`nrow()`](https://rdrr.io/r/base/nrow.html),
  [`ndoc()`](https://quanteda.io/reference/ndoc.md) is not plural. Other
  examples include
  [`docnames()`](https://quanteda.io/reference/docnames.md) and
  [`featnames()`](https://quanteda.io/reference/featnames.md) – similar
  to [`rownames()`](https://rdrr.io/r/base/colnames.html) and
  [`colnames()`](https://rdrr.io/r/base/colnames.html).

- Grammatical exceptions: Every language has these, usually due to path
  dependency from historical development, and **quanteda** is no
  exception. The list, however, is short:

  - [`convert()`](https://quanteda.io/reference/convert.md): converts
    from a dfm to foreign package formats  
  - [`sparsity()`](https://quanteda.io/reference/sparsity.md): returns
    the sparsity (as a proportion) of a dfm  
  - [`topfeatures()`](https://quanteda.io/reference/topfeatures.md):
    returns a named vector of the counts of the most frequently
    occurring features in a `dfm`.

### Constructors for core data types

The **quanteda** package consists of a few core data types, created by
calling constructors with identical names. These are all “nouns” in the
sense of declaring what they construct. This follows very similar R
behaviour in many of the core R objects, such as
[`data.frame()`](https://rdrr.io/r/base/data.frame.html),
[`list()`](https://rdrr.io/r/base/list.html), etc.

Core object types and their constructor functions:

- `corpus`  
- `tokens`
- `dfm`  
- `fcm`
- `kwic`
- `dictionary`

Note that a core object class in **quanteda** is also the `character`
atomic type, for which there is no constructor function, and is
abbreviated as `char` in the function nomenclature.

### Functions for manipulating core data types

#### Naming convention

All functions that begin with the name of a core object class will both
*input* and *output* an object of this class, without exception.

This replaces the approach in versions up to 0.9.8.5 where a general
method such as `selectFeatures()` was defined for each applicable class
of core object. This approach made the specific function behaviour
unpredictable from the description of the general behaviour. It also
made it difficult to get an overview of the functionality available for
each object class. By renaming these functions following the convention
of object class, followed by an underscore, followed by a verb (or
verb-like statement), we could both separate the behaviours into
specific functions, as well as clearly describe through the function
name what action is taken on what type of object.

#### Advantages

In our view, the advantages of this clarity outweigh whatever advantages
might be found from overloading a generic function. The functions
[`corpus_sample()`](https://quanteda.io/reference/corpus_sample.md),
[`tokens_sample()`](https://quanteda.io/reference/tokens_sample.md), and
[`dfm_sample()`](https://quanteda.io/reference/dfm_sample.md), for
instance, are clearer to understand and read from a package’s function
index, than the previously overloaded version of
[`sample()`](https://rdrr.io/r/base/sample.html) that could be
dispatched on a corpus, tokenized text, or dfm object. Additionally, in
the case of [`sample()`](https://rdrr.io/r/base/sample.html), we avoid
the namespace “conflict” caused by redefining the function as a generic,
so that it could be overloaded. Our new, more specific naming
conventions therefore reduce the likelihood of namespace conflicts with
other packages.

#### Why are some operations unavailable for specific object types?

Because not every operation makes sense for every object type. Take the
example of a “feature co-occurrence matrix”, or `fcm`. Construction of a
feature co-occurrence matrix is slightly different from constructing a
dfm. Unlike the “Swiss-army” knife approach of
[`dfm()`](https://quanteda.io/reference/dfm.md), which can operate
directly on texts, [`fcm()`](https://quanteda.io/reference/fcm.md) works
only on tokens, since the definition of how the context of co-occurrence
is defined is dependent on token sequences and therefore highly
dependent on tokenization options. In addition,
[`fcm()`](https://quanteda.io/reference/fcm.md) is likely to be used a
lot less frequently, and primarily by more expert users.

Furthermore, many functions defined for
[`fcm()`](https://quanteda.io/reference/fcm.md) objects *should* be
unavailable, because they violate the principles of the object. For
instance, `fcm_wordstem()` and
[`fcm_tolower()`](https://quanteda.io/reference/dfm_tolower.md) should
not be applied to [`fcm()`](https://quanteda.io/reference/fcm.md)
objects, because collapsing these and treating them as equivalent (as
for a dfm object) is incorrect for the context in which co-occurrence is
defined, such as a +/- 5 token window.

### Extensions of core R functions

Many simple base R functions – simpler at least than the example of
[`sample()`](https://rdrr.io/r/base/sample.html) cited above – are still
extended to quanteda objects through overloading. The logic of allowing
is that these functions,
e.g. [`cbind()`](https://rdrr.io/r/base/cbind.html) for a dfm, are very
simple and very common, and therefore are well-known to users.
Furthermore, they can operate in only one fashion on the object for
which they are defined, such as
[`cbind()`](https://rdrr.io/r/base/cbind.html) combining two dfm objects
by joining columns. Similar functions extended in this way include
[`print()`](https://rdrr.io/r/base/print.html),
[`head()`](https://rdrr.io/r/utils/head.html),
[`tail()`](https://rdrr.io/r/utils/head.html), and
[`t()`](https://rdrr.io/r/base/t.html). Most of these functions are so
natural that their documentation is not included in the package index.

### Additions to core R(-like) functions

Additional functions have been defined for **quanteda** objects that are
[very similar to simple base R functions](#r-like-functions), but are
not named using the `class_action` format because they do not return a
modified object of the same class. These follow as closely as possible
the naming conventions found in the base R functions that are similar.
For instance, [`docnames()`](https://quanteda.io/reference/docnames.md)
and [`featnames()`](https://quanteda.io/reference/featnames.md) return
the document names of various **quanteda** objects, in the same way that
[`rownames()`](https://rdrr.io/r/base/colnames.html) does for
matrix-like objects (a matrix, data.frame, data.table, etc.). The
abbreviation of
[`featnames()`](https://quanteda.io/reference/featnames.md) is
intentionally modeled on
[`colnames()`](https://rdrr.io/r/base/colnames.html). Likewise,
[`ndoc()`](https://quanteda.io/reference/ndoc.md) returns the number of
documents, using the singular form similar to
[`nrow()`](https://rdrr.io/r/base/nrow.html) and
[`ncol()`](https://rdrr.io/r/base/nrow.html).

## Workflow principles

**quanteda** is designed both to facilitate and to enforce a
“best-practice” workflow. This includes the following basic principles.

1.  **Corpus texts should remain *unchanged* during subsequent analysis
    and processing.** In other words, after *loading* and *encoding*, we
    should discourage users from modifying a corpus of texts as a form
    of processing, so that the corpus can act as a library and record of
    the original texts, prior to any downstream processing. This not
    only aids in replication, but also means that a corpus presents the
    unmodified texts to which any processing, feature selection,
    transformations, or sampling may be applied or reapplied, without
    hard-coding any changes made as part of the process of analyzing the
    texts. The only exception is to reshape the units of text in a
    corpus, but we will record the details of this reshaping to make it
    relatively easy to reverse unit changes. Since the definition of a
    “document” is part of the process of loading texts into a corpus,
    however, rather than processing, we will take a less stringent line
    on this aspect of changing a corpus.

2.  **A corpus should be capable of holding additional objects that will
    be associated with the corpus, such as dictionaries, stopword, and
    phrase lists.** These will be named objects, that can be invoked
    when using (for instance)
    [`dfm()`](https://quanteda.io/reference/dfm.md). This allows a
    corpus to contain all of the additional objects that would normally
    be associated with it, rather than requiring a set of separate,
    extra-corpus objects.

3.  **Objects should record histories of the operations applied to
    them.** This is for purposes of analytic transparency. A tokens
    object and a dfm object, for instance, should have settings that
    record the processing options applied to the texts or corpus from
    which they were created. These provide a record of what was done to
    the text, and where it came from. Examples are
    [`tokens_tolower()`](https://quanteda.io/reference/tokens_tolower.md),
    [`dfm_wordstem()`](https://quanteda.io/reference/tokens_wordstem.md),
    and settings such as `remove_twitter`. They also include any objects
    used in feature selection, such as dictionaries or stopword lists.

4.  **A dfm should always be a *documents* (or document groups) in rows
    by *features* in columns.** A `dfm` object may be transposed but
    then it is no longer a `dfm` class object.

5.  **Encoding of texts** should always be UTF-8.

## Basic text analysis workflow

### Working with a corpus, documents, and features

1.  **Creating the corpus**

    Reading files, probably using `readtext()` from the **readtext**
    package, then creating a corpus using
    [`corpus()`](https://quanteda.io/reference/corpus.md), making sure
    the texts have a common encoding, and adding document variables
    ([`docvars()`](https://quanteda.io/reference/docvars.md)) and
    metadata (`meta`).

2.  **Defining and delimiting documents**

    Defining what are “texts”, for instance using
    [`corpus_reshape()`](https://quanteda.io/reference/corpus_reshape.md)
    or grouping
    ([`corpus_segment()`](https://quanteda.io/reference/corpus_segment.md)).

3.  **Defining and delimiting textual features**

    This step involves defining and extracting the relevant features
    from each document, using
    [`tokens()`](https://quanteda.io/reference/tokens.md), the main
    function for this step, involves identifying instances of defined
    features (“tokens”) and extracting them as vectors. Usually these
    will consist of words, but may also consist of:

    - `ngrams`: adjacent sequences of words, not separated by
      punctuation marks or sentence boundaries; including
    - multi-word expressions, through
      [`tokens_compound()`](https://quanteda.io/reference/tokens_compound.md),
      for selected word ngrams as identified in selected lists rather
      than simply using all adjacent word pairs or n-sequences.

    [`tokens()`](https://quanteda.io/reference/tokens.md) returns a new
    object class of tokenized texts, a hashed list of index types, with
    each element in the list corresponding to a document, and each hash
    vector representing the tokens in that document.

    By defining the broad class of tokens we wish to extract, in this
    step we also apply rules that will keep or ignore elements such as
    punctuation or digits, or special aggregations of word and other
    characters that make up URLs, Twitter tags, or currency-prefixed
    digits. This will involve adding the following options to `tokens`:

    - `remove_numbers`
    - `remove_punct`
    - `remove_symbols`
    - `remove_twitter`
    - `remove_url`

    **By default**,
    [`tokens()`](https://quanteda.io/reference/tokens.md) extracts word
    tokens, and only `remove_separators` is `TRUE`, meaning that
    [`tokens()`](https://quanteda.io/reference/tokens.md) will return a
    list including punctuation as tokens. This follows a philosophy of
    minimal intervention, and one requiring that additional decisions be
    made explicit by the user when invoking
    [`tokens()`](https://quanteda.io/reference/tokens.md).

    For converting to lowercase, it is actually faster to perform this
    step *before* tokenization, but logically it falls under the next
    workflow step. However for efficiency, `*_tolower()` functions are
    defined for `character`, `tokens`, and `dfm` objects.

    Since the tokenizer we will use may not distinguish the punctuation
    characters used in constructs such as URLs, email addresses, Twitter
    handles, or digits prefixed by currency symbols, we will mostly need
    to use a substitution strategy to replace these with alternative
    characters prior to tokenization, and then replace the substitutions
    with the original characters. This will slow down processing but
    will only be active by explicit user request for this type of
    handling to take place.

    Note that that defining and delimiting features may also include
    their *parts of speech*, meaning we will need to add functionality
    for POS tagging and extraction in this step.

4.  **Further feature selection**

    Once features have been identified and separated from the texts in
    the tokenization step, features may be removed from token lists, or
    handled as part of `dfm` construction. Features may be:

    - *eliminated* through use of predefined lists or patterns of *stop
      words*, using `dfm(x, remove = )` or
      [`tokens_remove()`](https://quanteda.io/reference/tokens_select.md)
    - *kept* through use of predefined lists or patterns of *stop
      words*, using `dfm(x, select = )` or
      [`tokens_select()`](https://quanteda.io/reference/tokens_select.md)
    - *collapsed* by:
      - considering morphological variations as equivalent to a stem or
        lemma, through `stem` option in `dfm` (or
        [`dfm_wordstem()`](https://quanteda.io/reference/tokens_wordstem.md))
      - considering lists of features as equivalent to a *dictionary*
        key, either exclusively using `dfm(x, dictionary = )` or as a
        supplement to uncollapsed features through
        `dfm(x, thesaurus = )`
      - [`tokens_tolower()`](https://quanteda.io/reference/tokens_tolower.md)
        or
        [`dfm_tolower()`](https://quanteda.io/reference/dfm_tolower.md)
        to consider as equivalent the same word features despite having
        different cases, by converting all features to lower case

    It will be sometimes possible to perform these steps separately from
    the `dfm` creation stage, but in most cases these steps will be
    performed as options to the
    [`dfm()`](https://quanteda.io/reference/dfm.md) function.

5.  **Analysis of the documents and features**

    1.  From a corpus.

        These steps don’t necessarily require the processing steps
        above.

        - [`kwic()`](https://quanteda.io/reference/kwic.md)
        - [`textstat_readability()`](https://quanteda.io/reference/textstat_readability.html)
        - [`summary()`](https://rdrr.io/r/base/summary.html)

    2.  From a `dfm` – after
        [`dfm()`](https://quanteda.io/reference/dfm.md) on the processed
        document and features.
