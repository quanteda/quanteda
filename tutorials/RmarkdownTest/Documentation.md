Importing Documents into a Quanteda Corpus
========================================================

A Quanteda corpus is a combination of texts and a dataframe of attribute-value pairs that apply to the texts. The data packaged with Quanteda includes a vector of texts of Irish budget speeches, and a dataframe of attributes describing the texts.


```r
data(ieTexts)
```

```
## Warning: data set 'ieTexts' not found
```

```r
summary(ieTexts)
```

```
## Error: object 'ieTexts' not found
```

```r
data(ieAttribs)
```

```
## Warning: data set 'ieAttribs' not found
```

```r
names(ieAttribs)
```

```
## Error: object 'ieAttribs' not found
```

We can create a new corpus by combining the texts and the dataframe of attributes:


```r
budgetCorpus <- corpusCreate(ieTexts, ieAttribs)
```

```
## Error: could not find function "corpusCreate"
```

This corpus object is our basic mapping between texts and their attributes, and is the input to many of Quanteda's functions. For example, we can extract a document-feature matrix:


```r
budgetDfm <- dfm(budgetCorpus)
```

```
## Error: object 'budgetCorpus' not found
```

We can use a document-feature matrix to perform quantitative analysis, for example scaling the texts with Poisson scaling:

You can also embed plots, for example:

```r
library(austin)
wfm <- as.wfm(t(budgetDfm))
```

```
## Error: object 'budgetDfm' not found
```

```r
wf <- wordfish(wfm)
```

```
## Error: First argument must be an object of type wfm
```

And plotting the result:


```r
plot(wf, col=classes[order(wf$theta)])
```

```
## Error: object 'wf' not found
```

