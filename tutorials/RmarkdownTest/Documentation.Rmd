Importing Documents into a Quanteda Corpus
========================================================

A Quanteda corpus is a combination of texts and a dataframe of attribute-value pairs that apply to the texts. The data packaged with Quanteda includes a vector of texts of Irish budget speeches, and a dataframe of attributes describing the texts.

```{r}
data(ieTexts)
summary(ieTexts)
data(ieAttribs)
names(ieAttribs)
```

We can create a new corpus by combining the texts and the dataframe of attributes:

```{r}
budgetCorpus <- corpusCreate(ieTexts, ieAttribs)
```

This corpus object is our basic mapping between texts and their attributes, and is the input to many of Quanteda's functions. For example, we can extract a document-feature matrix:

```{r}
budgetDfm <- dfm(budgetCorpus)
```

We can use a document-feature matrix to perform quantitative analysis, for example scaling the texts with Poisson scaling:

You can also embed plots, for example:
```{r}
library(austin)
wfm <- as.wfm(t(budgetDfm))
wf <- wordfish(wfm)

```

And plotting the result:

```{r fig.width=7, fig.height=6}
plot(wf, col=classes[order(wf$theta)])
```

