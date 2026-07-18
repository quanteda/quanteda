# Get or set document-level variables

Get or set variables associated with a document in a
[corpus](https://quanteda.io/reference/corpus.md),
[tokens](https://quanteda.io/reference/tokens.md) or
[dfm](https://quanteda.io/reference/dfm.md) object.

## Usage

``` r
docvars(x, field = NULL)

docvars(x, field = NULL) <- value

# S3 method for class 'corpus'
x$name

# S3 method for class 'corpus'
x$name <- value

# S3 method for class 'tokens'
x$name

# S3 method for class 'tokens'
x$name <- value

# S3 method for class 'dfm'
x$name

# S3 method for class 'dfm'
x$name <- value
```

## Arguments

- x:

  [corpus](https://quanteda.io/reference/corpus.md),
  [tokens](https://quanteda.io/reference/tokens.md), or
  [dfm](https://quanteda.io/reference/dfm.md) object whose
  document-level variables will be read or set

- field:

  string containing the document-level variable name

- value:

  a vector of document variable values to be assigned to `name`

- name:

  a literal character string specifying a single docvars name

## Value

`docvars` returns a data.frame of the document-level variables, dropping
the second dimension to form a vector if a single docvar is returned.

`docvars<-` assigns `value` to the named `field`

## Note

Reassigning document variables for a
[tokens](https://quanteda.io/reference/tokens.md) or
[dfm](https://quanteda.io/reference/dfm.md) object is allowed, but
discouraged. A better, more reproducible workflow is to create your
docvars as desired in the
[corpus](https://quanteda.io/reference/corpus.md), and let these
continue to be attached "downstream" after tokenization and forming a
document-feature matrix. Recognizing that in some cases, you may need to
modify or add document variables to downstream objects, the assignment
operator is defined for
[tokens](https://quanteda.io/reference/tokens.md) or
[dfm](https://quanteda.io/reference/dfm.md) objects as well. Use with
caution.

## Accessing or assigning docvars using the `$` operator

As of quanteda v2, it is possible to access and assign a docvar using
the `$` operator. See Examples.

## Examples

``` r
# retrieving docvars from a corpus
head(docvars(data_corpus_inaugural))
#>   Year  President FirstName                 Party
#> 1 1789 Washington    George                  none
#> 2 1793 Washington    George                  none
#> 3 1797      Adams      John            Federalist
#> 4 1801  Jefferson    Thomas Democratic-Republican
#> 5 1805  Jefferson    Thomas Democratic-Republican
#> 6 1809    Madison     James Democratic-Republican
tail(docvars(data_corpus_inaugural, "President"), 10)
#>  [1] "Bush"    "Clinton" "Clinton" "Bush"    "Bush"    "Obama"   "Obama"  
#>  [8] "Trump"   "Biden"   "Trump"  
head(data_corpus_inaugural$President)
#> [1] "Washington" "Washington" "Adams"      "Jefferson"  "Jefferson" 
#> [6] "Madison"   

# assigning document variables to a corpus
corp <- data_corpus_inaugural
docvars(corp, "President") <- paste("prez", 1:ndoc(corp), sep = "")
head(docvars(corp))
#>   Year President FirstName                 Party
#> 1 1789     prez1    George                  none
#> 2 1793     prez2    George                  none
#> 3 1797     prez3      John            Federalist
#> 4 1801     prez4    Thomas Democratic-Republican
#> 5 1805     prez5    Thomas Democratic-Republican
#> 6 1809     prez6     James Democratic-Republican
corp$fullname <- paste(data_corpus_inaugural$FirstName,
                       data_corpus_inaugural$President)
tail(corp$fullname)
#> [1] "George W. Bush"  "Barack Obama"    "Barack Obama"    "Donald J. Trump"
#> [5] "Joseph R. Biden" "Donald J. Trump"


# accessing or assigning docvars for a corpus using "$"
data_corpus_inaugural$Year
#>  [1] 1789 1793 1797 1801 1805 1809 1813 1817 1821 1825 1829 1833 1837 1841 1845
#> [16] 1849 1853 1857 1861 1865 1869 1873 1877 1881 1885 1889 1893 1897 1901 1905
#> [31] 1909 1913 1917 1921 1925 1929 1933 1937 1941 1945 1949 1953 1957 1961 1965
#> [46] 1969 1973 1977 1981 1985 1989 1993 1997 2001 2005 2009 2013 2017 2021 2025
data_corpus_inaugural$century <- floor(data_corpus_inaugural$Year / 100)
data_corpus_inaugural$century
#>  [1] 17 17 17 18 18 18 18 18 18 18 18 18 18 18 18 18 18 18 18 18 18 18 18 18 18
#> [26] 18 18 18 19 19 19 19 19 19 19 19 19 19 19 19 19 19 19 19 19 19 19 19 19 19
#> [51] 19 19 19 20 20 20 20 20 20 20

# accessing or assigning docvars for tokens using "$"
toks <- tokens(corpus_subset(data_corpus_inaugural, Year <= 1805))
toks$Year
#> [1] 1789 1793 1797 1801 1805
toks$Year <- 1991:1995
toks$Year
#> [1] 1991 1992 1993 1994 1995
toks$nonexistent <- TRUE
docvars(toks)
#>   Year  President FirstName                 Party century nonexistent
#> 1 1991 Washington    George                  none      17        TRUE
#> 2 1992 Washington    George                  none      17        TRUE
#> 3 1993      Adams      John            Federalist      17        TRUE
#> 4 1994  Jefferson    Thomas Democratic-Republican      18        TRUE
#> 5 1995  Jefferson    Thomas Democratic-Republican      18        TRUE

# accessing or assigning docvars for a dfm using "$"
dfmat <- dfm(toks)
dfmat$Year
#> [1] 1991 1992 1993 1994 1995
dfmat$Year <- 1991:1995
dfmat$Year
#> [1] 1991 1992 1993 1994 1995
dfmat$nonexistent <- TRUE
docvars(dfmat)
#>   Year  President FirstName                 Party century nonexistent
#> 1 1991 Washington    George                  none      17        TRUE
#> 2 1992 Washington    George                  none      17        TRUE
#> 3 1993      Adams      John            Federalist      17        TRUE
#> 4 1994  Jefferson    Thomas Democratic-Republican      18        TRUE
#> 5 1995  Jefferson    Thomas Democratic-Republican      18        TRUE
```
