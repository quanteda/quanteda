# Internal function to get, set or initialize system metadata

Sets or initializes system metadata for new objects.

## Usage

``` r
meta_system(x, field = NULL)

meta_system(x, field = NULL) <- value

# S3 method for class 'corpus'
meta_system(x, field = NULL) <- value

# S3 method for class 'tokens'
meta_system(x, field = NULL) <- value

# S3 method for class 'dfm'
meta_system(x, field = NULL) <- value

# S3 method for class 'dictionary'
meta_system(x, field = NULL) <- value

meta_system_defaults()
```

## Arguments

- x:

  an object for which the metadata will be read or set

- field:

  metadata field name(s); if `NULL` (default), return all metadata names

- value:

  new value of the metadata field

## Value

`meta_system` returns a list with the object's system metadata. It is
literally a wrapper to
[`meta(x, field, type = "system")()`](https://quanteda.io/reference/meta.md).

`meta_system<-` returns the object with the system metadata modified.
This is an internal function and not designed for users!

`meta_system_defaults` returns a list of default system values, with the
user setting the "source" value. This should be used to set initial
system meta information.

## Examples

``` r
corp <- corpus(c(d1 = "one two three", d2 = "two three four"))
# quanteda:::`meta_system<-`(corp, value = quanteda:::meta_system_defaults("example"))
quanteda:::meta_system(corp)
#> $`package-version`
#> [1] '4.4.1'
#> 
#> $`r-version`
#> [1] '4.6.1'
#> 
#> $system
#>   sysname   machine      user 
#> "Windows"  "x86-64"   "watan" 
#> 
#> $directory
#> [1] "C:/Users/watan/Repo/quanteda/docs/reference"
#> 
#> $created
#> [1] "2026-07-22"
#> 
```
