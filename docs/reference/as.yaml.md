# Convert quanteda dictionary objects to the YAML format

Converts a quanteda dictionary object constructed by the
[dictionary](https://quanteda.io/reference/dictionary.md) function into
the YAML format. The YAML files can be edited in text editors and
imported into quanteda again.

## Usage

``` r
as.yaml(x)
```

## Arguments

- x:

  a [dictionary](https://quanteda.io/reference/dictionary.md) object

## Value

`as.yaml` a dictionary in the YAML format, as a character object

## Examples

``` r
if (FALSE) { # \dontrun{
dict <- dictionary(list(one = c("a b", "c*"), two = c("x", "y", "z??")))
cat(yaml <- as.yaml(dict))
cat(yaml, file = (yamlfile <- paste0(tempfile(), ".yml")))
dictionary(file = yamlfile)
} # }
```
