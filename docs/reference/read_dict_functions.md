# Internal functions to import dictionary files

Internal functions to import dictionary files in a variety of formats

`read_dict_lexicoder` imports Lexicoder files in the `.lc3` format.

`read_dict_wordstat` imports WordStat files in the `.cat` format.

`read_dict_liwc` imports LIWC dictionary files in the `.dic` format.

`read_dict_yoshikoder` imports Yoshikoder files in the `.ykd` format.

## Usage

``` r
read_dict_lexicoder(path)

read_dict_wordstat(path, encoding = "utf-8")

read_dict_liwc(path, encoding = "utf-8")

read_dict_yoshikoder(path)
```

## Arguments

- path:

  the full path and filename of the dictionary file to be read

- encoding:

  the encoding of the file to be imported

## Value

a quanteda [dictionary](https://quanteda.io/reference/dictionary.md)
object

## Examples

``` r
dict <- quanteda:::read_dict_lexicoder(
    system.file("extdata", "LSD2015.lc3", package = "quanteda")
)


if (FALSE) { # \dontrun{
dict <- quanteda:::read_dict_wordstat(system.file("extdata", "RID.cat", package = "quanteda"))
# dict <- read_dict_wordstat("/home/kohei/Documents/Dictionary/LaverGarry.txt", "utf-8")
# dict <- read_dict_wordstat("/home/kohei/Documents/Dictionary/Wordstat/ROGET.cat", "utf-8")
# dict <- read_dict_wordstat("/home/kohei/Documents/Dictionary/Wordstat/WordStat Sentiments.cat",
#                            encoding = "iso-8859-1")
} # }

dict <- quanteda:::read_dict_liwc(
    system.file("extdata", "moral_foundations_dictionary.dic", package = "quanteda")
)

dict <- quanteda:::read_dict_yoshikoder(system.file("extdata", "laver_garry.ykd",
                                                    package = "quanteda"))
```
