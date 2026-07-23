# format a sparsity value for printing

Inputs a dfm sparsity value from
[`sparsity()`](https://quanteda.io/reference/sparsity.md) and formats it
for printing in
[`print.dfm()`](https://quanteda.io/reference/print-methods.md).

## Usage

``` r
format_sparsity(x)
```

## Arguments

- x:

  input sparsity value, ranging from 0 to 1.0

## Examples

``` r
ss <- c(1, .99999, .9999, .999, .99, .9,
       .1, .01, .001, .0001, .000001, .0000001, .00000001, .000000000001, 0)
for (s in ss) 
    cat(format(s, width = 10),  ":", quanteda:::format_sparsity(s), "\n")
#>          1 : 100.00% 
#>    0.99999 : >99.99% 
#>     0.9999 : 99.99% 
#>      0.999 : 99.90% 
#>       0.99 : 99.00% 
#>        0.9 : 90.00% 
#>        0.1 : 10.00% 
#>       0.01 : 1.00% 
#>      0.001 : 0.10% 
#>      1e-04 : 0.01% 
#>      1e-06 : <0.01% 
#>      1e-07 : <0.01% 
#>      1e-08 : <0.01% 
#>      1e-12 : <0.01% 
#>          0 : 0.00% 
```
