# Benchmark Guidelines

## Purpose of the `benchmarks/` directory

This is a where benchmarks and comparisons should exist, for comparing the performance of different methods of doing the same thing to one another in terms of performance.  

It is also a good place to put comparisons of **quanteda** functions to similar functions from other packages, either to compare performance or behaviour.

Because the `benchmarks/` directory (and its sub-directories) are specified in `.Rbuildignore`, none of them will be used when the package is built.  So: Nothing in this directory will affect the package, and it can be used for tests and comparisons only.

## Format

All benchmark files should be be in RMarkdown (.Rmd) format and compiled to .html (using **knitr**) prior to being pushed.

## Guidelines

### Naming your file

Please use one of the two filename conventions:

*  `benchmark_SOMETHING.Rmd` - for benchmarks to compare one thing versus another  
*  `comparison_SOMETHING.Rmd` - for comparing behaviours, e.g. for comparing the behaviour of **quanteda**'s `tokenize()` function to those in the **tokenizers** package.

### Recommended benchmarking tools

*  `microbenchmark::microbenchmark()`  
*  `rbenchmark::benchmark()`

Recommended that you use `units = "relative"` for more direct comparison than the actual times.

### Where to put test data

In the `benchmarks/data/` folder.

