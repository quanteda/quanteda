# quanteda 2.0

## Changes

**quanteda** 2.0 introduces some major changes, detailed here.

1.  New corpus object structure.  

    The internals of the corpus object have been redesigned, and now are based around a character vector with meta- and system-data in attributes.  These are all updated to work with the existing extractor and replacement functions.  If you were using these before, then you should not even notice the change.  Docvars are now handled separately from the texts, in the same way that  docvars are handled for tokens objects.
    
2.  New metadata handling.

    Corpus-level metadata is now inserted in a user metadata list via `meta()` and `meta<-()`.  `metacorpus()` is kept as a synonym for `meta()`, for backwards compatibility.  Additional system-level corpus information is also recorded, but automatically when an object is created.  
    
    Document-level metadata is deprecated, and now all document-level information is simply a "docvar".  For backward compatibility, `metadoc()` is kept and will insert document variables (docvars) with the name prefixed by an underscore.
    
3.  Redesigned index operators for core objects.

    TABLE HERE
    
4.  `*_subset()` functions.  

     The `subset` argument now must be logical, and the `select` argument has been removed.  (This is part of `base::subset()` but has never made sense, either in **quanteda** or **base**!)

5.  Return format from `textstat_simil()` and `textstat_dist()`.

    Now defaults to a data.frame of pairwise similarities or distances, making these functions return a data.frame just like the other textstat functions.  Coercion methods are provided for `as.dist()`, `as.simil()`, `as.matrix()`, and `as.Matrix()` (producing a ?? sparse symmetric matrix).
    
6.  settings functions (and related slots and object attributes) are gone.

7.  All included data objects are upgraded to the new formats.  This includes the three corpus objects and the single dfm data object.
    
## Bug fixes and stability enhancements

*  None so far.

## New features

* Changed the default value of the `size` argument in `dfm_sample()` to the number of features, not the number of documents.  (#1643)
* Fixes a few CRAN-related issues (compiler warnings on Solaris and encoding warnings on r-devel-linux-x86_64-debian-clang.)

## Behaviour changes

* Added a `force = TRUE` option and error checking for the situations of applying `dfm_weight()` or `dfm_group()` to a dfm that has already been weighted.  (#1545)  The function `textstat_frequency()` now allows passing this argument to `dfm_group()` via `...`.  (#1646)
* `textstat_frequency()` now has a new argument for resolving ties when ranking term frequencies, defaulting to the "min" method.  (#1634)