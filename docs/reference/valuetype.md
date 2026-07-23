# Pattern matching using valuetype

Pattern matching in quanteda using the `valuetype` argument.

## Arguments

- valuetype:

  the type of pattern matching: `"glob"` for "glob"-style wildcard
  expressions; `"regex"` for regular expressions; or `"fixed"` for exact
  matching. See valuetype for details.

- case_insensitive:

  logical; if `TRUE`, ignore case when matching a `pattern` or
  [dictionary](https://quanteda.io/reference/dictionary.md) values

## Details

Pattern matching in in quanteda uses "glob"-style pattern matching as
the default, because this is simpler than regular expression matching
while addressing most users' needs. It is also has the advantage of
being identical to fixed pattern matching when the wildcard characters
(`*` and `?`) are not used. Finally, most
[dictionary](https://quanteda.io/reference/dictionary.md) formats use
glob matching.

- `"glob"`:

  "glob"-style wildcard expressions, the quanteda default. The
  implementation used in quanteda uses `*` to match any number of any
  characters including none, and `?` to match any single character. See
  also [`utils::glob2rx()`](https://rdrr.io/r/utils/glob2rx.html) and
  References below.

- `"regex"`:

  Regular expression matching.

- `"fixed"`:

  Fixed (literal) pattern matching.

## Note

If "fixed" is used with `case_insensitive = TRUE`, features will
typically be lowercased internally prior to matching. Also, glob matches
are converted to regular expressions (using
[`utils::glob2rx()`](https://rdrr.io/r/utils/glob2rx.html)) when they
contain wild card characters, and to fixed pattern matches when they do
not.

## See also

[`utils::glob2rx()`](https://rdrr.io/r/utils/glob2rx.html), [glob
pattern matching
(Wikipedia)](https://en.wikipedia.org/wiki/Glob_(programming)),
[`stringi::stringi-search-regex()`](https://rdrr.io/pkg/stringi/man/about_search_regex.html),
[`stringi::stringi-search-fixed()`](https://rdrr.io/pkg/stringi/man/about_search_fixed.html)
