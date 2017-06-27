#' pattern matching using valuetype
#' 
#' Pattern matching in \pkg{quanteda} using the \code{valuetype} argument.
#' @param valuetype how to interpret keyword expressions: \code{"glob"} for 
#'   "glob"-style wildcard expressions; \code{"regex"} for regular expressions;
#'   or \code{"fixed"} for exact matching. See \link{valuetype} for details.
#' @details Pattern matching in in \pkg{quanteda} uses "glob"-style pattern
#'   matching as the default, because this is simpler than regular expression
#'   matching while addressing most users' needs.  It is also has the advantage
#'   of being identical to fixed pattern matching when the wildcard characters
#'   (`*` and `?`) are not used. Finally, most \link{dictionary} formats use
#'   glob matching.
#'   
#'   \describe{
#'   \item{\code{"glob"}}{"glob"-style wildcard expressions, the quanteda default.  
#'     The implementation used in \pkg{quanteda} uses `*` to match any number of any 
#'     characters including none, and `?` to match any single character.  See also 
#'   \code{\link[utils]{glob2rx}} and References below.}
#'   \item{\code{"regex"}}{Regular expression matching.}
#'   \item{\code{"fixed"}}{Fixed (literal) pattern matching.}
#'   } 
#' @note If "fixed" is used with \code{case_insensitive = TRUE}, features will 
#'   typically be lowercased internally prior to matching.  Also, glob matches
#'   are converted to regular expressions (using \link[utils]{glob2rx}) when
#'   they contain wild card characters, and to fixed pattern matches when they
#'   do not.
#' @name valuetype
#' @seealso \code{\link[utils]{glob2rx}}, 
#' \href{https://en.wikipedia.org/wiki/Glob_(programming)}{glob pattern matching (Wikipedia)}, 
#' \link{regex}
#' @keywords internal
NULL
