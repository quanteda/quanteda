#' @include quanteda-documentation.R
#' @include meta.R
#' @include tokenizers.R

# package-level global environment
global <- new.env(parent = emptyenv())

# v4 tokenizer rules
breakrules_reset("word")
breakrules_reset("sentence")

