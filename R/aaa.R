#' @include quanteda-documentation.R
#' @include meta.R

# package-level global environment
global <- new.env(parent = emptyenv())

# v4 tokenizer rules
global$breakrules_word <-
    c(list(base = paste0(readLines(system.file("breakrules/word.txt", package = "quanteda")),
                         collapse = "\n")),
      yaml::read_yaml(system.file("breakrules/custom.yml", package = "quanteda")))
global$breakrules_sentence <-
    list(base = paste0(readLines(system.file("breakrules/sent.txt", package = "quanteda")),
                         collapse = "\n"))
