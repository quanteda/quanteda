.onAttach <- function(...) {
    packageStartupMessage("quanteda version ", as.character(utils::packageVersion("quanteda")), "\n")
}
