# @rdname catm
# make temporary files and directories in a more reasonable way than tempfile()
# or tempdir(): here, the filename is different each time you call mktemp()
mktemp <- function(prefix='tmp.', base_path=NULL, directory=F) {
    #  Create a randomly-named temporary file or directory, sort of like
    #  https://www.mktemp.org/manual.html
    if (is.null(base_path))
        base_path <- tempdir()

    alphanumeric <- c(0:9, LETTERS, letters)

    filename <- paste0(sample(alphanumeric, 10, replace=T), collapse='')
    filename <- paste0(prefix, filename)
    filename <- file.path(base_path, filename)
    while (file.exists(filename) || dir.exists(filename)) {
        filename <- paste0(sample(alphanumeric, 10, replace=T), collapse='')
        filename <- paste0(prefix, filename)
        filename <- file.path(base_path, filename)
    }

    if (directory) {
        dir.create(filename)
    }
    else {
        file.create(filename)
    }

    return(filename)
}


# @rdname catm
# messages() with some of the same syntax as cat(): takes a sep argument and
# does not append a newline by default
catm <- function(..., sep = " ", appendLF = F) {
    message(paste(..., sep = sep), appendLF = appendLF)
}

##
## reassign the slots to an S4 dfm-like object
## necessary when some operation from the Matrix class obliterates them
## Ken B
reassign_slots <- function(x_new, x_orig) {
    snames <- slotNames(x_orig)
    snames <- snames[!snames %in% 
                         c("Dim", "Dimnames", "i", "p", "x", "factors")]
    for (s in snames) {
        slot(x_new, s) <- slot(x_orig, s)
    }
    x_new
}


