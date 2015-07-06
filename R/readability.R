

#' calculate readability
#' 
#' Calculate the readability of text(s).
#' @export
readability <- function(x, ...) {
    UseMethod("readability")
}


#' @rdname readability
#' @param x a \link{corpus} object
#' @param index character vector defining the readability index to calculate
#' @author Kenneth Benoit, adapted from the S4 class implementation written by 
#'   Meik Michalke in the \pkg{koRpus} package.
#' @export
#' @examples 
#' readability(c("ABC.", "The fat, cat."), index = "Flesch.Kincaid")
readability.character <- function(x, index) {
    
    readabilitySingle <- function(x, index) {
        feat_sentences <- nsentence(x)
        
        tokenizedWords <- tokenize(toLower(x), removePunct = TRUE, simplify = TRUE) 
        feat_words <- length(tokenizedWords)
    
        # would be faster to take these from table() but then need to account for non-occuring frequencies
        tokenizedWordsLetters <- stringi::stri_length(tokenizedWords)
        feat_letters <- c(all = sum(tokenizedWordsLetters), 
                          l1 = sum(tokenizedWordsLetters == 1),
                          l2 = sum(tokenizedWordsLetters == 2),
                          l3 = sum(tokenizedWordsLetters == 3),
                          l4 = sum(tokenizedWordsLetters == 4),
                          l5 = sum(tokenizedWordsLetters == 5),
                          l6 = sum(tokenizedWordsLetters == 6))
        
    
        tmp_syll <- syllables(tokenizedWords)
        feat_syll <- c(all = sum(tmp_syll), 
                       s1 = sum(tmp_syll == 1),
                       s2 = sum(tmp_syll == 2),
                       s3 = sum(tmp_syll == 3))
        
        feat_punct <- stringi::stri_count_charclass(x, "[\\p{P}\\p{S}]")
        
        feat_all.chars <- stringi::stri_length(x)
        
        feat_TTR <- lexdiv(dfm(x, verbose = FALSE))

        feat_Bormuth.NOL <- feat_Dale.Chall.NOL <- sum(!(tokenizedWords %in% wordlists$dalechall))
        feat_Spache.NOL <- sum(!(tokenizedWords %in% wordlists$spache))
        
        koRpusReadability(list(sentences = feat_sentences,
                               words = feat_words,
                               letters = feat_letters,
                               syllables = feat_syll,
                               punct = feat_punct,
                               all.chars = feat_all.chars,
                               prepositions = NULL,
                               conjunctions = NULL,
                               pronouns = NULL,
                               foreign = NULL,
                               TTR = feat_TTR,
                               Bormuth.NOL = feat_Bormuth.NOL,
                               Dale.Chall.NOL = feat_Dale.Chall.NOL,
                               Harris.Jacobson.NOL = NULL,
                               Spache.NOL = feat_Spache.NOL), 
                          index = index)
    }

    lapply(x, readabilitySingle, index)
}


koRpusReadability <- function(txt.features, index) {
    if (!requireNamespace("koRpus", quietly = TRUE))
        stop("You must have koRpus installed to compute readability indexes.")
    koRpus::readability.num(txt.features, index)
}


