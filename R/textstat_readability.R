#' Calculate readability
#'
#' Calculate the readability of text(s) using one of a variety of computed 
#' indexes.
#' @param x a character or \link{corpus} object containing the texts
#' @param measure character vector defining the readability measure to calculate.  
#'   Matches are case-insensitive.
#' @param remove_hyphens if \code{TRUE}, treat constituent words in hyphenated as
#'   separate terms, for purposes of computing word lengths, e.g.
#'   "decision-making" as two terms of lengths 8 and 6 characters respectively,
#'   rather than as a single word of 15 characters
#' @param min_sentence_length,max_sentence_length set the minimum and maximum 
#'   sentence lengths (in tokens, excluding punctuation) to include in the
#'   computation of readability.  This makes it easy to exclude "sentences" that
#'   may not really be sentences, such as section titles, table elements, and 
#'   other cruft that might be in the texts following conversion.
#'   
#'   For finer-grained control, consider filtering sentences prior first, 
#'   including through pattern-matching, using \code{\link{corpus_trim}}.
#' @param intermediate if \code{TRUE}, include intermediate quantities in the output
#' @param ... not used
#' @author Kenneth Benoit, re-engineered from Meik Michalke's \pkg{koRpus}
#'   package.
#' @return \code{textstat_readability} returns a data.frame of documents and
#'   their readability scores.
#' @export
#' @examples
#' txt <- c(doc1 = "Readability zero one.  Ten, Eleven.", 
#'          doc2 = "The cat in a dilapidated tophat.")
#' textstat_readability(txt, "Flesch")
#' textstat_readability(txt, c("FOG", "FOG.PSK", "FOG.NRI"))
#' 
#' textstat_readability(data_corpus_inaugural[48:58], 
#'                      measure = c("Flesch.Kincaid", "Dale.Chall.old"))
textstat_readability <- function(x,
                        measure = c("all", "ARI", "ARI.simple", "Bormuth", "Bormuth.GP",
                                    "Coleman", "Coleman.C2",
                                    "Coleman.Liau", "Coleman.Liau.grade", "Coleman.Liau.short",
                                    "Dale.Chall", "Dale.Chall.old", "Dale.Chall.PSK",
                                    "Danielson.Bryan", "Danielson.Bryan.2",
                                    "Dickes.Steiwer", "DRP", "ELF", "Farr.Jenkins.Paterson",
                                    "Flesch", "Flesch.PSK", "Flesch.Kincaid",
                                    "FOG", "FOG.PSK", "FOG.NRI", "FORCAST", "FORCAST.RGL",
                                    "Fucks", "Linsear.Write", "LIW",
                                    "nWS", "nWS.2", "nWS.3", "nWS.4", "RIX", "Scrabble",
                                    "SMOG", "SMOG.C", "SMOG.simple", "SMOG.de",
                                    "Spache", "Spache.old", "Strain",
                                    "Traenkle.Bailer", "Traenkle.Bailer.2",
                                    "Wheeler.Smith", "meanSentenceLength", "meanWordSyllables"),
                        remove_hyphens = TRUE,
                        min_sentence_length = 1, 
                        max_sentence_length = 10000, 
                        intermediate = FALSE, ...) {
    UseMethod("textstat_readability")
}

#' @export
textstat_readability.default <- function(x,
                                        measure = c("all", "ARI", "ARI.simple", "Bormuth", "Bormuth.GP",
                                                    "Coleman", "Coleman.C2",
                                                    "Coleman.Liau", "Coleman.Liau.grade", "Coleman.Liau.short",
                                                    "Dale.Chall", "Dale.Chall.old", "Dale.Chall.PSK",
                                                    "Danielson.Bryan", "Danielson.Bryan.2",
                                                    "Dickes.Steiwer", "DRP", "ELF", "Farr.Jenkins.Paterson",
                                                    "Flesch", "Flesch.PSK", "Flesch.Kincaid",
                                                    "FOG", "FOG.PSK", "FOG.NRI", "FORCAST", "FORCAST.RGL",
                                                    "Fucks", "Linsear.Write", "LIW",
                                                    "nWS", "nWS.2", "nWS.3", "nWS.4", "RIX", "Scrabble",
                                                    "SMOG", "SMOG.C", "SMOG.simple", "SMOG.de",
                                                    "Spache", "Spache.old", "Strain",
                                                    "Traenkle.Bailer", "Traenkle.Bailer.2",
                                                    "Wheeler.Smith", "meanSentenceLength", "meanWordSyllables"),
                                        remove_hyphens = TRUE,
                                        min_sentence_length = 1, 
                                        max_sentence_length = 10000, 
                                        intermediate = FALSE, ...) {
    stop(friendly_class_undefined_message(class(x), "textstat_readability"))
}    

#' @importFrom stringi stri_length
#' @export
textstat_readability.corpus <- function(x,
                                        measure = c("all", "ARI", "ARI.simple", "Bormuth", "Bormuth.GP",
                                                    "Coleman", "Coleman.C2",
                                                    "Coleman.Liau", "Coleman.Liau.grade", "Coleman.Liau.short",
                                                    "Dale.Chall", "Dale.Chall.old", "Dale.Chall.PSK",
                                                    "Danielson.Bryan", "Danielson.Bryan.2",
                                                    "Dickes.Steiwer", "DRP", "ELF", "Farr.Jenkins.Paterson",
                                                    "Flesch", "Flesch.PSK", "Flesch.Kincaid",
                                                    "FOG", "FOG.PSK", "FOG.NRI", "FORCAST", "FORCAST.RGL",
                                                    "Fucks", "Linsear.Write", "LIW",
                                                    "nWS", "nWS.2", "nWS.3", "nWS.4", "RIX", "Scrabble",
                                           "SMOG", "SMOG.C", "SMOG.simple", "SMOG.de",
                                           "Spache", "Spache.old", "Strain",
                                           "Traenkle.Bailer", "Traenkle.Bailer.2",
                                           "Wheeler.Smith", "meanSentenceLength", "meanWordSyllables"),
                               remove_hyphens = TRUE,
                               min_sentence_length = 1, 
                               max_sentence_length = 10000, 
                               intermediate = FALSE, ...) {
    
    unused_dots(...)
    
    measure_option <- c("ARI", "ARI.simple", "Bormuth", "Bormuth.GP",
                        "Coleman", "Coleman.C2",
                        "Coleman.Liau", "Coleman.Liau.grade", "Coleman.Liau.short",
                        "Dale.Chall", "Dale.Chall.old", "Dale.Chall.PSK",
                        "Danielson.Bryan", "Danielson.Bryan.2",
                        "Dickes.Steiwer", "DRP", "ELF", "Farr.Jenkins.Paterson",
                        "Flesch", "Flesch.PSK", "Flesch.Kincaid",
                        "FOG", "FOG.PSK", "FOG.NRI", "FORCAST", "FORCAST.RGL",
                        "Fucks", "Linsear.Write", "LIW",
                        "nWS", "nWS.2", "nWS.3", "nWS.4", "RIX",
                        "Scrabble",
                        "SMOG", "SMOG.C", "SMOG.simple", "SMOG.de",
                        "Spache", "Spache.old", "Strain",
                        "Traenkle.Bailer", "Traenkle.Bailer.2",
                        "Wheeler.Smith",
                        "meanSentenceLength",
                        "meanWordSyllables")
    
    if (measure[1] == 'all') {
        measure <- measure_option
    } else {
        is_valid <- measure %in% measure_option
        if (!all(is_valid))
            stop("Invalid measure(s): ", measure[!is_valid])
    }
    
    x <- texts(x)
    if (!is.null(min_sentence_length) || !is.null(max_sentence_length)) {
        x <- char_trim(x, 'sentences',
                       min_ntoken = min_sentence_length,
                       max_ntoken = max_sentence_length)
    }
    
    # get sentence lengths - BEFORE lower-casing
    n_sent <- nsentence(x)
    
    # get the word length and syllable info for use in computing quantities
    x <- char_tolower(x)
    toks <- tokens(x, remove_punct = TRUE, remove_hyphens = remove_hyphens)
    
    # number of syllables
    n_syll <- nsyllable(toks)
    # replace any NAs with a single count (most of these will be numbers)
    n_syll <- lapply(n_syll, function(y) ifelse(is.na(y), 1, y))
    
    # lengths in characters of the words
    len_token <- lapply(toks, stringi::stri_length)
    
    # to avoid "no visible binding for global variable" CHECK NOTE
    textID <- W <- St <- C <- Sy <- W3Sy <- W2Sy <- W_1Sy <- W6C <- W7C <- Wlt3Sy <- W_wl.Dale.Chall <-
        W_wl.Spache <- ARI <- ARI.NRI <- ARI.simple <- Bormuth.GP <- Coleman <- Coleman.C2 <-
        Coleman.Liau.ECP <- Coleman.Liau.grade <- Coleman.Liau.short <- Dale.Chall <- Dale.Chall.old <-
        Dale.Chall.PSK <- Danielson.Bryan <- Danielson.Bryan.2 <- Dickes.Steiwer <- DRP <- ELF <-
        Farr.Jenkins.Paterson <- Flesch <- Flesch.PSK <- Flesch.Kincaid <- FOG <- FOG.PSK <- FOG.NRI <-
        FORCAST <- FORCAST.RGL <- Fucks <- Linsear.Write <- LIW <- nWS <- nWS.2 <- nWS.3 <- nWS.4 <-
        RIX <- SMOG <- SMOG.C <- SMOG.simple <- SMOG.de <- Spache <- Spache.old <- Strain <- Wheeler.Smith <-
        Bormuth.MC <- Bl <- Traenkle.Bailer <- Traenkle.Bailer.2 <- Bormuth <-
        Coleman.Liau <- meanSentenceLength <- meanWordSyllables <- NULL
    
    # common statistics required by (nearly all) indexes
    temp <- data.table(textID = names(x),
                       W = lengths(toks),  # number of words
                       St = n_sent,            # number of sentences
                       C =  vapply(len_token, sum, numeric(1)), # number of characters (letters)
                       Sy = vapply(n_syll, sum, numeric(1)),    # number of syllables
                       W3Sy =  vapply(n_syll, function(x) sum(x >= 3), numeric(1)),  # number words with >= 3 syllables
                       W2Sy =  vapply(n_syll, function(x) sum(x >= 2), numeric(1)),  # number words with >= 2 syllables
                       W_1Sy = vapply(n_syll, function(x) sum(x == 1), numeric(1)),  # number words with 1 syllable
                       W6C = vapply(len_token, function(x) sum(x >= 6), numeric(1)), # number of words with at least 6 letters
                       W7C = vapply(len_token, function(x) sum(x >= 7), numeric(1))) # number of words with at least 7 letters
    
    temp[, Wlt3Sy := W - W3Sy]   # number of words with less than three syllables
    
    # look up D-C words if needed
    if (any(c("Dale.Chall", "Dale.Chall.old", "Dale.Chall.PSK", "Bormuth", "Bormuth.GP") %in% measure)) {
        temp[, W_wl.Dale.Chall := lengths(tokens_remove(toks, 
                                                        pattern = quanteda::data_char_wordlists$dalechall,
                                                        valuetype = "fixed", 
                                                        case_insensitive = TRUE))]
    }
    
    if ("ARI" %in% measure)
        temp[, ARI := 0.5 * W / St + 4.71 * C / W - 21.43]
    
    if ("ARI.NRI" %in% measure)
        temp[, ARI.NRI := 0.4 * W / St + 6 * C / W - 27.4]
    
    if ("ARI.simple" %in% measure)
        temp[, ARI.simple := W / St + 9 * C / W]
    
    if ("Bormuth" %in% measure) {
        temp[, Bormuth := 0.886593 - (0.08364 * C/W) + 0.161911 *
                         (W_wl.Dale.Chall / W) ^ 3 - 0.21401 * (W/St) + 0.000577 * (W/St) ^ 2 - 0.000005 * (W/St) ^ 3]
    }
    if ("Bormuth.GP" %in% measure) {
        CCS <- 35 # Cloze criterion score, percent as integer
        temp[, Bormuth.MC := 0.886593 - (0.08364 * C/W) + 0.161911 *
                         (W_wl.Dale.Chall / W) ^ 3 - 0.21401 * (W/St) + 0.000577 * (W/St) ^ 2 - 0.000005 * (W/St) ^ 3]
        temp[, Bormuth.GP := 4.275 + 12.881 * Bormuth.MC - (34.934 * Bormuth.MC^2) + (20.388 * Bormuth.MC^3) +
                         (26.194 * C - 2.046 * CCS ^ 2) - (11.767 * CCS ^ 3) - (44.285 * Bormuth.MC * CCS) +
                         (97.620 * (Bormuth.MC * CCS)^2) - (59.538 * (Bormuth.MC * CCS)^3)]
        temp[, Bormuth.MC := NULL]
    }
    
    if ("Coleman" %in% measure)
        temp[, Coleman := 1.29 * (100 * W_1Sy / W) - 38.45]
    
    if ("Coleman.C2" %in% measure)
        temp[, Coleman.C2 := 1.16 * (100 * W_1Sy / W) + 1.48 * (100 * St / W) - 37.95]
    
    ## cannot compute Coleman.C3, Coleman.C4 without knowing the number of pronouns or prepositions
    
    if ("Coleman.Liau" %in% measure)
        temp[, Coleman.Liau   := 141.8401 - 0.214590 * (100 * C / W) + 1.079812 * (100 * St / W)]
    
    if ("Coleman.Liau.grade" %in% measure) {
        temp[, Coleman.Liau.ECP   := 141.8401 - 0.214590 * (100 * C / W) + 1.079812 * (100 * St / W)]
        temp[, Coleman.Liau.grade := -27.4004 * Coleman.Liau.ECP / 100 + 23.06395]
        temp[, Coleman.Liau.ECP   := NULL]
    }
    
    if ("Coleman.Liau.short" %in% measure)
        temp[, Coleman.Liau.short := 5.88 * C / W - 29.6 * St / W - 15.8]
    
    if ("Dale.Chall" %in% measure) {
        temp[, Dale.Chall := 64 - 0.95 * 100 * W_wl.Dale.Chall / W - 0.69 * W / St]
    }
    
    if ("Dale.Chall.old" %in% measure) {
        DC_constant <- NULL
        temp[, DC_constant := ((W_wl.Dale.Chall / W) > .05) * 3.6365]
        temp[, Dale.Chall.old := 0.1579 * 100 * W_wl.Dale.Chall / W + 0.0496 * W / St + DC_constant]
        temp[, DC_constant := NULL]
    }
    
    # Powers-Sumner-Kearl (1958) variation
    if ("Dale.Chall.PSK" %in% measure)
        temp[, Dale.Chall.PSK := 0.1155 * 100 * W_wl.Dale.Chall / W + 0.0596 * W / St + 3.2672]
    
    if ("Danielson.Bryan" %in% measure) {
        temp[, Bl := W - 1]  # could be more accurate if count spaces
        temp[, Danielson.Bryan := (1.0364 * C / Bl) + (0.0194 * C / St) - 0.6059]
        temp[, Bl := NULL]
    }
    
    if ("Danielson.Bryan.2" %in% measure) {
        temp[, Bl := W - 1]  # could be more accurate if count spaces
        temp[, Danielson.Bryan.2 := 131.059 - (10.364 * C / Bl) + (0.0194 * C / St)]
        temp[, Bl := NULL]
    }
    
    if ("Dickes.Steiwer" %in% measure) {
        TTR <- textstat_lexdiv(dfm(x, verbose = FALSE), measure = "TTR")$TTR
        temp[, Dickes.Steiwer := 235.95993 - (73.021 * C / W) - (12.56438 * W / St) - (50.03293 * TTR)]
    }
    
    if ("DRP" %in% measure) {
        temp[, Bormuth.MC := 0.886593 - (0.08364 * C/W) + 0.161911 *
                         (W_wl.Dale.Chall / W) ^ 3 - 0.21401 * (W/St) + 0.000577 * (W/St) ^ 2 - 0.000005 * (W/St) ^ 3]
        temp[, DRP := (1 - Bormuth.MC) * 100]
        temp[, Bormuth.MC := NULL]
    }
    
    if ("ELF" %in% measure)
        temp[, ELF := W2Sy / St]
    
    if ("Farr.Jenkins.Paterson" %in% measure)
        temp[, Farr.Jenkins.Paterson := -31.517 - 1.015 * W / St + 1.599 * W_1Sy / W]
    
    if ("Flesch" %in% measure)
        temp[, Flesch := 206.835 - 1.015 * W / St - 84.6 * Sy / W ]
    
    if ("Flesch.PSK" %in% measure)
        temp[, Flesch.PSK := 0.0778 * W / St + 4.55 * Sy / W - 2.2029]
    
    if ("Flesch.Kincaid" %in% measure)
        temp[, Flesch.Kincaid := 0.39 * W / St + 11.8 * Sy / W - 15.59]
    
    if ("meanSentenceLength" %in% measure)
        temp[, meanSentenceLength := W / St]
    
    if ("meanWordSyllables" %in% measure)
        temp[, meanWordSyllables := Sy / W]
    
    if ("FOG" %in% measure)
        temp[, FOG := 0.4 * ( W / St + 100 * W3Sy / W )]
    # If the text was POS-tagged accordingly, proper nouns and combinations of only easy words
    # will not be counted as hard words, and the syllables of verbs ending in "-ed", "-es" or
    # "-ing" will be counted without these suffixes.
    
    if ("FOG.PSK" %in% measure)
        temp[, FOG.PSK := 3.0680 * ( 0.0877 * W / St ) + (0.0984 * 100 * W3Sy / W )]
    
    if ("FOG.NRI" %in% measure)
        temp[, FOG.NRI := ((( Wlt3Sy + 3 * W3Sy ) / (100 * St / W)) - 3) / 2]
    
    if ("FORCAST" %in% measure)
        temp[, FORCAST := 20 - (W_1Sy * 150 / W) / 10]
    
    if ("FORCAST.RGL" %in% measure)
        temp[, FORCAST.RGL := 20.43 - 0.11 * W_1Sy * 150 / W]
    
    if ("Fucks" %in% measure)
        temp[, Fucks := C / W * W / St]
    
    if ("Linsear.Write" %in% measure)
        temp[, Linsear.Write := ((100 - (100 * Wlt3Sy)/W) + (3 * 100 * W3Sy / W)) / (100 * St / W)]
    
    if ("LIW" %in% measure)
        temp[, LIW := (W / St) + (100 * W7C) / W]
    
    if ("nWS" %in% measure)
        temp[, nWS := 19.35 * W3Sy / W + 0.1672 * W / St + 12.97 * W6C / W - 3.27 * W_1Sy / W - 0.875]
    
    if ("nWS.2" %in% measure)
        temp[, nWS.2 := 20.07 * W3Sy / W + 0.1682 * W / St + 13.73 * W6C / W - 2.779]
    
    if ("nWS.3" %in% measure)
        temp[, nWS.3 := 29.63 * W3Sy / W + 0.1905 * W / St - 1.1144]
    
    if ("nWS.4" %in% measure)
        temp[, nWS.4 := 27.44 * W3Sy / W + 0.2656 * W / St - 1.693]
    
    if ("RIX" %in% measure)
        temp[, RIX := W7C / St]
    
    if ("SMOG" %in% measure)
        temp[, SMOG := 1.043 * sqrt(W3Sy * 30 / St) + 3.1291]
    
    if ("SMOG.C" %in% measure)
        temp[, SMOG.C := 0.9986 * sqrt(W3Sy * 30 / St + 5) + 2.8795]
    
    if ("SMOG.simple" %in% measure)
        temp[, SMOG.simple := sqrt(W3Sy * 30 / St) + 3]
    
    if ("SMOG.de" %in% measure)
        temp[, SMOG.de := sqrt(W3Sy * 30 / St) - 2]
    
    if (any(c("Spache", "Spache.old") %in% measure)) {
        # number of words which are not in the Spache word list
        temp[, W_wl.Spache := lengths(tokens_remove(toks, 
                                                    pattern = quanteda::data_char_wordlists$spache,
                                                    valuetype = "fixed", 
                                                    case_insensitive = TRUE))]
    }
    
    if ("Spache" %in% measure)
        temp[, Spache := 0.121 * W / St + 0.082 * (100 * W_wl.Spache / W) + 0.659]

    if ("Spache.old" %in% measure)
        temp[, Spache.old := 0.141 * W / St + 0.086 * (100 * W_wl.Spache / W) + 0.839]

    if (any(c("Spache", "Spache.old") %in% measure)) temp[, W_wl.Spache := NULL]
        
    if ("Strain" %in% measure)
        temp[, Strain := Sy * 1 / (St/3) / 10]
    
    if ("Traenkle.Bailer" %in% measure) {
        Wprep <- vapply(toks, function(x) sum(x %in% prepositions), numeric(1))  # English prepositions
        Wconj <- vapply(toks, function(x) sum(x %in% conjunctions), numeric(1))  # English conjunctions
        temp[, Traenkle.Bailer := 224.6814 - (79.8304 * C / W) - (12.24032 * W / St) - (1.292857 * 100 * Wprep / W)]
    }
    
    if ("Traenkle.Bailer.2" %in% measure) {
        Wprep <- vapply(toks, function(x) sum(x %in% prepositions), numeric(1))  # English prepositions
        Wconj <- vapply(toks, function(x) sum(x %in% conjunctions), numeric(1))  # English conjunctions
        temp[, Traenkle.Bailer.2 := 234.1063 - (96.11069 * C / W) - (2.05444 * 100 * Wprep / W) - (1.02805 * 100 * Wconj / W)]
    }
    
    #     if ("TRI" %in% measure) {
    #         Ptn <- lengths(tokens(x, remove_punct = FALSE)) - lengths(toks)
    #         Frg <- NA  # foreign words -- cannot compute without a dictionary
    #         temp[, TRI := (0.449 * W_1Sy) - (2.467 * Ptn) - (0.937 * Frg) - 14.417]
    #     }
    
    if ("Wheeler.Smith" %in% measure)
        temp[, Wheeler.Smith := W / St * (10 * W2Sy) / W]
    
    Scrabble <- NULL
    if ("Scrabble" %in% measure)
        temp[, Scrabble := nscrabble(x, mean)]
    
    result <- data.frame(document = names(x), stringsAsFactors = FALSE)
    
    # if intermediate is desired, add intermediate quantities to output
    if (intermediate)
        measure <- c(measure, names(temp)[names(temp) %in% 
                                              c(c("W", "St", "C", "Sy", "W3Sy", "W2Sy", "W_1Sy", 
                                                  "W6C", "W7C", "Wlt3Sy", "W_wl.Dale.Chall", "W_wl.Spache"))])

    result <- cbind(result, as.data.frame(temp[, measure, with = FALSE]))
    class(result) <- c("readability", "textstat", "data.frame")
    rownames(result) <- as.character(seq_len(nrow(result)))
    return(result)
}


#' @noRd
#' @export
textstat_readability.character <- function(x,
                                  measure = c("all", "ARI", "ARI.simple", "Bormuth", "Bormuth.GP",
                                                 "Coleman", "Coleman.C2",
                                                 "Coleman.Liau", "Coleman.Liau.grade", "Coleman.Liau.short",
                                                 "Dale.Chall", "Dale.Chall.old", "Dale.Chall.PSK",
                                                 "Danielson.Bryan", "Danielson.Bryan.2",
                                                 "Dickes.Steiwer", "DRP", "ELF", "Farr.Jenkins.Paterson",
                                                 "Flesch", "Flesch.PSK", "Flesch.Kincaid",
                                                 "FOG", "FOG.PSK", "FOG.NRI", "FORCAST", "FORCAST.RGL",
                                                 "Fucks", "Linsear.Write", "LIW",
                                                 "nWS", "nWS.2", "nWS.3", "nWS.4", "RIX", "Scrabble",
                                                 "SMOG", "SMOG.C", "SMOG.simple", "SMOG.de",
                                                 "Spache", "Spache.old", "Strain",
                                                 "Traenkle.Bailer", "Traenkle.Bailer.2",
                                                 "Wheeler.Smith", "meanSentenceLength", "meanWordSyllables"),
                                  remove_hyphens = TRUE,
                                  min_sentence_length = 1, 
                                  max_sentence_length = 10000, ...) {
    
    textstat_readability(corpus(x), measure, remove_hyphens,
                         min_sentence_length, max_sentence_length, ...)
    
 
}

conjunctions <- c("for", "and", "nor", "but", "or", "yet", "so")
prepositions <- c("a", "abaft", "abeam", "aboard", "about", "above", "absent", "across", "afore", "after", "against", "along",
                  "alongside", "amid", "amidst", "among", "amongst", "an", "anenst", "apropos", "apud", "around", "as", "aside",
                  "astride", "at", "athwart", "atop", "barring", "before", "behind", "below", "beneath", "beside", "besides",
                  "between", "beyond", "but", "by", "chez", "circa", "ca", "c", "concerning", "despite", "down", "during", "except",
                  "excluding", "failing", "following", "for", "forenenst", "from", "given", "in", "including", "inside", "into",
                  "like", "mid", "midst", "minus", "modulo", "near", "next", "notwithstanding", "o'", "of", "off", "on", "onto",
                  "opposite", "out", "outside", "over", "pace", "past", "per", "plus", "pro", "qua", "regarding", "round", "sans",
                  "save", "since", "than", "through", "thru", "throughout", "thruout", "times", "to", "toward", "towards", "under",
                  "underneath", "unlike", "until", "unto", "up", "upon", "versus", "vs", "v", "via", "vis-a-vis", "with", "within",
                  "without", "worth")


#' @rdname data-internal
#' @details 
#' \code{data_char_wordlists} provides word lists used in some readability indexes; 
#' it is a named list of character vectors where each list element 
#' corresponds to a different readability index.  
#' 
#' These are:
#' \describe{
#' \item{\code{DaleChall}}{The long Dale-Chall list of 3,000 familiar (English) words needed to compute the Dale-Chall Readability Formula.}
#' \item{\code{Spache}}{The revised Spache word list (see Klare 1975, 73) needed to compute the Spache Revised Formula of readability (Spache 1974.}
#' }
#' @references
#' Chall, J. S., & Dale, E.  1995. \emph{Readability Revisited: The New Dale-Chall Readability Formula}. Brookline Books.
#'
#' Dale, Edgar, and Jeanne Sternlicht Chall. 1948. "A Formula for Predicting
#' Readability". \emph{Educational Research Bulletin} 27(1): 11-20.
#' 
#' Dale, Edgar, and Jeanne S Chall. 1948. "A Formula for Predicting Readability:
#' Instructions." \emph{Educational Research Bulletin} 27(2): 37–54.
#'
#' Klare, G. R. 1975. "Assessing readability." \emph{Reading Research Quarterly} 10(1): 62-102.
#'
#' Spache, G. 1953. "A new readability formula for primary-grade reading materials." \emph{The Elementary School Journal} 53: 410-413.
"data_char_wordlists"

