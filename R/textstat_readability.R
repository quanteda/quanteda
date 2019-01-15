#' Calculate readability
#'
#' Calculate the readability of text(s) using one of a variety of computed 
#' indexes.
#' @details 
#' The following readability formulas have been implemented, where
#' \itemize{
#'   \item \eqn{ASL} =  (Average Sentence Length) word count / sentence count
#'   \item \eqn{AWL} = (Average Word Length) character count / word count
#'   \item \eqn{AFL} = (Average Familiar Words) count of words matching the Dale-Chall List of 3000 "simple words" / total word count
#'   \item \eqn{Difficult Words} = count of words not matching the Dale-Chall List of 3000 "simple words"
#' }
#' 
#' \describe{
#'   \item{\code{"ARI"}:}{\emph{Automated Readability Index (1967)}: \deqn{ARI = 0.5 ASL  +
#'   4.71 AWL - 21.34}{ARI = 0.5 ASL  + 4.71 AWL - 21.34}}
#'   
#'   \item{\code{"ARI.Simple"}:}{\emph{Simplified Automated Readability Index}: \deqn{
#'   ARI.Simple =  ASL + 9 AWL}{ARI.Simple =  ASL + 9 AWL}}
#'   
#'   \item{\code{"Bormuth"}:}{\emph{Bormuth Mean Cloze Formula (1969)}: \deqn{Bormuth = 
#'   0.886593 - 0.03640 \times AWL + 0.161911 \times AFW  - 0.21401 \times ASL - 0.000577 \times ASL^2 - 
#'   0.000005 \times ASL^3 }{Bormuth = 0.886593 - 0.03640 x AWL + 0.161911 x AFW  - 0.21401 x 
#'   ASL - 0.000577 x ASL^2 -  0.000005 x ASL^3 }}
#'   
#'   \item{\code{"Bormuth.GP"}:}{\emph{Bormuth Grade Placement (1969)}:
#'   \deqn{Bormuth.GP = 4.275 + 12.881M - 34.934M^2 + 20.388 M^3 + 26.194CCS -
#'   2.046CCS^2 - 11.767CCS^3 - 42.285(M \times CCS) + 97.620(M \times CCS)^2 -
#'   59.538(M \times CCS)^2 where M is the Bormuth Mean Cloze Formula as in
#'   \emph{"Bormuth"} above, and CCS is the Cloze Criterion Score (Bormuth,
#'   1968)}{Bormuth.GP = 4.275 + 12.881M - 34.934M^2 + 20.388 M^3 + 26.194CCS -
#'   2.046CCS^2 - 11.767CCS^3 - 42.285(M x CCS) + 97.620(M x CCS)^2 - 59.538(M x
#'   CCS)^2 where M is the Bormuth Mean Cloze Formula as in "Bormuth" above, and
#'   CCS is the Cloze Criterion Score (Bormuth, 1968)}}
#'  
#'   \item{\code{"Coleman"}:}{\emph{Coleman's Readability Formula 1 (1971)}: \deqn{Coleman = 
#'   1.29 \times \frac{100 \times No. of 1-Syllable Words}{No. of Words} - 38.45}{Coleman = 
#'   1.29 x (100 x No. of 1-Syllable Words / No. of Words) - 38.45}}
#'   
#'   \item{\code{"Coleman.C2"}:}{\emph{Coleman's Readability Formula 2 (1971)}:
#'   \deqn{Coleman2 = 1.16 \times \frac{100 \times No. of 1-Syllable Words}{No.
#'   of Words} + 1.48 \times \frac{100 \times No. of Sentences}{No. of Words}
#'   -37.95 }{Coleman2 = 1.16 x (100 x No. of 1-Syllable Words / No. of Words) +
#'   1.48 x (100 x No. of Sentences / No. of Words) - 37.95}}
#' 
#'   \item{\code{"Coleman.Liau"}:}{\emph{Coleman-Liau Estimated Cloze Percent
#'   (ECP) (1975)}: \deqn{Coleman-Liau ECP = 141.8401 - 0.214590 \times 100
#'   \times AWL + 1.079812 \times \frac{No. of Sentences \times 100}{No. of
#'   Words}}{Coleman-Liau ECP =  141.8401 - (0.214590 x 100 x AWL) + (1.079812 x
#'   No. of Sentences x 100 / No. of Words)}}
#'   
#'   \item{\code{"Coleman.Liau.Grade"}:}{\emph{Coleman-Liau Grade Level (1975)}:
#'   \deqn{Coleman-Liau Grade = -27.4004 \times Coleman.Liau.ECP \times 100 +
#'   23.06395}{Coleman-Liau Grade =  -27.4004 x Coleman.Liau.ECP / 100 +
#'   23.06395}}
#'
#'   \item{\code{"Coleman.Liau.Short"}:}{\emph{Coleman-Liau Index (1975)}:
#'   \deqn{Coleman-Liau.Short= 5.88 \times AWL + 29.6 \times \frac{No. of
#'   Sentences}{No. of Words} - 15.8}{Coleman-Liau.Short = 5.88 x Average Word
#'   Length + 0.296 x (No. of Sentences / No. of Words) - 15.8}}
#'   
#'   \item{\code{"Dale.Chall"}:}{\emph{New Dale-Chall (1995)}: \deqn{Dale.Chall
#'   = 64 - (0.95 \times 100 \times \frac{No. of Difficult Words}{No. of Words})
#'   - (0.69 \times ASL)}{Dale.Chall = 64 - (0.95 x 100 x No. of Difficult
#'   Words/No. of Words) - (0.69 x ASL)}}
#'
#'   \item{\code{"Dale.Chall.Old"}:}{\emph{Dale-Chall (1948)}: \deqn{
#'   Dale.Chall.Old =  0.1579 \times 100 \times \frac{No. of Difficult
#'   Words}{No. of Words} + 0.0496 \times ASL (+ 3.6365)}{Dale.Chall.Old =
#'   0.1579 x 100 x No. of Difficult Words / No. of Words  + 0.0496 x ASL
#'   (+3.6365 if No. of Difficult Words / No. of Words is > 0.05)}}
#'
#'   \item{\code{"Dale.Chall.PSK"}:}{\emph{Dale-Chall Measure,
#'   Powers-Sumner-Kearl Variation (1958)}: \deqn{Dale.Chall.PSK = 0.1155 \times
#'   100 \frac{No. of Difficult Words}{No. of Words}) + (0.0596 \times ASL) +
#'   3.2672 }{Dale.Chall.PSK = (0.1155 x 100 x No. of Difficult Words / No. of
#'   Words) + (0.0596 x ASL) + 3.2672 }}
#'
#'   \item{\code{"Danielson.Bryan"}:}{\emph{Danielson-Bryan (1963)}: \deqn{
#'   Danielson-Bryan = (1.0364 \times \frac{No. of Characters}{No. of Blanks}) +
#'   (0.0194 \times \frac{No. of Characters}{No. of Sentences}) -
#'   0.6059}{Danielson-Bryan = (1.0364 x No. of Characters / No. of Blanks) +
#'   (0.0194 x No. of Characters / No. of Sentences) - 0.6059}}
#'
#'   \item{\code{"Danielson.Bryan2"}:}{\emph{Danielson-Bryan2 (1963)}: \deqn{
#'   Danielson-Bryan = 131.059- (10.364 \times \frac{No. of Characters}{No. of
#'   Blanks}) + (0.0194 \times \frac{No. of Characters}{No. of
#'   Sentences})}{Danielson-Bryan2 = 131.059 - (10.364 x No. of Characters / No.
#'   of Blanks) + (0.0194 x No. of Characters / No. of Sentences)}}
#'
#'   \item{\code{"Dickes.Steiwer"}:}{\emph{Dickes-Steiwer Index (1977)}: \deqn{
#'   Dickes-Steiwer = 235.95993 - (7.3021 \times AWL)  - (12.56438 \times ASL) -
#'   (50.03293 \times Type-Token Ratio)}{Dickes-Steiwer = 235.95993 - (73.021 x
#'   AWL) - (12.56438 x ASL) - (50.03293 x TTR)}}
#'
#'   \item{\code{"DRP"}:}{\emph{Degrees of Reading Power (Relies on Bormuth.MC
#'   (1969) as in "Bormuth" above)}: \deqn{ DRP = (1 - Bormuth.MC) \times
#'   100}{DRP = (1 - Bormuth.MC) x 100}}
#'
#'   \item{\code{"ELF"}:}{\emph{Easy Listening Formula, Fang (1966)}: \deqn{ ELF
#'   = \frac{No. of Words with >= 2 Syllables}{No. of Sentences}}{ELF = (No. of
#'   Words with >= 2 Syllables / No. of Sentences)}}
#'
#'   \item{\code{"Farr.Jenkins.Paterson"}:}{\emph{Farr-Jenkins-Paterson's
#'   Simplification of Flesch's Reading Ease Score (1951)}: \deqn{
#'   Farr.Jenkins.Paterson = -31.517 - (1.015 \times ASL) + (1.599 \times
#'   \frac{No. of 1-Syllable Words}{No. of Words}}{Farr.Jenkins.Paterson -31.517
#'   - (1.015 x ASL) + (1.599 x No. of 1-Syllable Words / No. of Words)}}
#'
#'   \item{\code{"Flesch"}:}{\emph{Flesch Reading Ease Score (FRES) (1948)}:
#'   \deqn{ FRES = 206.835 - (1.015 \times ASL) - (84.6 \times \frac{No. of
#'   Syllables}{No. of Words})}{FRES = 206.835 - (1.015 x ASL) - (84.6 x (No. of
#'   Syllables / No. of Words))}}
#'
#'   \item{\code{"Flesch.PSK"}:}{\emph{Flesch Reading Ease Score,
#'   Powers-Sumner-Kearl's Variation (1958)}: \deqn{Flesch.PSK = (0.0778 \times
#'   ASL) + (4.55 \times \frac{No. of Syllables}{No. of Words}) -
#'   2.2029}{Flesch.PSK = (0.0078 x ASL) + (4.55 x No. of Syllables / No. of
#'   Words) - 2.2029}}
#'
#'   \item{\code{"Flesch.Kincaid"}:}{\emph{Flesch-Kincaid Score (1975)}: \deqn{
#'   Flesch-Kincaid = 0.39 \times ASL + 11.8  \times \frac{No. of Syllables}{No.
#'   of Words}- 15.59}{Flesch-Kincaid = 0.39 x ASL + 11.8  x(No. of Syllables /
#'   No. of Words) - 15.59}}
#'
#'   \item{\code{"FOG"}:}{\emph{Gunning's Fog Index (1952)}: \deqn{FOG = 0.4
#'   \times (ASL + 100 \times (No. of Words with >= 3 Syllables  / No. of
#'   Words))}{FOG = 0.4 x (ASL + 100 x (No. of Words with >= 3 Syllables / No.
#'   of Words))}}
#'
#'   \item{\code{"FOG.PSK"}:}{\emph{FOG, Powers-Sumner-Kearl's Variation
#'   (1958)}: \deqn{FOG.PSK = 3.0680 \times (0.0877 \times ASL) +(0.0984 \times
#'   100 \times \frac{No. of Words with >=3 Syllables}{No. of Words})}{FOG.PSK =
#'   3.0680 x (0.0877 x ASL) +(0.0984 x 100 x No. of Words with >=3 Syllables /
#'   No. of Words}}
#'
#'   \item{\code{"FOG.NRI"}:}{\emph{FOG, Navy Readability Index (1975)}:
#'   \deqn{FOG.NRI = ( \frac{(No. of Words with < 3 Syllables + 3 \times No. of
#'   3-Syllables Words)}{(100 \times \frac{No. of Sentences}{No. of Words})}  -
#'   3) / 2 }{FOG = (((No. of Words with <3 Syllables + 3 x No. of 3-Syllables
#'   Words) / (100 x No. of Sentences / No. of Words))-3) / 2}}
#'
#'   \item{\code{"FORCAST"}:}{\emph{FORCAST (Simplified FORCAST.RGL) (Caylor &
#'   Sticht, 1973)}: \deqn{FORCAST = 20 - \frac{(No. of 1-Syllable Words \times
#'   150)}{(No. of Words \times 10)}}{FORCAST = 20 - (No. of 1-Syllable Words x
#'   150) / (No. of Words x 10)
#'
#'   (The scaling by 150 arises because the initial FORCAST index is based on
#'   just a sample of 150 tokens)}}
#'
#'   \item{\code{"FORCAST.RGL"}:}{\emph{FORCAST.RGL (Caylor & Sticht, 1973)}:
#'   \deqn{FORCAST.RGL= 20.43 - 0.11 \times \frac{(No. of 1-Syllable Words
#'   \times 150)}{No. of Words}}{FORCAST.RGL = 20.43 - 0.11 x (No. of 1-Syllable
#'   Words x 150) / (No. of Words)
#'
#'   (The scaling by 150 arises because the initial FORCAST index is based on
#'   just a sample of 150 tokens)}}
#'
#'   \item{\code{"Fucks"}:}{\emph{Fucks' Stilcharakteristik (Style
#'   Characteristic)}: \deqn{Fucks = AWL  \times ASL }{Fucks = AWL x ASL}}
#'
#'   \item{\code{"Linsear.Write"}:}{\emph{Linsear Write (Klare, 1975)}:
#'   \deqn{Linsear Write = \frac{( (100 - (\frac{100 \times No. of Words with <3
#'   Syllables}{No. of Words})) + (3 \times \frac{100 \times No. of Words with
#'   >= 3 Syllables}{No. of Words}) )}{(100 \times \frac{No. of Sentences}{No.
#'   of Words})}}{((100 - (100 x No. of Words with <3 Syllables / No. of Words))
#'   + (3 x 100 x No. of Words with >= 3 Syllables / No. of Words)) / (100 x No.
#'   of Sentences / No. of Words)}}
#'
#'
#'   \item{\code{"LIW"}:}{\emph{Björnsson's Läsbarhetsindex (1968) (For Swedish
#'   Texts)}: \deqn{LIW = ASL + \frac{100 \times No. of Words with >= 7
#'   Syllables}{No. of Words}}{LIW = ASL + (100 * No. of Words with >= 7
#'   Syllables / No. of Words)}}
#'
#'   \item{\code{"nWS"}:}{\emph{Neue Wiener Sachtextformeln 1 (Bamberger &
#'   Vanecek, 1984)}: \deqn{nwS = 19.35 \times \frac{No. of Words with >=3
#'   Syllables}{No. of Words} + 0.1672 \times ASL + 12.97 \times \frac{No. of
#'   Words with >= 6 Characters}{No. of Words} - 3.27 \times \frac{No. of
#'   1-Syllable Words}{No. of Words} - 0.875}{ nwS = (19.35 x No. of Words with
#'   >=3 Syllables / No. of Words) + (0.1672 x ASL) + (12.97 x No. of Words with
#'   >=6 Characters / No. of Words) - (3.27 x No. of 1-Syllable Words / No. of
#'   Words)- 0.875}}
#'
#'   \item{\code{"nWS.2"}:}{\emph{Neue Wiener Sachtextformeln 2 (Bamberger &
#'   Vanecek, 1984)}: \deqn{nwS.2 = 20.07 \times \frac{No. of Words with >=3
#'   Syllables}{No. of Words} + 0.1682 \times ASL + 13.73 \times \frac{No. of
#'   Words with >= 6 Characters}{No. of Words} - 2.779}{ nWs.2 = (20.07 x No. of
#'   Words with >=3 Syllables / No. of Words) + (0.1682 x ASL) + (13.73 x No. of
#'   Words with >=6 Characters / No. of Words) - 2.779}}
#'
#'   \item{\code{"nWS.3"}:}{\emph{Neue Wiener Sachtextformeln 3 (Bamberger &
#'   Vanecek, 1984)}: \deqn{nwS.3 = 29.63 \times \frac{No. of Words with >=3
#'   Syllables}{No. of Words} + 0.1905 \times ASL - 1.1144}{ nWs.3 = (29.63 x
#'   No. of Words with >=3 Syllables / No. of Words) + (0.1905 x ASL) - 1.1144}}
#'
#'   \item{\code{"nWS.4"}:}{\emph{Neue Wiener Sachtextformeln 4 (Bamberger &
#'   Vanecek, 1984)}: \deqn{nwS.4 = 27.44 \times \frac{No. of Words with >=3
#'   Syllables}{No. of Words} + 0.2656 \times ASL - 1.693}{ nWs.4 = (27.44 x No.
#'   of Words with >=3 Syllables / No. of Words) + (0.2656 x ASL) - 1.693}}
#'
#'   \item{\code{"RIX"}:}{\emph{Anderson's Readability Index (1983)}: \deqn{RIX
#'   = \frac{No. of Words with >= 7 Syllables}{No. of Sentences}}{RIX = No. of
#'   Words with >= 7 Syllables / No. of Sentences)}}
#'
#'   \item{\code{"Scrabble"}:}{\emph{Scrabble Measure}: {Scrabble = Mean
#'   Scrabble Letter Values of All Words}}
#'
#'   \item{\code{"SMOG"}:}{\emph{Simple Measure of Gobbledygook (McLaughlin,
#'   1969)}: \deqn{SMOG = 1.043 \times \sqrt{No. of  Words with >=3 Syllables
#'   \times \frac{30}{No. of Sentences}} + 3.1291}{SMOG = 1.043 x sqrt(No. of
#'   Words with >=3 Syllables x 30 / No. Of Sentences) + 3.1291}}
#'
#'   \item{\code{"SMOG.C"}:}{\emph{SMOG.C}: \deqn{SMOG.C = 0.9986 \times
#'   \sqrt{No. of Words with >=3 Syllables \times \frac{30}{No. of Sentences} +
#'   5} +  2.8795}{SMOG.C = 0.9986 x sqrt(No. of Words with >=3 Syllables x (30
#'   / No. Of Sentences) + 5) + 2.8795}}
#'
#'   \item{\code{"SMOG.Simple"}:}{\emph{SMOG Simplified}: \deqn{SMOG.Simple =
#'   \sqrt{No. of Words with >=3 Syllables \times \frac{30}{No. of Sentences}} +
#'   3}{SMOG.Simple = sqrt(No. of Words with >=3 Syllables x 30 / No. of
#'   Sentences) + 3}}
#'
#'   \item{\code{"SMOG.de"}:}{\emph{SMOG.de (Adapted for German Texts)}:
#'   \deqn{SMOG.de = \sqrt{No. of Words with >=3 Syllables \times \frac{30}{No.
#'   of Sentences}}-2}{SMOG.de = sqrt(No. of Words with >=3 Syllables x 30 / No.
#'   Of Sentences) - 2 }}
#'
#'   \item{\code{"Spache"}:}{\emph{Spache (1952)}: \deqn{Spache = 0.121 \times
#'   ASL + 0.082 \times (Unique Words not in Spache Word List / No. of Words) +
#'   0.659}{Spache = 0.121 x ASL + 0.082 x (Unique Words not in Spache Word List
#'   / No. of Words) + 0.659}}
#'
#'   \item{\code{"Spache.old"}:}{\emph{Spache (1952)}: \deqn{Spache.old = 0.141
#'   \times ASL + 0.086 \times Unique Words not in Spache Word List / No. of
#'   Words) + 0.839}{Spache.old = 0.141 x ASL + 0.086 x (Unique Words not in
#'   Spache Word List/ No. of Words) + 0.839}}
#'
#'   \item{\code{"Strain"}:}{\emph{Strain Index (Solomon, 2006)}: \deqn{ Strain
#'   = Number of Syllables / (Number of Sentences / 3) /10}{Strain = Number of
#'   Syllables / (Number of Sentences / 3) / 10 (The scaling by 3 arises because
#'   the initial Strain index is based on just the first 3 sentences )}}
#'
#'   \item{\code{"Traenkle.Bailer"}:}{\emph{Tränkle & Bailer (1984)}:
#'   \deqn{Tränkle.Bailer = 224.6814 - (79.8304 \times AWL) - (12.24032 \times
#'   ASL) - (1.292857 \times 100 \times \frac{No. of Prepositions}{No. of
#'   Words}}{Tränkle.Bailer = 224.6814 - (79.8304 x AWL) + (12.24032 x ASL) -
#'   (1.292857 x 100 x No. of Prepositions / No. of Words)}}
#'
#'   \item{\code{"Traenkle.Bailer2"}:}{\emph{Tränkle & Bailer (1984)}:
#'   \deqn{Tränkle.Bailer2 = Tränkle.Bailer2 =  234.1063 - (96.11069 \times AWL
#'   ) - (2.05444 \times 100 \times \frac{No. of Prepositions}{No. of Words}) -
#'   (1.02805 \times 100 \times \frac{No. of Conjucntions}{No. of
#'   Words}}{Tränkle.Bailer2 = 234.1063 - 96.11069 x AWL  - 2.05444 x 100 x (No.
#'   of Prepositions / No. of Words) - 1.02805 x 100 x (No. of Conjunctions/No.
#'   of Words)}}
#'
#'   \item{\code{"Wheeler.Smith"}:}{\emph{Wheeler & Smith (1954)}:
#'   \deqn{Wheeler.Smith = ASL \times 10 \times \frac{No. of Words with >=
#'   2Syllables}{No. of Words}}{Wheeler.Smith = ASL x 10 x (No. of Words with >=
#'   2Syllables /No. of Words)}}
#'
#'   \item{\code{"meanSentenceLength"}:}{\emph{Average Sentence Length}:
#'   \deqn{Average Sentence Length = \frac{No. of Words}{No. of
#'   Sentences}}{Average Sentence Length = No. of Words / No. of Sentences}}
#'
#'   \item{\code{"meanWordSyllables"}:}{\emph{Average Word Syllables}:
#'   \deqn{Average Word Syllables = \frac{No. of Syllables}{No. of
#'   Words}}{Average Word Syllables = No. of Syllables / No. of Words}}
#'   
#' }
#'
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
#' txt <- c(doc1 = "Readability zero one. Ten, Eleven.", 
#'          doc2 = "The cat in a dilapidated tophat.")
#' textstat_readability(txt, measure = "Flesch")
#' textstat_readability(txt, measure = c("FOG", "FOG.PSK", "FOG.NRI"))
#' 
#' textstat_readability(data_corpus_inaugural[48:58], 
#'                      measure = c("Flesch.Kincaid", "Dale.Chall.old"))
#' @references 
#'   Anderson, J. (1983). Lix and rix: Variations on a little-known readability index. 
#'   Journal of Reading, 26(6), 490-496.
#'   
#'   Bamberger, R. & Vanecek, E. (1984). Lesen–Verstehen–Lernen–Schreiben. Wien: Jugend und Volk.
#'   
#'   Björnsson, C. H. (1968). \emph{Läsbarhet}. Stockholm: Liber.
#'   
#'   Bormuth, J. R. (1969). \href{https://files.eric.ed.gov/fulltext/ED029166.pdf}{Development 
#'   of Readability Analysis.}  
#'   
#'   Bormuth, J. R. (1968). Cloze test readability: Criterion reference scores. 
#'   Journal of educational measurement, 5(3), 189-196.  
#'   
#'   Caylor, J. S. (1973). Methodologies for Determining Reading Requirements of Military 
#'   Occupational Specialities.
#'   
#'   Caylor, J. S., & Sticht, T. G. (1973). Development of a Simple Readability Index for Job 
#'   Reading Material.
#'   
#'   Coleman, E. B. (1971). Developing a technology of written instruction: Some determiners 
#'   of the complexity of prose. Verbal learning research and the technology of written 
#'   instruction, 155-204.
#'   
#'   Coleman, M., & Liau, T. L. (1975). A computer readability formula designed for machine
#'   scoring. Journal of Applied Psychology, 60(2), 283.
#'   
#'   Dale, E., & Chall, J. S. (1948). "A formula for predicting readability: Instructions."
#'   \emph{Educational Research Bulletin} 37-54.
#'   
#'   Chall, J. S., & Dale, E. (1995). \emph{Readability revisited: The new
#'   Dale-Chall readability formula}. Brookline Books.
#'   
#'   Dickes, P. & Steiwer, L. (1977). "Ausarbeitung von Lesbarkeitsformeln
#'   für die Deutsche Sprache." \emph{Zeitschrift für Entwicklungspsychologie und
#'   Pädagogische Psychologie} 9(1), 20--28.
#'   
#'   Danielson, W. A., & Bryan, S. D. (1963). "Computer automation of two
#'   readability formulas." \emph{Journalism Quarterly} 40(2), 201-206.
#'   
#'   DuBay, W. H. (2004). \emph{The Principles of Readability.}
#'   
#'   Fang, I. E. (1966). "The 'Easy listening formula'". \emph{Journal of
#'   Broadcasting & Electronic Media} 11(1): 63-68.
#'   
#'   Farr, J. N., Jenkins, J. J., & Paterson, D. G. (1951).  "Simplification of
#'   Flesch Reading Ease Formula." \emph{Journal of applied psychology} 35(5):
#'   333.
#'   
#'   Flesch, R. (1948). "A new readability yardstick." \emph{Journal of applied
#'   psychology} 32(3): 221.
#'   
#'   Fucks, W. (1955). Der Unterschied des Prosastils von Dichtern und anderen Schriftstellern. 
#'   Sprachforum, 1, 233–244.
#'   
#'   Gunning, R. (1952). The technique of clear writing.
#'   
#'   Klare, G.R. (1975). Assessing readability. Reading Research Quarterly, 10(1), 62--102.
#'   
#'   Kincaid, J. P., Fishburne Jr, R. P., Rogers, R. L., & Chissom, B. S.
#'   (1975). Derivation of new readability formulas (automated readability
#'   index, FOG count and Flesch reading ease formula) for navy enlisted
#'   personnel.
#'   
#'   McLaughlin, G. H. (1969). SMOG grading-a new readability formula. Journal
#'   of reading, 12(8), 639-646.
#'
#'   Powers, R. D., Sumner, W. A., & Kearl, B. E. (1958). A recalculation of
#'   four adult readability formulas. Journal of Educational Psychology, 49(2),
#'   99.
#'
#'   Senter, R. J., & Smith, E. A. (1967). Automated readability index.
#'   CINCINNATI UNIV OH.
#'
#'   *Solomon, N. W. (2006). Qualitative Analysis of Media Language. India.
#'
#'   Spache, G. (1953). A new readability formula for primary-grade reading
#'   materials. The Elementary School Journal, 53, 410–413.
#'
#'   Tränkle, U. & Bailer, H. (1984). Kreuzvalidierung und Neuberechnung von
#'   Lesbarkeitsformeln für die deutsche Sprache. Zeitschrift für
#'   Entwicklungspsychologie und Pädagogische Psychologie, 16(3), 231–244.
#'
#'   Wheeler, L.R. & Smith, E.H. (1954). A practical readability formula for the
#'   classroom teacher in the primary grades. Elementary English, 31, 397–399.
#'
#'   *Nimaldasan is the pen name of N. Watson Solomon, Assistant Professor of
#'   Journalism, School of Media Studies, SRM University, India.
#'   
textstat_readability <- function(x,
                                 measure,
                                 remove_hyphens = TRUE,
                                 min_sentence_length = 1, 
                                 max_sentence_length = 10000, 
                                 intermediate = FALSE, ...) {
    UseMethod("textstat_readability")
}

#' @export
textstat_readability.default <- function(x,
                                        measure,
                                        remove_hyphens = TRUE,
                                        min_sentence_length = 1, 
                                        max_sentence_length = 10000, 
                                        intermediate = FALSE, ...) {
    stop(friendly_class_undefined_message(class(x), "textstat_readability"))
}    

#' @importFrom stringi stri_length
#' @export
textstat_readability.corpus <- function(x, 
                                        measure,
                                        remove_hyphens = TRUE,
                                        min_sentence_length = 1, 
                                        max_sentence_length = 10000, 
                                        intermediate = FALSE, ...) {
    
    unused_dots(...)
    
    measure_option <- c("ARI", "ARI.simple", "Bormuth.MC", "Bormuth.GP",
                        "Coleman", "Coleman.C2",
                        "Coleman.Liau.ECP", "Coleman.Liau.grade", "Coleman.Liau.short",
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
    accepted_measures <- c(measure_option, "Bormuth" , "Coleman.Liau")
    
    if (measure[1] == 'all') {
        measure <- measure_option
    } else {
        is_valid <- measure %in% accepted_measures
        if (!all(is_valid))
            stop("Invalid measure(s): ", measure[!is_valid])
    }
    
    if ("Bormuth" %in% measure) {
        measure[measure=="Bormuth"] <- "Bormuth.MC"
        measure <- unique(measure)
    }
        
    
    if ("Coleman.Liau" %in% measure) {
        measure[measure=="Coleman.Liau"] <- "Coleman.Liau.ECP"
        measure <- unique(measure)
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
        W_wl.Spache <- ARI <- ARI.NRI <- ARI.simple <- Bormuth.MC <- Bormuth.GP <- Coleman <- Coleman.C2 <-
        Coleman.Liau.ECP <- Coleman.Liau.grade <- Coleman.Liau.short <- Dale.Chall <- Dale.Chall.old <-
        Dale.Chall.PSK <- Danielson.Bryan <- Danielson.Bryan.2 <- Dickes.Steiwer <- DRP <- ELF <-
        Farr.Jenkins.Paterson <- Flesch <- Flesch.PSK <- Flesch.Kincaid <- FOG <- FOG.PSK <- FOG.NRI <-
        FORCAST <- FORCAST.RGL <- Fucks <- Linsear.Write <- LIW <- nWS <- nWS.2 <- nWS.3 <- nWS.4 <-
        RIX <- SMOG <- SMOG.C <- SMOG.simple <- SMOG.de <- Spache <- Spache.old <- Strain <- Wheeler.Smith <-
        Bl <- Traenkle.Bailer <- Traenkle.Bailer.2 <- Bormuth <-
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
    
    if ("Bormuth.MC" %in% measure) {
        temp[, Bormuth.MC := 0.886593 - (0.08364 * C/W) + 0.161911 *
                         (W_wl.Dale.Chall / W) ^ 3 - 0.21401 * (W/St) + 0.000577 * (W/St) ^ 2 - 0.000005 * (W/St) ^ 3]
    }
    if ("Bormuth.GP" %in% measure) {
        CCS <- 35 # Cloze criterion score, percent as integer
        temp[, Bormuth.MC.Temp := 0.886593 - (0.08364 * C/W) + 0.161911 *
                         (W_wl.Dale.Chall / W) ^ 3 - 0.21401 * (W/St) + 0.000577 * (W/St) ^ 2 - 0.000005 * (W/St) ^ 3]
        temp[, Bormuth.GP := 4.275 + 12.881 * Bormuth.MC.Temp - (34.934 * Bormuth.MC.Temp^2) + (20.388 * Bormuth.MC.Temp^3) +
                         (26.194 * C - 2.046 * CCS ^ 2) - (11.767 * CCS ^ 3) - (44.285 * Bormuth.MC.Temp * CCS) +
                         (97.620 * (Bormuth.MC.Temp * CCS)^2) - (59.538 * (Bormuth.MC.Temp * CCS)^3)]
        temp[, Bormuth.MC.Temp := NULL]
    }
    
    if ("Coleman" %in% measure)
        temp[, Coleman := 1.29 * (100 * W_1Sy / W) - 38.45]
    
    if ("Coleman.C2" %in% measure)
        temp[, Coleman.C2 := 1.16 * (100 * W_1Sy / W) + 1.48 * (100 * St / W) - 37.95]
    
    ## cannot compute Coleman.C3, Coleman.C4 without knowing the number of pronouns or prepositions
    
    if ("Coleman.Liau.ECP" %in% measure)
        temp[, Coleman.Liau.ECP   := 141.8401 - 0.214590 * (100 * C / W) + 1.079812 * (100 * St / W)]
    
    if ("Coleman.Liau.grade" %in% measure) {
        temp[, Coleman.Liau.ECP.Temp   := 141.8401 - 0.214590 * (100 * C / W) + 1.079812 * (100 * St / W)]
        temp[, Coleman.Liau.grade := -27.4004 * Coleman.Liau.ECP.Temp / 100 + 23.06395]
        temp[, Coleman.Liau.ECP.Temp   := NULL]
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
        temp[, Bormuth.MC.Temp := 0.886593 - (0.08364 * C/W) + 0.161911 *
                         (W_wl.Dale.Chall / W) ^ 3 - 0.21401 * (W/St) + 0.000577 * (W/St) ^ 2 - 0.000005 * (W/St) ^ 3]
        temp[, DRP := (1 - Bormuth.MC.Temp) * 100]
        temp[, Bormuth.MC.Temp := NULL]
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
                                           measure,
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
#' \item{\code{DaleChall}}{The long Dale-Chall list of 3,000 familiar (English)
#' words needed to compute the Dale-Chall Readability Formula.}
#' \item{\code{Spache}}{The revised Spache word list (see Klare 1975, 73) needed
#' to compute the Spache Revised Formula of readability (Spache 1974.}
#' }
#' @references
#' Chall, J. S., & Dale, E.  1995. \emph{Readability Revisited: The New
#' Dale-Chall Readability Formula}. Brookline Books.
#'
#' Dale, Edgar, and Jeanne Sternlicht Chall. 1948. "A Formula for Predicting
#' Readability". \emph{Educational Research Bulletin} 27(1): 11-20.
#'
#' Dale, Edgar, and Jeanne S Chall. 1948. "A Formula for Predicting Readability:
#' Instructions." \emph{Educational Research Bulletin} 27(2): 37–54.
#'
#' Klare, G. R. 1975. "Assessing readability." \emph{Reading Research Quarterly}
#' 10(1): 62-102.
#'
#' Spache, G. 1953. "A new readability formula for primary-grade reading
#' materials." \emph{The Elementary School Journal} 53: 410-413.
#'
#' Tränkle, U. & Bailer, H. (1984). Kreuzvalidierung und Neuberechnung von
#' Lesbarkeitsformeln für die deutsche Sprache. \emph{Zeitschrift für
#' Entwicklungspsychologie und Pädagogische Psychologie} 16(3), 231–244.
#'
#' Wheeler, L.R. & Smith, E.H. (1954). A practical readability formula for the
#' classroom teacher in the primary grades. \emph{Elementary English} 31,
#' 397–399.
"data_char_wordlists"

