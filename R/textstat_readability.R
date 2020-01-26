#' Calculate readability
#'
#' Calculate the readability of text(s) using one of a variety of computed
#' indexes.
#' @details
#' The following readability formulas have been implemented, where
#' \itemize{
#'   \item Nw = \eqn{n_{w}} = number of words
#'   \item Nc = \eqn{n_{c}} = number of characters
#'   \item Nst = \eqn{n_{st}} = number of sentences
#'   \item Nsy = \eqn{n_{sy}} = number of syllables
#'   \item Nwf = \eqn{n_{wf}} = number of words matching the Dale-Chall List
#'         of 3000 "familiar words"
#'   \item ASL = Average Sentence Length: number of words / number of sentences
#'   \item AWL = Average Word Length: number of characters / number of words
#'   \item AFW = Average Familiar Words: count of words matching the Dale-Chall
#'         list of 3000 "familiar words" / number of all words
#'   \item Nwd = \eqn{n_{wd}} = number of "difficult" words not matching the
#'         Dale-Chall list of "familiar" words
#' }
#'
#' \describe{
#'   \item{`"ARI"`:}{Automated Readability Index (Senter and Smith 1967)
#'   \deqn{0.5 ASL  + 4.71 AWL - 21.34}}
#'
#'   \item{`"ARI.Simple"`:}{A simplified version of Senter and Smith's (1967) Automated Readability Index.
#'   \deqn{ASL + 9 AWL}}
#'
#'   \item{`"Bormuth.MC"`:}{Bormuth's (1969) Mean Cloze Formula.
#'   \deqn{0.886593 - 0.03640 \times AWL + 0.161911 \times AFW  - 0.21401 \times
#'   ASL - 0.000577 \times ASL^2 - 0.000005 \times ASL^3}{
#'   0.886593 - 0.03640 * AWL + 0.161911 * AFW  - 0.21401 *
#'   ASL - 0.000577 * ASL^2 - 0.000005 * ASL^3}}
#'
#'   \item{`"Bormuth.GP"`:}{Bormuth's (1969) Grade Placement score.
#'   \deqn{4.275 + 12.881M - 34.934M^2 + 20.388 M^3 + 26.194 CCS -
#'   2.046 CCS^2 - 11.767 CCS^3 - 42.285(M \times CCS) + 97.620(M \times CCS)^2 -
#'   59.538(M \times CCS)^2}{
#'   4.275 + 12.881M - 34.934M^2 + 20.388 M^3 + 26.194 CCS -
#'   2.046 CCS^2 - 11.767 CCS^3 - 42.285(M * CCS) + 97.620(M * CCS)^2 -
#'   59.538(M * CCS)^2}
#'   where \eqn{M} is the Bormuth Mean Cloze Formula as in
#'   `"Bormuth"` above, and \eqn{CCS} is the Cloze Criterion Score (Bormuth,
#'   1968).}
#'
#'   \item{`"Coleman"`:}{Coleman's (1971) Readability Formula 1.
#'   \deqn{1.29 \times \frac{100 \times n_{wsy=1}}{n_{w}} - 38.45}{
#'   1.29 * (100 * Nwsy1 / Nw) - 38.45}
#'
#'   where \eqn{n_{wsy=1}} = Nwsy1 = the number of one-syllable words.  The
#'   scaling by 100 in this and the other Coleman-derived measures arises
#'   because the Coleman measures are calculated on a per 100 words basis.}
#'
#'   \item{`"Coleman.C2"`:}{Coleman's (1971) Readability Formula 2.
#'   \deqn{1.16 \times \frac{100 \times n_{wsy=1}}{
#'   Nw + 1.48 \times \frac{100 \times n_{st}}{n_{w}} - 37.95}}{
#'   1.16 * (100 * Nwsy1 / Nw) + 1.48 * (100 * Nst / Nw) - 37.95}}
#'
#'   \item{`"Coleman.Liau.ECP"`:}{Coleman-Liau Estimated Cloze Percent
#'   (ECP) (Coleman and Liau 1975).
#'   \deqn{141.8401 - 0.214590 \times 100
#'   \times AWL + 1.079812 \times \frac{n_{st} \times 100}{n_{w}}}{
#'   141.8401 - (0.214590 * 100 * AWL) + (1.079812 * Nst * 100 / Nw)}}
#'
#'   \item{`"Coleman.Liau.grade"`:}{Coleman-Liau Grade Level (Coleman
#'   and Liau 1975).
#'   \deqn{-27.4004 \times \mathtt{Coleman.Liau.ECP} \times 100 +
#'   23.06395}{-27.4004 * Coleman.Liau.ECP / 100 + 23.06395}}
#'
#'   \item{`"Coleman.Liau.short"`:}{Coleman-Liau Index (Coleman and Liau 1975).
#'   \deqn{5.88 \times AWL + 29.6 \times \frac{n_{st}}{n_{w}} - 15.8}{
#'   5.88 * AWL + (0.296 * Nst / Nw) - 15.8}}
#'
#'   \item{`"Dale.Chall"`:}{The New Dale-Chall Readability formula (Chall
#'   and Dale 1995).
#'   \deqn{64 - (0.95 \times 100 \times \frac{n_{wd}}{n_{w}}) - (0.69 \times ASL)}{
#'   64 - (0.95 * 100 * Nwd / Nw) - (0.69 * ASL)}}
#'
#'   \item{`"Dale.Chall.Old"`:}{The original Dale-Chall Readability formula
#'   (Dale and Chall (1948).
#'   \deqn{0.1579 \times 100 \times \frac{n_{wd}}{n_{w}} + 0.0496 \times ASL [+ 3.6365]}{
#'   0.1579 * 100 * Nwd / Nw  + 0.0496 * ASL [+ 3.6365]}
#'
#'   The additional constant 3.6365 is only added if (Nwd / Nw) > 0.05.}
#'
#'   \item{`"Dale.Chall.PSK"`:}{The Powers-Sumner-Kearl Variation of the
#'   Dale and Chall Readability formula (Powers, Sumner and Kearl, 1958).
#'   \deqn{0.1155 \times
#'   100 \frac{n_{wd}}{n_{w}}) + (0.0596 \times ASL) + 3.2672 }{
#'   (0.1155 * 100 * Nwd / Nw) + (0.0596 * ASL) + 3.2672}}
#'
#'   \item{`"Danielson.Bryan"`:}{Danielson-Bryan's (1963) Readability Measure 1. \deqn{
#'   (1.0364 \times \frac{n_{c}}{n_{blank}}) +
#'   (0.0194 \times \frac{n_{c}}{n_{st}}) -
#'   0.6059}{(1.0364 * Nc / Nblank) +
#'   (0.0194 * Nc / Nst) - 0.6059}
#'
#'   where \eqn{n_{blank}} = Nblank = the number of blanks.}
#'
#'   \item{`"Danielson.Bryan2"`:}{Danielson-Bryan's (1963) Readability Measure 2. \deqn{
#'   131.059- (10.364 \times \frac{n_{c}}{n_{blank}}) + (0.0194
#'    \times \frac{n_{c}}{n_{st}})}{131.059 - (10.364 * Nc /
#'    Nblank) + (0.0194 * Nc / Nst)}
#'
#'    where \eqn{n_{blank}} = Nblank = the number of blanks.}
#'
#'   \item{`"Dickes.Steiwer"`:}{Dickes-Steiwer Index (Dicks and Steiwer 1977). \deqn{
#'   235.95993 - (7.3021 \times AWL)  - (12.56438 \times ASL) -
#'   (50.03293 \times TTR)}{235.95993 - (73.021 *
#'   AWL) - (12.56438 * ASL) - (50.03293 * TTR)}
#'
#'   where TTR is the Type-Token Ratio (see [textstat_lexdiv()])}
#'
#'   \item{`"DRP"`:}{Degrees of Reading Power. \deqn{(1 - Bormuth.MC) *
#'   100}
#'
#'   where Bormuth.MC refers to Bormuth's (1969)  Mean Cloze Formula (documented above)}
#'
#'   \item{`"ELF"`:}{Easy Listening Formula (Fang 1966): \deqn{\frac{n_{wsy>=2}}{n_{st}}}{(Nwmin2sy / Nst)}
#'
#'    where \eqn{n_{wsy>=2}} = Nwmin2sy = the number of words with 2 syllables or more.}
#'
#'   \item{`"Farr.Jenkins.Paterson"`:}{Farr-Jenkins-Paterson's
#'   Simplification of Flesch's Reading Ease Score (Farr, Jenkins and Paterson 1951). \deqn{
#'    -31.517 - (1.015 \times ASL) + (1.599 \times
#'   \frac{n_{wsy=1}}{n_{w}}}{ -31.517
#'   - (1.015 * ASL) + (1.599 * Nwsy1 / Nw)}
#'
#'   where \eqn{n_{wsy=1}} = Nwsy1 = the number of one-syllable words.}
#'
#'   \item{`"Flesch"`:}{Flesch's Reading Ease Score (Flesch 1948).
#'   \deqn{206.835 - (1.015 \times ASL) - (84.6 \times \frac{n_{sy}}{n_{w}})}{
#'   206.835 - (1.015 * ASL) - (84.6 * (Nsy / Nw))}}
#'
#'   \item{`"Flesch.PSK"`:}{The Powers-Sumner-Kearl's Variation of Flesch Reading Ease Score
#'   (Powers, Sumner and Kearl, 1958). \deqn{ (0.0778 \times
#'   ASL) + (4.55 \times \frac{n_{sy}}{n_{w}}) -
#'   2.2029}{(0.0078 * ASL) + (4.55 * Nsy / Nw) - 2.2029}}
#'
#'   \item{`"Flesch.Kincaid"`:}{Flesch-Kincaid Readability Score (Flesch and Kincaid 1975). \deqn{
#'   0.39 \times ASL + 11.8  \times \frac{n_{sy}}{n_{w}} -
#'   15.59}{0.39 * ASL + 11.8  * (NSy /Nw) - 15.59}}
#'
#'   \item{`"FOG"`:}{Gunning's Fog Index (Gunning 1952). \deqn{0.4
#'   \times (ASL + 100 \times \frac{n_{wsy>=3}}{n_{w}})}{0.4 *
#'   (ASL + 100 * (Nwmin3sy / Nw)}
#'
#'   where \eqn{n_{wsy>=3}} = Nwmin3sy = the number of words with 3-syllables or more.
#'   The scaling by 100 arises because the original FOG index is based on
#'   just a sample of 100 words)}
#'
#'   \item{`"FOG.PSK"`:}{The Powers-Sumner-Kearl Variation of Gunning's
#'   Fog Index (Powers, Sumner and Kearl, 1958). \deqn{3.0680 \times
#'   (0.0877 \times ASL) +(0.0984 \times 100 \times \frac{n_{wsy>=3}}{n_{w}})}{
#'   3.0680 * (0.0877 * ASL) +(0.0984 * 100 * (Nwmin3sy / Nw)}
#'
#'   where \eqn{n_{wsy>=3}} = Nwmin3sy = the number of words with 3-syllables or more.
#'   The scaling by 100 arises because the original FOG index is based on
#'   just a sample of 100 words)}
#'
#'   \item{`"FOG.NRI"`:}{The Navy's Adaptation of Gunning's Fog Index (Kincaid, Fishburne, Rogers and Chissom 1975).
#'   \deqn{(\frac{(n_{wsy<3} + 3 \times n_{wsy=3})}{(100 \times \frac{N_{st}}{N_{w}})}  -
#'   3) / 2 }{(((Nwless3sy + 3 * Nw3sy) / (100 * Nst / Nw))-3) / 2}
#'
#'   where \eqn{n_{wsy<3}} = Nwless3sy = the number of words with *less than* 3 syllables, and
#'   \eqn{n_{wsy=3}} = Nw3sy = the number of 3-syllable words. The scaling by 100
#'   arises because the original FOG index is based on just a sample of 100 words)}
#'
#'   \item{`"FORCAST"`:}{FORCAST (Simplified Version of FORCAST.RGL) (Caylor and
#'   Sticht 1973). \deqn{ 20 - \frac{n_{wsy=1} \times
#'   150)}{(n_{w} \times 10)}}{ 20 - (Nwsy1 *
#'   150) / (Nw * 10)}
#'
#'   where \eqn{n_{wsy=1}} = Nwsy1 = the number of one-syllable words. The scaling by 150
#'   arises because the original FORCAST index is based on just a sample of 150 words.}
#'
#'   \item{`"FORCAST.RGL"`:}{FORCAST.RGL (Caylor and Sticht 1973).
#'   \deqn{20.43 - 0.11 \times \frac{n_{wsy=1} \times
#'   150)}{(n_{w} \times 10)}}{ 20.43 - 0.11 * (Nwsy1 *
#'   150) / (Nw * 10)}
#'
#'   where \eqn{n_{wsy=1}} = Nwsy1 = the number of one-syllable words. The scaling by 150 arises
#'   because the original FORCAST index is based on just a sample of 150 words.}
#'
#'   \item{`"Fucks"`:}{Fucks' (1955) Stilcharakteristik (Style
#'   Characteristic). \deqn{AWL * ASL}}
#'
#'   \item{`"Linsear.Write"`:}{Linsear Write (Klare 1975).
#'   \deqn{\frac{[(100 - (\frac{100 \times n_{wsy<3}}{n_{w}})) +
#'   (3 \times \frac{100 \times n_{wsy>=3}}{n_{w}})]}{(100 \times
#'   \frac{n_{st}}{n_{w}})}}{[(100 - (100 * Nwless3sy / Nw))
#'   + (3 * 100 * Nwmin3sy / Nw)] / (100 * Nst / Nw)}
#'
#'   where \eqn{n_{wsy<3}} = Nwless3sy = the number of words with *less than* 3 syllables, and
#'   \eqn{n_{wsy>=3}} = Nwmin3sy = the number of words with 3-syllables or more. The scaling
#'   by 100 arises because the original Linsear.Write measure is based on just a sample of 100 words)}
#'
#'
#'   \item{`"LIW"`:}{Björnsson's (1968) Läsbarhetsindex (For Swedish
#'   Texts). \deqn{ASL + \frac{100 \times n_{wsy>=7}}{n_{w}}}{ ASL + (100 *
#'   Nwmin7sy / Nw)}
#'
#'   where \eqn{n_{wsy>=7}} = Nwmin7sy = the number of words with 7-syllables or more. The scaling
#'   by 100 arises because the Läsbarhetsindex index is based on just a sample of 100 words)}
#'
#'   \item{`"nWS"`:}{Neue Wiener Sachtextformeln 1 (Bamberger and
#'   Vanecek 1984). \deqn{19.35 \times \frac{n_{wsy>=3}}{n_{w}} +
#'   0.1672 \times ASL + 12.97 \times \frac{b_{wchar>=6}}{n_{w}} - 3.27 \times
#'    \frac{n_{wsy=1}}{n_{w}} - 0.875}{(19.35 * Nwmin3sy / Nw) +
#'   (0.1672 * ASL) + (12.97 * Nwmin6char / Nw) - (3.27 * Nw1sy / Nw)- 0.875}
#'
#'   where \eqn{n_{wsy>=3}} = Nwmin3sy = the number of words with 3 syllables or more,
#'   \eqn{n_{wchar>=6}} = Nwmin6char = the number of words with 6 characters or more, and
#'   \eqn{n_{wsy=1}} = Nwsy1 = the number of one-syllable words.}
#'
#'   \item{`"nWS.2"`:}{Neue Wiener Sachtextformeln 2 (Bamberger and
#'   Vanecek 1984). \deqn{20.07 \times \frac{n_{wsy>=3}}{n_{w}} + 0.1682 \times ASL +
#'   13.73 \times \frac{n_{wchar>=6}}{n_{w}} - 2.779}{ (20.07 * Nwmin3sy / Nw) + (0.1682 * ASL) +
#'   (13.73 * Nwmin6char / Nw) - 2.779}
#'
#'   where \eqn{n_{wsy>=3}} = Nwmin3sy = the number of words with 3 syllables or more, and
#'   \eqn{n_{wchar>=6}} = Nwmin6char = the number of words with 6 characters or more.}
#'
#'   \item{`"nWS.3"`:}{Neue Wiener Sachtextformeln 3 (Bamberger and
#'   Vanecek 1984). \deqn{29.63 \times \frac{n_{wsy>=3}}{n_{w}} + 0.1905 \times
#'   ASL - 1.1144}{(29.63 * Nwmin3sy / Nw) + (0.1905 * ASL) - 1.1144}
#'
#'   where \eqn{n_{wsy>=3}} = Nwmin3sy = the number of words with 3 syllables or more.}
#'
#'   \item{`"nWS.4"`:}{Neue Wiener Sachtextformeln 4 (Bamberger and
#'   Vanecek 1984). \deqn{27.44 \times \frac{n_{wsy>=3}}{n_{w}} + 0.2656 \times
#'   ASL - 1.693}{ (27.44 * Nwmin3sy / Nw) + (0.2656 * ASL) - 1.693}
#'
#'   where \eqn{n_{wsy>=3}} = Nwmin3sy = the number of words with 3 syllables or more.}
#'
#'   \item{`"RIX"`:}{Anderson's (1983) Readability Index. \deqn{
#'   \frac{n_{wsy>=7}}{n_{st}}}{ Nwmin7sy / Nst}
#'
#'   where \eqn{n_{wsy>=7}} = Nwmin7sy = the number of words with 7-syllables or more.}
#'
#'   \item{`"Scrabble"`:}{Scrabble Measure. \deqn{Mean
#'   Scrabble Letter Values of All Words}.
#'   Scrabble values are for English.  There is no reference for this, as we
#'   created it experimentally.  It's not part of any accepted readability
#'   index!}
#'
#'   \item{`"SMOG"`:}{Simple Measure of Gobbledygook (SMOG) (McLaughlin 1969). \deqn{ 1.043
#'    \times \sqrt{n_{wsy>=3}} \times \frac{30}{n_{st}} + 3.1291}{1.043 * sqrt(Nwmin3sy
#'    * 30 / Nst) + 3.1291}
#'
#'   where \eqn{n_{wsy>=3}} = Nwmin3sy = the number of words with 3 syllables or more.
#'   This measure is regression equation D in McLaughlin's original paper.}
#'
#'   \item{`"SMOG.C"`:}{SMOG (Regression Equation C) (McLaughlin's 1969) \deqn{0.9986 \times
#'   \sqrt{Nwmin3sy \times \frac{30}{n_{st}} +
#'   5} +  2.8795}{0.9986 * sqrt(Nwmin3sy * (30 / Nst) +
#'   5) + 2.8795}
#'
#'   where \eqn{n_{wsy>=3}} = Nwmin3sy = the number of words with 3 syllables or more.
#'   This measure is regression equation C in McLaughlin's original paper.}
#'
#'   \item{`"SMOG.simple"`:}{Simplified Version of McLaughlin's (1969) SMOG Measure. \deqn{
#'   \sqrt{Nwmin3sy \times \frac{30}{n_{st}}} +
#'   3}{sqrt(Nwmin3sy * 30 / Nst) + 3}}
#'
#'   \item{`"SMOG.de"`:}{Adaptation of McLaughlin's (1969) SMOG Measure for German Texts.
#'   \deqn{ \sqrt{Nwmin3sy \times \frac{30}{n_{st}}-2}}{
#'   sqrt(Nwmin3sy * 30 / Nst) - 2 }}
#'
#'   \item{`"Spache"`:}{Spache's (1952) Readability Measure. \deqn{ 0.121 \times
#'   ASL + 0.082 \times \frac{n_{wnotinspache}}{n_{w}}  +
#'   0.659}{ 0.121 * ASL + 0.082 * Nwnotinspache / Nw) + 0.659}
#'
#'   where \eqn{n_{wnotinspache}} = Nwnotinspache = number of unique words not in the Spache word list.}
#'
#'   \item{`"Spache.old"`:}{Spache's (1952) Readability Measure (Old). \deqn{0.141
#'   \times ASL + 0.086 \times \frac{n_{wnotinspache}}{n_{w}}  +
#'   0.839}{0.141 * ASL + 0.086 * (Nwnotinspache/ Nw) + 0.839}
#'
#'   where \eqn{n_{wnotinspache}} = Nwnotinspache = number of unique words not in the Spache word list.}
#'
#'   \item{`"Strain"`:}{Strain Index (Solomon 2006). \deqn{n_{sy} /
#'   \frac{n_{st}}{3} /10}{Nsy / (Nst / 3) / 10 }
#'
#'   The scaling by 3 arises because the original Strain index is based on just the first 3 sentences.}
#'
#'   \item{`"Traenkle.Bailer"`:}{Tränkle & Bailer's (1984) Readability Measure 1.
#'   \deqn{224.6814 - (79.8304 \times AWL) - (12.24032 \times
#'   ASL) - (1.292857 \times 100 \times \frac{n_{prep}}{n_{w}}}{ 224.6814 - (79.8304 * AWL) + (12.24032 * ASL) -
#'   (1.292857 * 100 * Nprep / Nw)}
#'
#'   where \eqn{n_{prep}} = Nprep = the number of prepositions. The scaling by 100 arises because the original
#'   Tränkle & Bailer index is based on just a sample of 100 words.}
#'
#'   \item{`"Traenkle.Bailer2"`:}{Tränkle & Bailer's (1984) Readability Measure 2.
#'   \deqn{Tränkle.Bailer2 =  234.1063 - (96.11069 \times AWL
#'   ) - (2.05444 \times 100 \times \frac{n_{prep}}{n_{w}}) -
#'   (1.02805 \times 100 \times \frac{n_{conj}}{n_{w}}}{
#'   234.1063 - 96.11069 * AWL  - 2.05444 * 100 * (Nprep / Nw) - 1.02805 * 100 * (Nconj / Nw).}
#'
#'   where \eqn{n_{prep}} = Nprep = the number of prepositions,
#'   \eqn{n_{conj}} = Nconj = the number of conjunctions,
#'   The scaling by 100 arises because the original Tränkle & Bailer index is based on
#'   just a sample of 100 words)}
#'
#'   \item{`"Wheeler.Smith"`:}{Wheeler & Smith's (1954) Readability Measure.
#'   \deqn{ ASL \times 10 \times \frac{n_{wsy>=2}}{n_{words}}}{ ASL * 10 * (Nwmin2sy / Nw)}
#'
#'    where \eqn{n_{wsy>=2}} = Nwmin2sy = the number of words with 2 syllables or more.}
#'
#'   \item{`"meanSentenceLength"`:}{Average Sentence Length (ASL).
#'   \deqn{\frac{n_{w}}{n_{st}}}{ Nw / Nst }}
#'
#'   \item{`"meanWordSyllables"`:}{Average Word Syllables (AWL).
#'   \deqn{\frac{n_{sy}}{n_{w}}}{ Nsy / Nw}}
#'
#' }
#'
#' @param x a character or [corpus] object containing the texts
#' @param measure character vector defining the readability measure to calculate.
#'   Matches are case-insensitive.  See other valid measures under Details.
#' @param remove_hyphens if `TRUE`, treat constituent words in hyphenated as
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
#'   including through pattern-matching, using [corpus_trim()].
#' @param intermediate if `TRUE`, include intermediate quantities in the output
#' @param ... not used
#' @author Kenneth Benoit, re-engineered from Meik Michalke's \pkg{koRpus}
#'   package.
#' @return `textstat_readability` returns a data.frame of documents and
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
#'   Anderson, J. (1983). [Lix and rix: Variations on a little-known readability
#'   index](https://www.jstor.org/stable/40031755). *Journal of Reading*, 26(6),
#'   490--496.
#'
#'   Bamberger, R. & Vanecek, E. (1984). *Lesen-Verstehen-Lernen-Schreiben*.
#'   Wien: Jugend und Volk.
#'
#'   Björnsson, C. H. (1968). *Läsbarhet*. Stockholm: Liber.
#'
#'   Bormuth, J.R. (1969). [Development of Readability
#'   Analysis](https://files.eric.ed.gov/fulltext/ED029166.pdf).
#'
#'   Bormuth, J.R. (1968). [Cloze test readability: Criterion reference
#'   scores](https://www.jstor.org/stable/1433978). *Journal of educational
#'   measurement*, 5(3), 189--196.
#'
#'   Caylor, J.S. (1973). [Methodologies for Determining Reading Requirements of
#'   Military Occupational Specialities](https://eric.ed.gov/?id=ED074343).
#'
#'   Caylor, J.S. & Sticht, T.G. (1973). [*Development of a Simple Readability
#'   Index for Job Reading
#'   Material*](https://archive.org/details/ERIC_ED076707).
#'
#'   Coleman, E.B. (1971). Developing a technology of written instruction: Some
#'   determiners of the complexity of prose. *Verbal learning research and the
#'   technology of written instruction*, 155--204.
#'
#'   Coleman, M. & Liau, T.L. (1975). [A Computer Readability Formula Designed
#'   for Machine Scoring](https://psycnet.apa.org/record/1975-22007-001).
#'   *Journal of Applied Psychology*, 60(2), 283.
#'
#'   Dale, E. and Chall, J.S. (1948). [A Formula for Predicting Readability:
#'   Instructions](https://www.jstor.org/stable/1473169). *Educational Research
#'   Bulletin*, 37-54.
#'
#'   Chall, J.S. and Dale, E. (1995). *Readability Revisited: The New Dale-Chall
#'   Readability Formula*. Brookline Books.
#'
#'   Dickes, P. & Steiwer, L. (1977). Ausarbeitung von Lesbarkeitsformeln für
#'   die Deutsche Sprache. *Zeitschrift für Entwicklungspsychologie und
#'   Pädagogische Psychologie* 9(1), 20--28.
#'
#'   Danielson, W.A., & Bryan, S.D. (1963). [Computer Automation of Two
#'   Readability
#'   Formulas](https://journals.sagepub.com/doi/abs/10.1177/107769906304000207).
#'   *Journalism Quarterly*, 40(2), 201--206.
#'
#'   DuBay, W.H. (2004). [*The Principles of
#'   Readability*](http://www.impact-information.com/impactinfo/readability02.pdf).
#'
#'   Fang, I. E. (1966). [The "Easy listening
#'   formula"](https://www.tandfonline.com/doi/abs/10.1080/08838156609363529?journalCode=hbem19).
#'   *Journal of Broadcasting & Electronic Media*, 11(1), 63--68.
#'
#'   Farr, J. N., Jenkins, J.J., & Paterson, D.G. (1951). [Simplification of
#'   Flesch Reading Ease
#'   Formula](https://psycnet.apa.org/record/1952-03973-001). *Journal of
#'   Applied Psychology*, 35(5): 333.
#'
#'   Flesch, R. (1948). [A New Readability
#'   Yardstick](https://psycnet.apa.org/record/1949-01274-001). *Journal of
#'   Applied Psychology*, 32(3), 221.
#'
#'   Fucks, W. (1955). Der Unterschied des Prosastils von Dichtern und anderen
#'   Schriftstellern. *Sprachforum*, 1, 233-244.
#'
#'   Gunning, R. (1952). *The Technique of Clear Writing*.  New York:
#'   McGraw-Hill.
#'
#'   Klare, G.R. (1975). [Assessing
#'   Readability](https://www.jstor.org/stable/747086). *Reading Research
#'   Quarterly*, 10(1), 62-102.
#'
#'   Kincaid, J. P., Fishburne Jr, R.P., Rogers, R.L., & Chissom, B.S. (1975).
#'   [Derivation of New Readability Formulas (Automated Readability Index, FOG
#'   count and Flesch Reading Ease Formula) for Navy Enlisted
#'   Personnel](https://stars.library.ucf.edu/istlibrary/56/).
#'
#'   McLaughlin, G.H. (1969). [SMOG Grading: A New Readability
#'   Formula.](https://ogg.osu.edu/media/documents/health_lit/WRRSMOG_Readability_Formula_G._Harry_McLaughlin__1969_.pdf)
#'   *Journal of Reading*, 12(8), 639-646.
#'
#'   Powers, R.D., Sumner, W.A., and Kearl, B.E. (1958). [A Recalculation of
#'   Four Adult Readability
#'   Formulas.](https://psycnet.apa.org/record/1962-03617-001). *Journal of
#'   Educational Psychology*, 49(2), 99.
#'
#'   Senter, R. J., & Smith, E. A. (1967). [Automated readability
#'   index.](https://apps.dtic.mil/dtic/tr/fulltext/u2/667273.pdf)
#'   Wright-Patterson Air Force Base. Report No. AMRL-TR-6620.
#'
#'   *Solomon, N. W. (2006). *Qualitative Analysis of Media Language*. India.
#'
#'   Spache, G. (1953). ["A new readability formula for primary-grade reading
#'   materials."](https://www.jstor.org/stable/998915) *The Elementary School
#'   Journal*, 53, 410--413.
#'
#'   Tränkle, U. & Bailer, H. (1984). Kreuzvalidierung und Neuberechnung von
#'   Lesbarkeitsformeln für die deutsche Sprache. *Zeitschrift für
#'   Entwicklungspsychologie und Pädagogische Psychologie*, 16(3), 231--244.
#'
#'   Wheeler, L.R. & Smith, E.H. (1954). [A Practical Readability Formula for
#'   the Classroom Teacher in the Primary
#'   Grades](https://www.jstor.org/stable/41384251). *Elementary English*, 31,
#'   397--399.
#'
#'   *Nimaldasan is the pen name of N. Watson Solomon, Assistant Professor of
#'   Journalism, School of Media Studies, SRM University, India.
#'
textstat_readability <- function(x,
                                 measure = "Flesch",
                                 remove_hyphens = TRUE,
                                 min_sentence_length = 1,
                                 max_sentence_length = 10000,
                                 intermediate = FALSE, ...) {
    UseMethod("textstat_readability")
}

#' @export
textstat_readability.default <- function(x,
                                        measure = "Flesch",
                                        remove_hyphens = TRUE,
                                        min_sentence_length = 1,
                                        max_sentence_length = 10000,
                                        intermediate = FALSE, ...) {
    stop(friendly_class_undefined_message(class(x), "textstat_readability"))
}

#' @importFrom stringi stri_length
#' @export
textstat_readability.corpus <- function(x,
                                        measure = "Flesch",
                                        remove_hyphens = TRUE,
                                        min_sentence_length = 1,
                                        max_sentence_length = 10000,
                                        intermediate = FALSE, ...) {

    unused_dots(...)

    measure_option <- c("ARI", "ARI.simple", "ARI.NRI",
                        "Bormuth", "Bormuth.MC", "Bormuth.GP",
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
    accepted_measures <- c(measure_option, "Bormuth", "Coleman.Liau")

    if (measure[1] == "all") {
        measure <- measure_option
    } else {
        is_valid <- measure %in% accepted_measures
        if (!all(is_valid))
            stop("Invalid measure(s): ", measure[!is_valid])
    }

    if ("Bormuth" %in% measure) {
        measure[measure == "Bormuth"] <- "Bormuth.MC"
        measure <- unique(measure)
    }

    if ("Coleman.Liau" %in% measure) {
        measure[measure == "Coleman.Liau"] <- "Coleman.Liau.ECP"
        measure <- unique(measure)
    }

    x <- texts(x)
    if (!is.null(min_sentence_length) || !is.null(max_sentence_length)) {
        x <- char_trim(x, "sentences",
                       min_ntoken = min_sentence_length,
                       max_ntoken = max_sentence_length)
    }

    # get sentence lengths - BEFORE lower-casing
    n_sent <- nsentence(x)

    # get the word length and syllable info for use in computing quantities
    x <- char_tolower(x)
    toks <- tokens(x, remove_punct = TRUE, split_hyphens = remove_hyphens)

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
        Bl <- Traenkle.Bailer <- Traenkle.Bailer.2 <- Bormuth <- Bormuth.MC.Temp <- Coleman.Liau.ECP.Temp <-
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
    if (any(c("Dale.Chall", "Dale.Chall.old", "Dale.Chall.PSK", "Bormuth.MC", "Bormuth.GP", "DRP") %in% measure)) {
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
        temp[, Bormuth.MC := 0.886593 - (0.08364 * C / W) + 0.161911 * (W_wl.Dale.Chall / W) ^ 3 -
                 0.21401 * (W / St) + 0.000577 * (W / St) ^ 2 - 0.000005 * (W / St) ^ 3]
    }
    if ("Bormuth.GP" %in% measure) {
        CCS <- 35 # Cloze criterion score, percent as integer
        temp[, Bormuth.MC.Temp := 0.886593 - (0.08364 * C / W) + 0.161911 *
                 (W_wl.Dale.Chall / W) ^ 3 - 0.21401 * (W / St) + 0.000577 *
                 (W / St) ^ 2 - 0.000005 * (W / St) ^ 3]
        temp[, Bormuth.GP := 4.275 +
                 12.881 * Bormuth.MC.Temp -
                 (34.934 * Bormuth.MC.Temp^2) +
                 (20.388 * Bormuth.MC.Temp^3) +
                 (26.194 * C - 2.046 * CCS ^ 2) - (11.767 * CCS ^ 3) -
                 (44.285 * Bormuth.MC.Temp * CCS) +
                 (97.620 * (Bormuth.MC.Temp * CCS)^2) -
                 (59.538 * (Bormuth.MC.Temp * CCS)^3)]
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
        temp[, Bormuth.MC.Temp := 0.886593 - (0.08364 * C / W) + 
                 0.161911 * (W_wl.Dale.Chall / W) ^ 3 -
                 0.21401 * (W / St) +
                 0.000577 * (W / St) ^ 2 - 0.000005 * (W / St) ^ 3]
        temp[, DRP := (1 - Bormuth.MC.Temp) * 100]
        temp[, Bormuth.MC.Temp := NULL]
    }

    if ("ELF" %in% measure)
        temp[, ELF := W2Sy / St]

    if ("Farr.Jenkins.Paterson" %in% measure)
        temp[, Farr.Jenkins.Paterson := -31.517 - 1.015 * W / St + 1.599 * W_1Sy / W]

    if ("Flesch" %in% measure)
        temp[, Flesch := 206.835 - 1.015 * W / St - 84.6 * Sy / W]

    if ("Flesch.PSK" %in% measure)
        temp[, Flesch.PSK := 0.0778 * W / St + 4.55 * Sy / W - 2.2029]

    if ("Flesch.Kincaid" %in% measure)
        temp[, Flesch.Kincaid := 0.39 * W / St + 11.8 * Sy / W - 15.59]

    if ("meanSentenceLength" %in% measure)
        temp[, meanSentenceLength := W / St]

    if ("meanWordSyllables" %in% measure)
        temp[, meanWordSyllables := Sy / W]

    if ("FOG" %in% measure)
        temp[, FOG := 0.4 * (W / St + 100 * W3Sy / W)]
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
        temp[, Linsear.Write := ((100 - (100 * Wlt3Sy) / W) + (3 * 100 * W3Sy / W)) / (100 * St / W)]

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
        temp[, Strain := Sy * 1 / (St / 3) / 10]

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
                                           measure = "Flesch",
                                           remove_hyphens = TRUE,
                                           min_sentence_length = 1,
                                           max_sentence_length = 10000, ...) {

    textstat_readability(corpus(x), measure, remove_hyphens,
                         min_sentence_length, max_sentence_length, ...)
}

conjunctions <- c("for", "and", "nor", "but", "or", "yet", "so")
prepositions <- c("a", "abaft", "abeam", "aboard", "about", "above", "absent",
                  "across", "afore", "after", "against", "along", "alongside",
                  "amid", "amidst", "among", "amongst", "an", "anenst", "apropos",
                  "apud", "around", "as", "aside", "astride", "at", "athwart", "atop",
                  "barring", "before", "behind", "below", "beneath", "beside", "besides",
                  "between", "beyond", "but", "by", "chez", "circa", "ca", "c",
                  "concerning", "despite", "down", "during", "except",
                  "excluding", "failing", "following", "for", "forenenst", "from",
                  "given", "in", "including", "inside", "into",
                  "like", "mid", "midst", "minus", "modulo", "near", "next",
                  "notwithstanding", "o'", "of", "off", "on", "onto",
                  "opposite", "out", "outside", "over", "pace", "past", "per", "plus",
                  "pro", "qua", "regarding", "round", "sans", "save", "since", "than",
                  "through", "thru", "throughout", "thruout", "times", "to", "toward",
                  "towards", "under", "underneath", "unlike", "until", "unto", "up",
                  "upon", "versus", "vs", "v", "via", "vis-a-vis", "with", "within",
                  "without", "worth")

#' @rdname data-internal
#' @details
#' `data_char_wordlists` provides word lists used in some readability indexes;
#' it is a named list of character vectors where each list element
#' corresponds to a different readability index.
#'
#' These are:
#' \describe{
#' \item{`DaleChall`}{The long Dale-Chall list of 3,000 familiar (English)
#' words needed to compute the Dale-Chall Readability Formula.}
#' \item{`Spache`}{The revised Spache word list (see Klare 1975, 73) needed
#' to compute the Spache Revised Formula of readability (Spache 1974.}
#' }
#' @references
#' Chall, J.S., & Dale, E. (1995). *Readability Revisited: The New
#' Dale-Chall Readability Formula*. Brookline Books.
#'
#' Dale, E. & Chall, J.S. (1948). A Formula for Predicting
#' Readability. *Educational Research Bulletin*, 27(1): 11--20.
#'
#' Dale, E. & Chall, J.S. (1948). A Formula for Predicting Readability:
#' Instructions. *Educational Research Bulletin*, 27(2): 37--54.
#'
#' Klare, G.R. (1975). Assessing Readability. *Reading Research Quarterly*
#' 10(1), 62--102.
#'
#' Spache, G. (1953). A New Readability Formula for Primary-Grade Reading
#' Materials. *The Elementary School Journal*, 53, 410--413.
#'
#' Tränkle, U. & Bailer, H. (1984). Kreuzvalidierung und Neuberechnung von
#' Lesbarkeitsformeln für die deutsche Sprache. *Zeitschrift für
#' Entwicklungspsychologie und Pädagogische Psychologie*, 16(3), 231--244.
#'
#' Wheeler, L.R. & Smith, E.H. (1954). A Practical Readability Formula for the
#' Classroom Teacher in the Primary Grades. *Elementary English*, 31, 
#' 397--399.
"data_char_wordlists"
