
rm(list=ls())
library(quanteda)

mydir <- directory("~/Dropbox/QUANTESS/corpora/UDHR")

# text vector
UDHRtexts <- getTextDir(mydir, enc="UTF-8")
UDHRtexts <- UDHRtexts[-6]  # remove the Georgian text
str(UDHRtexts)
names(UDHRtexts) <- gsub(".txt", "", names(UDHRtexts))
 
# make a corpus with encoding set
UDHRcorpus <- corpus(UDHRtexts, enc="UTF-8")
# this should have happened automatically with the corpus call??
metadoc(UDHRcorpus, "language") <- sapply(strsplit(docnames(UDHRcorpus), "_"), "[[", 2)
metadoc(UDHRcorpus)

metadoc(UDHRcorpus, "language") <- "Testing"
metadoc(UDHRcorpus)
metadoc(UDHRcorpus)$"_language"[1] <- "Replacement2"
metadoc(UDHRcorpus)


# make some new encodings - add a new one for Chinese
documents(UDHRcorpus) <- rbind(documents(UDHRcorpus), documents(UDHRcorpus)[1,])
texts(UDHRcorpus)[ndoc(UDHRcorpus)] <- iconv(texts(UDHRcorpus)[ndoc(UDHRcorpus)], "UTF-8", "CHINESE")
encoding(UDHRcorpus)[ndoc(UDHRcorpus)] <- "CHINESE"

# two more encodings for Czech
documents(UDHRcorpus) <- rbind(documents(UDHRcorpus), documents(UDHRcorpus)[2,])
texts(UDHRcorpus)[ndoc(UDHRcorpus)] <- iconv(texts(UDHRcorpus)[ndoc(UDHRcorpus)], "UTF-8", "LATIN2")
encoding(UDHRcorpus)[ndoc(UDHRcorpus)] <- "LATIN2"
documents(UDHRcorpus) <- rbind(documents(UDHRcorpus), documents(UDHRcorpus)[2,])
texts(UDHRcorpus)[ndoc(UDHRcorpus)] <- iconv(texts(UDHRcorpus)[ndoc(UDHRcorpus)], "UTF-8", "WINDOWS-1250")
encoding(UDHRcorpus)[ndoc(UDHRcorpus)] <- "WINDOWS-1250"

# for Danish
documents(UDHRcorpus) <- rbind(documents(UDHRcorpus), documents(UDHRcorpus)[2,])
texts(UDHRcorpus)[ndoc(UDHRcorpus)] <- iconv(texts(UDHRcorpus)[ndoc(UDHRcorpus)], "UTF-8", "LATIN1")
encoding(UDHRcorpus)[ndoc(UDHRcorpus)] <- "LATIN1"


# UDHR_chinese        UTF-8    chinese  CHINESE
# UDHR_czech          UTF-8      czech  LATIN2, WINDOWS-1250
# UDHR_danish         UTF-8     danish  LATIN1
# UDHR_english        UTF-8    english  MAC WINDOWS-1252
# UDHR_french         UTF-8     french  IBM437
# UDHR_greek          UTF-8      greek  GREEK, MACGREEK, WINDOWS-1253, ISO-8859-7
# UDHR_hungarian      UTF-8  hungarian  LATIN2, MACCENTRALEUROPE, WINDOWS-1250, 
# UDHR_icelandic      UTF-8  icelandic  LATIN1
# UDHR_irish          UTF-8      irish  LATIN1
# UDHR_japanese       UTF-8   japanese  MS_KANJI SHIFT-JIS
# UDHR_russian        UTF-8    russian  WINDOWS-1251 ISO-8859-5
# UDHR_vietnamese     UTF-8 vietnamese  CP1258 VISCII
# UDHR_chinese1       UTF-8    chinese  CP950
# 



