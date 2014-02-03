# attempt to improve sentence syllable count efficiency

library(quanteda)

load("~/Dropbox/QUANTESS/FOR PAUL 2012-11-30/sentences.RData")

#if i don't declare the encoding like this nchar fails("invalid multibyte string)
Encoding(sentences$sentence_text) <- "UTF-8"

countSyllables <- function(sourceText) {
  # load the RData file but only if not already loaded!
  # note that data() defaults to .Globalenv
  if (!exists(as.character(substitute(counts)))) {
    data(syllableCounts)
    print("loaded: syllableCounts")
  }
  # clean the string, change to uppercase for syllable dictionary match
  string <- gsub("[[:punct:][:digit:]]", "", sourceText)
  string <- gsub("\n", "", string)
  string <- toupper(string)
  words <- unlist(strsplit(string, " "))
  # lookup the syllables in the words found in the dictionary
  # uses vectorization and named vector indexing - not looping!
  n.syllables <- counts[words]
  # name the syllable count vector with the words
  names(n.syllables) <- words
  # count the syllables in each word?
  vowel.count.lookup <- sapply(words, function(l) sum((attr(gregexpr("[AEIOUY]*", l)[[1]], "match.length"))!=0))
  # replace the un-looked-up words with vowel formula words
  n.syllables[is.na(n.syllables)] <- 
    vowel.count.lookup[is.na(n.syllables)]
  return(sum(n.syllables))
}



rm(counts)
system.time(syllable_counts <- sapply(sentences$sentence_text[1:1000], countSyllables, USE.NAMES=FALSE))
system.time(syllable_counts <- sapply(sentences$sentence_text, countSyllables, USE.NAMES=FALSE))

# have to loop because of Java heap space error
time.start <- proc.time()
pos.df <- data.frame(table(determine.pos(sentences$sentence_text[1])))
pos.df <- cbind(sentences$sentenceid, pos.df)
names(pos.df) <- c("pos", sentences$sentenceid[1])
cat("Processing sentence: 1 ")
for (i in 2:nrow(sentences)) {
#for (i in 20:30) {
  cat(i, " ")         
  this.sentence <- data.frame(table(determine.pos(sentences$sentence_text[i])))
  # got to have this to catch any zero-length POS vectors
  if (nrow(this.sentence)==0) { this.sentence <- data.frame(Var1="DT", Freq=0) }
  pos.df <- merge(pos.df, this.sentence, by.x=1, by.y=1, all=TRUE)
  names(pos.df)[ncol(pos.df)] <- sentences$sentenceid[i]
}
row.names(pos.df) <- pos.df$pos
pos.df <- t(pos.df[,-1])
print(proc.time()-time.start)

save(pos.df, file="pos.df.Rdata")
pos.df[is.na(pos.df)] <- 0
#row.names(pos.df) <- 1:nrow(pos.df)

sentences.pos <- data.frame(sentenceid=sentences$sentenceid,
                            nouns=apply(pos.df[,c("NN", "NNS", "NNP", "NNPS")], 1, sum),
                            verbs=apply(pos.df[,c("VB", "VBD", "VBG", "VBN", "VBP", "VBZ")], 1, sum),
                            adjs=apply(pos.df[,c("JJR", "JJ", "JJS")], 1, sum),
                            advs=apply(pos.df[,c("RB", "RBR", "RBS")], 1, sum),
                            propnouns=apply(pos.df[,c("NNP", "NNPS")], 1, sum))


system.time(t <- sapply(sentences$sentence_text[1:100], determine.pos))
names(t) <- paste(sentences$sentenceid[1:10], ".", sep="")
ult <- unlist(t)
tdf <- data.frame(sentenceid=floor(as.numeric(names(ult))), 
                  pos=ult)
pos.df <- table(tdf$sentenceid, tdf$pos)

# interesting here that the vectorization is not really faster - and may not scale as well

load("pos.df")
row.names(pos.df)[2:nrow(pos.df)] <- gsub("s", "", row.names(pos.df)[2:nrow(pos.df)])

