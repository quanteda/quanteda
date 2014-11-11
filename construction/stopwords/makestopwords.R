# from the tm project, which takes them from:

# Available stopword lists are:
#     
#     SMART English stopwords from the SMART information retrieval
# system (obtained from
#         http://jmlr.csail.mit.edu/papers/volume5/lewis04a/a11-smart-stop-list/english.stop)
# (which coincides with the stopword list used by the MC toolkit
#  (http://www.cs.utexas.edu/users/dml/software/mc/)), catalan Catalan
# stopwords (obtained from
#            http://latel.upf.edu/morgana/altres/pub/ca_stop.htm),
# 
# and a set of stopword lists from the Snowball stemmer project in
# different languages (obtained from
#                      http://svn.tartarus.org/snowball/trunk/website/algorithms/*/stop.txt). Supported
# languages are danish, dutch, english, finnish, french, german,
# hungarian, italian, norwegian, portuguese, russian, spanish, and
# swedish. Language names are case sensitive. Alternatively, their IETF
# language tags may be used.

filenames <- c("SMART.dat", "danish.dat", "english.dat", "french.dat",
               "hungarian.dat", "norwegian.dat", "russian.dat",
               "swedish.dat", "catalan.dat", "dutch.dat", "finnish.dat", "german.dat",
               "italian.dat", "portuguese.dat", "spanish.dat")

stopwords <- list()
for (i in 1:length(filenames)) {
    thisfile <- filenames[i]
    print(thisfile)
    stopwords[[i]] <-  scan(paste("stopwords/", thisfile, sep=""), what="char", encoding="UTF-8")
}
names(stopwords) <- gsub(".dat", "", filenames)

save(stopwords, file="data/stopwords.RData")
