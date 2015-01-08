library(rJava)
.jinit()
#.jaddClassPath('/home/paul/ArabicStemmerCode')

.jaddClassPath('/home/paul/ArabicStemmerCode')
#.jaddClassPath('/home/paul/Desktop/astem.jar')
.jclassPath()

as <- .jnew("Stem")
