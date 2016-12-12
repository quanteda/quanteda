

txt <- readLines('~/Documents/Brexit/Analysis/all_bbc_2015.txt') # 80MB
cop <- corpus(txt)
cop_zip <- corpuszip(txt)

s_org <- object.size(txt)
object.size(cop) / s_org
object.size(cop_zip) / s_org

microbenchmark::microbenchmark(
    texts(cop_zip),
    times=10
)
