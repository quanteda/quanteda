library(quanteda)

x <- "this is a test sentence plugged in as a test sentence should be and is a fine example"
collocations(x, method="all")

# create a data.table of all adjacent bigrams
t <- unlist(tokenize(x), use.names=FALSE)
wordpairs <- data.table(w1 = t[1:(length(t)-1)], 
                        w2 = t[2:length(t)])
wordpairs

# table for "test sentence" collocation
(mytable <- with(wordpairs, table(w1=="test", w2=="sentence")))
# FALSE TRUE
# FALSE    15    0
# TRUE      0    2
chisq.test(mytable, correct=FALSE)
Deducer::likelihood.test(mytable, conservative=FALSE)

## manual computation of expected values for tables
2*2/17
# [1] 0.2352941
15*2/17
# [1] 1.764706
15*2/17
#[1] 1.764706
15*15/17
#[1] 13.23529


# for "is a" collocation
chisq.test(with(wordpairs, table(w1=="is", w2=="a")), correct=FALSE)
Deducer::likelihood.test(with(wordpairs, table(w1=="is", w2=="a")), conservative=FALSE)

# # python's Chi^2
# [(('test', 'sentence'), 18.0), 
#  (('be', 'and'), 18.0), 
#  (('fine', 'example'), 18.0), 
#  (('in', 'as'), 18.0), 
#  (('plugged', 'in'), 18.0), 
#  (('should', 'be'), 18.0), 
#  (('a', 'test'), 11.25), 
#  (('is', 'a'), 11.25), 
#  (('and', 'is'), 8.470588235294118), 
#  (('sentence', 'plugged'), 8.470588235294118), 
#  (('sentence', 'should'), 8.470588235294118), 
#  (('this', 'is'), 8.470588235294118), 
#  (('a', 'fine'), 5.294117647058823), 
#  (('as', 'a'), 5.294117647058823)]
# 
# # python's lr ratio
# [(('test', 'sentence'), 25.115910900698296), 
#  (('a', 'test'), 17.477740881160543), 
#  (('is', 'a'), 17.477740881160543), 
#  (('be', 'and'), 15.44825917270115), 
#  (('fine', 'example'), 15.44825917270115), 
#  (('in', 'as'), 15.44825917270115), 
#  (('plugged', 'in'), 15.44825917270115), 
#  (('should', 'be'), 15.44825917270115), 
#  (('and', 'is'), 9.903081728221604), 
#  (('sentence', 'plugged'), 9.903081728221604), 
#  (('sentence', 'should'), 9.903081728221604), 
#  (('this', 'is'), 9.903081728221604), 
#  (('a', 'fine'), 7.8100891531634105), 
#  (('as', 'a'), 7.8100891531634105)]

