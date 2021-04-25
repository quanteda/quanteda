test_that("corpus works with empty objects", {
  
  corp1 <- corpus(character(), docvars = data.frame(var1 = integer()))
  corp2 <- corpus(data.frame(text = character(), var1 = integer()))
  
})

test_that("corpus works with empty objects", {
  
toks1 <- tokens(character())
toks2 <- as.tokens(list())

})

corpus(corpus(character(), docvars = data.frame(var1 = integer())))
tokens(corp_empty)