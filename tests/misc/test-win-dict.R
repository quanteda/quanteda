require(quanteda)

x <- list(one = c("a", "b"), two = c("d", "e"))
y <- list(one = c("a", "b"), two = c("d", "e"), three = list(sub1 = "s1", sub2 = c("s2a", "s2b")))

dx <- dictionary(x)
dy <- dictionary(y)

dx
dx$one

dy
dy$one
dy$three
