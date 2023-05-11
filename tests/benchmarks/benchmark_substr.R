require(quanteda)
require(stringi)

txt <- rep(stri_rand_strings(1000, 100, "[一-龠]"), 10000)

identical(cpp_substr(txt, 5, 1),
          stri_sub(txt, 1, 5))

microbenchmark::microbenchmark(
    quanteda:::cpp_substr(txt, 5, 1),
    #quanteda:::cpp_substr(txt, 50, 2),
    stri_sub(txt, 1, 5),
    stri_sub(txt, 5, -1),
    times = 10
)

