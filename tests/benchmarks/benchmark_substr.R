require(quanteda)
require(stringi)

txt <- stri_rand_strings(1000000, 10, "[一-龠]")

identical(quanteda:::cpp_substr(txt, 5, 1),
          stri_sub(txt, 1, 5))

microbenchmark::microbenchmark(
    quanteda:::cpp_substr(txt, 5, 1),
    #quanteda:::cpp_substr(txt, 50, 2),
    stri_sub(txt, 1, 5),
    #stri_sub(txt, -5, -1)
    times = 100
)

