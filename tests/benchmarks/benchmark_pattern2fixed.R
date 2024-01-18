require(stringi)
require(quanteda)
type <- stri_rand_strings(100000, 1:200)
microbenchmark::microbenchmark(
    v3 = quanteda3::object2fixed(data_dictionary_LSD2015[1:2], type),
    v4 = quanteda::object2fixed(data_dictionary_LSD2015[1:2], type),
    times = 1
)

microbenchmark::microbenchmark(
    v3 = quanteda3:::index_types(type, "glob", TRUE),
    v4 = quanteda:::index_types(unlist(data_dictionary_LSD2015[1:2]), type, "glob", TRUE)
)
