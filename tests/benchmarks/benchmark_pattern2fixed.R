require(stringi)
require(quanteda)
type <- stri_rand_strings(1000000, 1:200)
microbenchmark::microbenchmark(
    quanteda3::object2fixed(data_dictionary_LSD2015[1:2], type),
    quanteda::object2fixed(data_dictionary_LSD2015[1:2], type),
    times = 10
)
