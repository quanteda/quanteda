require(stringi)
lis <- replicate(10, stri_rand_strings(10, sample(1:3, 10, replace = TRUE), 
                                       "[A-Za-z0-9\u3042-\u3093]"),
                 simplify = FALSE)

test_that("cpp_serialize works", {
    
    for (h in seq_along(lis)) {
        if (h == 1) {
            xtoks <- quanteda:::cpp_serialize(lis[h])
        } else {
            xtoks <- quanteda:::cpp_serialize_add(lis[h], xtoks)
        }
    }
    out1 <- unclass(quanteda:::cpp_as_list(xtoks))
    
    type <- unique(unlist(lis, use.names = FALSE))
    out2 <- structure(lapply(lis, function(x) {match(x, type)}), 
                      types = type)
    
    expect_identical(out1, out2)
})

