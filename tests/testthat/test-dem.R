library(quanteda)

x <- dfm(tokens(data_corpus_inaugural))
dvars <- quanteda:::get_docvars(x, user = TRUE, system = TRUE)
attrs <- attributes(x)

# dummy document embedding
k <- 50
mat <- matrix(rnorm(nrow(x) * k), ncol = k, 
              dimnames = list(docnames(x), NULL))

dem <- quanteda:::build_dem(mat, 
                            docvars = quanteda:::get_docvars(x, user = TRUE, system = TRUE), 
                            meta = attrs[["meta"]], class = "dem")

names(attributes(dem))
head(dem)
