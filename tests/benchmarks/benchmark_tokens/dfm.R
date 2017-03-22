txt <- rep(paste0(letters, collapse=' '), 10000)
toks <- tokens(txt, hash=FALSE)
toks_hash <- tokens(txt, hash=TRUE)

profvis(
  dfm(toks)
)
profvis(
  dfm(toks_hash)
)
