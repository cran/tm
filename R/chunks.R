makeChunks <- function(corpus, chunksize) {
    chunks <- list()
    for (k in seq_along(corpus)) {
        s <- c(seq(1, length(corpus[[k]]),
                   chunksize),
               length(corpus[[k]])+1)
        for (i in 1:(length(s)-1)) {
            chunks <- c(chunks,
                        list(corpus[[k]][s[i]:(s[i+1]-1)]))
        }
    }
    Corpus(VectorSource(chunks))
}
