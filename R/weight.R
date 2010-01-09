# Author: Ingo Feinerer

WeightFunction <- function(x, name, acronym) {
    class(x) <- c("WeightFunction", "function")
    attr(x, "Name") <- name
    attr(x, "Acronym") <- acronym
    x
}

# Actual TermDocumentMatrix weighting functions
weightTf <- WeightFunction(identity, "term frequency", "tf")

weightTfIdf <-
    WeightFunction(function(m) {
        isDTM <- inherits(m, "DocumentTermMatrix")
        m$Weighting <- c("term frequency - inverse document frequency", "tf-idf")
        if (isDTM) m <- t(m)
        m <- m * log2(nDocs(m) / row_sums(m > 0))
        if (isDTM) t(m) else m
    }, "term frequency - inverse document frequency", "tf-idf")

weightBin <-
    WeightFunction(function(m) {
        m$v <- rep(1, length(m$v))
        m$Weighting <- c("binary", "bin")
        m
    }, "binary", "bin")
