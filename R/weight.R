# Author: Ingo Feinerer

# Weight Function Class
setClass("WeightFunction",
         representation(Name = "character",
                        Acronym = "character"),
         contains = "function")

# Constructor
setGeneric("WeightFunction", function(object, name, acronym) standardGeneric("WeightFunction"))
setMethod("WeightFunction",
          signature(object = "function", name = "character", acronym = "character"),
          function(object, name, acronym) {
              new("WeightFunction", .Data = object, Name = name, Acronym = acronym)
          })

# Actual TermDocumentMatrix weighting functions
weightTf <- WeightFunction(identity, "term frequency", "tf")

weightTfIdf <-
    WeightFunction(function(m) {
        isDTM <- inherits(m, "DocumentTermMatrix")
        if (isDTM) m <- t(m)
        m <- .tfidf(m)
        if (isDTM) t(m) else m
    }, "term frequency - inverse document frequency", "tf-idf")

weightBin <-
    WeightFunction(function(m) {
        m$v <- rep(1, length(m$v))
        m
    }, "binary", "bin")

.weight_rows_of_simple_triplet_matrix <-
function(x, w)
{
    x$v <- x$v * w[x$i]
    x
}

.row_sums_of_simple_triplet_matrix <-
function(x)
{
    ## Could add na.rm argument ...
    y <- double(x$nrow)
    sums <- tapply(x$v, x$i, sum)
    y[as.numeric(names(sums))] <- sums
    y
}

.tfidf <-
function(m)
{
    ## m a simple_triplet_matrix.
    ## Currently we cannot do m > 0, hence:
    m$v <- m$v > 0
    s <- .row_sums_of_simple_triplet_matrix(m)
    .weight_rows_of_simple_triplet_matrix(m, log2(m$ncol / s))
}
