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
        m <- m * log2(nDocs(m) / rowSums(m > 0))
        if (isDTM) t(m) else m
    }, "term frequency - inverse document frequency", "tf-idf")

weightBin <-
    WeightFunction(function(m) {
        m$v <- rep(1, length(m$v))
        m
    }, "binary", "bin")
