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

# Actual TermDocMatrix weighting functions
weightTf <- WeightFunction(function(m) m, "term frequency", "tf")
weightTfIdf <- WeightFunction(function(m) t(t(m) * log2(nrow(m) / colSums(as(m > 0, "dgCMatrix")))), "term frequency - inverse document frequency", "tf-idf")
weightBin <- WeightFunction(function(m) as(m > 0, "dgCMatrix"), "binary", "bin")
weightLogical <- WeightFunction(function(m) m > 0, "logical", "logic")
