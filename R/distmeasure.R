# Author: Ingo Feinerer

setGeneric("dissimilarity", function(x, y = NULL, method) standardGeneric("dissimilarity"))
setMethod("dissimilarity",
          signature(x = "TermDocMatrix", y = "ANY", method = "character"),
          function(x, y = NULL, method) {
              # Until factored out in a seperate package
              # use the \code{dists} function from the \pkg{cba} package
              dists(as(Data(x), "matrix"), y, method)
          })
setMethod("dissimilarity",
          signature(x = "TextDocument", y = "TextDocument", method = "character"),
          function(x, y = NULL, method) {
              tdm <- TermDocMatrix(c(x, y))
              dissim <- dissimilarity(tdm, method = method)
              return(dissim)
          })
