\name{WeightFunction}
\docType{methods}
\alias{WeightFunction}
\alias{WeightFunction,function,character,character-method}
\title{Weighting Function Constructor}
\description{
  Constructs a weighting function for term-document matrices.
}
\usage{
WeightFunction(object, name, acronym)
}
\arguments{
  \item{object}{A function which takes a \code{dgCMatrix} term-document
    matrix with term frequencies as input, weights the elements, and
    returns the weighted matrix.}
  \item{name}{A character naming the weighting function.}
  \item{acronym}{A character giving an acronym for the name of the
    weighting function.}
}
\value{
  An S4 object of class \code{WeightFunction} which extends the class
  \code{function} representing a weighting function.
}
\examples{
weightCutBin <- WeightFunction(function(m, cutoff) as(m > cutoff, "dgCMatrix"), "binary with cutoff", "bincut")
}
\author{Ingo Feinerer}
\keyword{methods}
