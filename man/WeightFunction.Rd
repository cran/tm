\name{WeightFunction}
\alias{WeightFunction}
\title{Weighting Function}
\description{
  Construct a weighting function for term-document matrices.
}
\usage{
WeightFunction(x, name, acronym)
}
\arguments{
  \item{x}{A function which takes a \code{\link{TermDocumentMatrix}}
    with term frequencies as input, weights the elements, and returns
    the weighted matrix.}
  \item{name}{A character naming the weighting function.}
  \item{acronym}{A character giving an acronym for the name of the
    weighting function.}
}
\value{
  An object of class \code{WeightFunction} which extends the class
  \code{function} representing a weighting function.
}
\examples{
weightCutBin <- WeightFunction(function(m, cutoff) m > cutoff,
                               "binary with cutoff", "bincut")
}
\author{Ingo Feinerer}
