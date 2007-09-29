\name{FunctionGenerator}
\docType{methods}
\alias{FunctionGenerator}
\alias{FunctionGenerator,function-method}
\title{Function Generator Constructor}
\description{
  Constructs a function generator object.
}
\usage{
\S4method{FunctionGenerator}{function}(object)
}
\arguments{
  \item{object}{a generator function which takes some input
    and constructs and returns a new function based on
    that input information.}
}
\value{
  An S4 object of class \code{FunctionGenerator} which extends the class
  \code{function} representing a function generator.
}
\seealso{
  Many reader functions are function generators, e.g.,
  \code{\link{readPlain}}.
}
\examples{
funGen <- FunctionGenerator(function(y, ...) {
if (is(y, "integer")) function(x) x+1 else function(x) x-1
})
funGen
funGen(3L)
funGen("a")
}
\author{Ingo Feinerer}
\keyword{methods}
