\name{FunctionGenerator}
\docType{methods}
\alias{FunctionGenerator}
\title{Function Generator}
\description{
  Construct a function generator.
}
\usage{
FunctionGenerator(x)
}
\arguments{
  \item{x}{A generator function which takes some input
    and constructs and returns a new function based on
    that input information.}
}
\value{
  An object of class \code{FunctionGenerator} which extends the class
  \code{function} representing a function generator.
}
\seealso{
  Most reader functions (use \code{\link{getReaders}} to list available
  readers) are function generators.
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
