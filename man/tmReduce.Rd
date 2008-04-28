\name{tmReduce}
\alias{tmReduce}
\title{Combine Transformations}
\description{
Combine multiple transformations (mappings) into a single one.
}
\usage{
tmReduce(x, tmFuns, ...)
}
\arguments{
  \item{x}{a corpus.}
  \item{tmFuns}{a \code{list} of \pkg{tm} transformations.}
  \item{\dots}{arguments to the individual transformations.}
}
\value{
  A single \pkg{tm} transformation function.
}
\author{Ingo Feinerer}
\seealso{
  Use \code{\link{getTransformations}} to list available transformation (mapping) functions.
}
\examples{
data(crude)
crude[[1]]
skipWords <- function(x, ...) removeWords(x, c("it", "the"))
funs <- list(tmTolower, removePunctuation, skipWords, stripWhitespace)
tmMap(crude, FUN = tmReduce, tmFuns = funs)[[1]]
}
