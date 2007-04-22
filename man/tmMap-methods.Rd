\name{tmMap-methods}
\docType{methods}
\alias{tmMap}
\alias{tmMap-methods}
\alias{tmMap,TextDocCol,function-method}
\title{Methods for Function tmMap in Package `tm'}
\description{
Methods for function \code{tmMap} in package \pkg{tm}.
}
\section{Methods}{
  \describe{
    \item{object = "TextDocCol", FUN}{Applies \code{FUN} onto each element
      of \code{object}. Note that document specific metadata (i.e.,
      \code{DMetaData(object)} is automatically passed to
      \code{FUN} as argument \code{DMetaData}. \code{FUN} must be a
      function which returns a \code{TextDocument}.}
  }
}
\examples{
data("crude")
tmMap(crude, stemDoc)
}
\keyword{methods}
