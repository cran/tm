\name{tmMap-methods}
\docType{methods}
\alias{tmMap}
\alias{tmMap-methods}
\alias{tmMap,Corpus,function-method}
\title{Methods for Function tmMap in Package `tm'}
\description{
Methods for function \code{tmMap} in package \pkg{tm}.
}
\section{Methods}{
  \describe{
    \item{object = "Corpus", FUN}{Applies \code{FUN} onto each element
      of \code{object}. Note that document specific metadata (i.e.,
      \code{DMetaData(object)} is automatically passed to
      \code{FUN} as argument \code{DMetaData}. \code{FUN} must be a
      function which returns a \code{TextDocument}.}
  }
}
\seealso{
  See \code{\link{getTransformations}} for available transformations
  shipped with \pkg{tm}.
}
\examples{
data("crude")
tmMap(crude, stemDoc)

headings <- function(object, ...) {
new("PlainTextDocument", Heading(object), Cached = TRUE, DateTimeStamp =
Sys.time(), ID = ID(object), Language = Language(object))
}
inspect(tmMap(crude, headings))
}
\keyword{methods}
