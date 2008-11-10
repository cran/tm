\name{tmIndex-methods}
\docType{methods}
\alias{tmIndex}
\alias{tmIndex-methods}
\alias{tmIndex,Corpus-method}
\title{Methods for Function tmIndex in Package `tm'}
\description{
Methods for function \code{tmIndex} in package \pkg{tm}.
}
\section{Methods}{
  \describe{
    \item{object = "Corpus", FUN = searchFullText, doclevel = TRUE}{If
      \code{doclevel} is set, the method applies \code{FUN} onto each element
      of \code{object} and returns the indices; otherwise
      \code{FUN} is applied to \code{object} itself. If \code{FUN} has
      an attribute \code{doclevel} its value will be automatically used.
      Note that meta data (\code{\link{DMetaData}}) is automatically passed to
      \code{FUN}. The function \code{FUN} must return a Boolean value.}
  }
}
\seealso{
  \code{\link{sFilter}}
  \code{\link{tmFilter}}
  \code{\link{getFilters}}
}
\examples{
data("crude")
attr(searchFullText, "doclevel")
tmIndex(crude, FUN = searchFullText, "company")
}
\keyword{methods}
