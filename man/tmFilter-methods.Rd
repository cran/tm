\name{tmFilter-methods}
\docType{methods}
\alias{tmFilter}
\alias{tmFilter-methods}
\alias{tmFilter,Corpus-method}
\title{Methods for Function tmFilter in Package `tm'}
\description{
Methods for function \code{tmFilter} in package \pkg{tm}.
}
\section{Methods}{
  \describe{
    \item{object = "Corpus", FUN = searchFullText, doclevel = TRUE}{If
      \code{doclevel} is set, the method applies \code{FUN} onto each element
      of \code{object} and returns the filtered \code{object}; otherwise
      \code{FUN} is applied to \code{object} itself. If \code{FUN} has
      an attribute \code{doclevel} its value will be automatically used.
      Note that meta data (\code{\link{DMetaData}}) is automatically passed to
      \code{FUN}. The function \code{FUN} must return a logical value.}
  }
}
\seealso{
  \code{\link{sFilter}}
  \code{\link{tmIndex}}
  \code{\link{getFilters}}
}
\examples{
data("crude")
attr(searchFullText, "doclevel")
tmFilter(crude, FUN = searchFullText, "company")
}
\keyword{methods}
