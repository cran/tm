\name{tmIntersect-methods}
\docType{methods}
\alias{tmIntersect}
\alias{tmIntersect-methods}
\alias{tmIntersect,PlainTextDocument,character-method}
\title{Methods for Function tmIntersect in Package `tm'}
\description{
  Methods for function \code{tmIntersect} in package \pkg{tm}.
}
\section{Methods}{
  \describe{
    \item{object = "PlainTextDocument", words = "character"}{Return
      \code{TRUE} if \code{words} appear in \code{object}'s text.}
  }
}
\seealso{
  Use \code{\link{getFilters}} to list available filter functions.
}
\examples{
data("crude")
tmIntersect(crude[[1]], c("crude", "oil"))
tmIntersect(crude[[1]], "acquisition")
}
\keyword{methods}
