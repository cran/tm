\name{removeNumbers-methods}
\docType{methods}
\alias{removeNumbers}
\alias{removeNumbers-methods}
\alias{removeNumbers,PlainTextDocument-method}
\title{Methods for Function removeNumbers in Package `tm'}
\description{
Methods for function \code{removeNumbers} in package \pkg{tm}.
}
\section{Methods}{
  \describe{
    \item{object = "PlainTextDocument", ...}{Strips any numbers from
      \code{object} and return the object.}
  }
}
\seealso{
  Use \code{\link{getTransformations}} to list available transformation (mapping) functions.
}
\examples{
data("crude")
crude[[1]]
removeNumbers(crude[[1]])
}
\keyword{methods}
